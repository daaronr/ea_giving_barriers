# Functions used in general Reinstein code


#### 'Hijacking' standard functions ####

### See - https://www.r-bloggers.com/hijacking-r-functions-changing-default-arguments/

hijack <- function (FUN, ...) {
    .FUN <- FUN
    args <- list(...)
    invisible(lapply(seq_along(args), function(i) {
        formals(.FUN)[[names(args)[i]]] <<- args[[i]]
    }))
    .FUN
}

# usage: .data.frame <- hijack(data.frame, stringsAsFactors = FALSE)

#e.g, .read_csv <- hijack(read_csv, trim_ws = TRUE)

### ... some quick hijacks, esp for na/rm

#why didn't this work? .mean <- hijack(base::mean, na.rm = TRUE)

mn <- function(x) {
    base::mean(x, na.rm=TRUE)
}

med <- function(x) {
    stats::median(x, na.rm=TRUE)
}

impute.med <- function(x) base::replace(x, is.na(x), median(x, na.rm = TRUE))

sdev <- function(x) {
    stats::sd(x, na.rm=TRUE)
}



.median <- hijack(stats::median, na.rm = TRUE)
.sd <- hijack(stats::sd, na.rm = TRUE)




############## STATISTICAL TESTS and  test functions ####

#### .... some missing functions? ###

#std error of binary (or logical) variable
se_bin <- function(x) {
  x = as.numeric(x)
  n = length(x)
  m = mean(x, na.rm=TRUE)
  se = sqrt((m*(1-m))/n)
  return(se)
}


# Calculate mean, n and sd per group
summ_add <- function(df, var) {
    df %>%
    mutate(
    mean = mean(as.numeric( {{ var }} )),
    n = n(),
    sd = sd(as.numeric(d_career_etg)),
    se = sqrt((m*(1-m))/n)
    )
}

# Generic test function: a helper function
doTest <- function(pair, df = ADSX, stage = 2, depvar = donation, treatvar = Treatment, testname = "t.test2") {
  require("dplyr")
  depvar <- enquo(depvar)
  treatvar <- enquo(treatvar)
  thetest <- match.fun(testname)
  ADSplit <- df %>%
      ungroup() %>%
      dplyr::filter(!is.na(!!depvar) & Stage == stage) %>%
      split(pull(., !!treatvar)) %>%
      map(~dplyr::select(., !!depvar)) %>%
      extract(pair)
  TR <- thetest(pull(ADSplit[[1]], !!depvar), pull(ADSplit[[2]], !!depvar)) %>%
      broom::tidy() %>%
      mutate(TreatRow = pair[1],
             TreatCol = pair[2])
}

# adds `dplyr::filter(is.na(depvar))` to the above
doTest_noatr <- function(pair, df = ADSX, stage = 2, depvar = donation,
  treatvar = Treatment, testname = "t.test2") {
  require("dplyr")
  depvar <- enquo(depvar)
  treatvar <- enquo(treatvar)
  thetest <- match.fun(testname)
  ADSplit <- df %>% ungroup() %>%
      dplyr::filter(!is.na(!!depvar) &
    Stage == stage) %>%
  dplyr::filter(is.na(Attrited)) %>%
    split(pull(., !!treatvar)) %>%
    map(~dplyr::select(., !!depvar)) %>%
  extract(pair)
TR <- thetest(pull(ADSplit[[1]], !!depvar), pull(ADSplit[[2]], !!depvar)) %>%
  broom::tidy() %>%
      mutate(TreatRow = pair[1],
    TreatCol = pair[2])
}

#### Fisher's exact test ####

## With numbers

fisherme <- function(g1, g1pos, g2, g2pos) {
  mat <- matrix(c(g1-g1pos, g1pos, g2-g2pos, g2pos),
       nrow = 2,
       dimnames = list(control = c("no", "yes"), treat = c("no", "yes")))
  fisher.test(mat, or=1, alternative="t", conf.int=TRUE)
}

## From data
fisher <- function(A, B) {
  m <- matrix(c(sum(A), length(A) - sum(A), sum(B), length(B) -
    sum(B)), byrow = T, nrow = 2)
  fisher.test(m)
}

doFisher <- function(pair, df = ADSX, stage = 2, depvar = d_donation,
  treatvar = Treatment) {
  require("dplyr")
  depvar <- enquo(depvar)
  treatvar <- enquo(treatvar)
  ADSplit <- df %>% ungroup() %>% filter(!is.na(depvar) & Stage ==
    stage) %>% split(pull(., !!treatvar)) %>% map(~dplyr::select(.,
    depvar)) %>% extract(pair)
  TR <- fisher(pull(ADSplit[[1]], ), pull(ADSplit[[2]], depvar)) %>%
    broom::tidy() %>% mutate(TreatRow = pair[1], TreatCol = pair[2])
}


fisher.bintest <- function(formula, data, alpha = 0.05, p.method = "fdr") {
  if (missing(formula) || (length(formula) != 3)) {
    stop("missing or incorrect formula")
  }
  m <- match.call()
  if (is.matrix(eval(m$data, parent.frame()))) {
    m$data <- as.data.frame(m$data)
  }
  m[[1]] <- as.name("model.frame")
  m$alpha <- m$p.method <- NULL
  mf <- eval(m, parent.frame())
  mf <- droplevels(mf[complete.cases(mf), ])
  dname <- paste(names(mf)[1], paste(names(mf)[2:ncol(mf)],
    collapse = ":"), sep = " by ")
  resp.mf <- mf[, 1]
  resp <- factor(as.numeric(factor(resp.mf)) - 1)
  if (nlevels(resp) != 2) {
    stop(paste(names(mf)[1], "is not a binary variable"))
  }
  resp.num <- as.numeric(as.character(resp))
  fact <- interaction(mf[, 2:ncol(mf)], sep = ":")
  proba <- tapply(resp.num, fact, mean)
  names(proba) <- paste("proba in group ", levels(fact), sep = "")
  tab.cont <- table(fact, relevel(resp, ref = "1"))
  nval <- 0
  names(nval) <- "difference in probabilities"
  result <- list(data.name = dname, alternative = "two.sided",
    null.value = nval, estimate = proba, alpha = alpha)
  test <- fisher.test(tab.cont)
  result$p.value <- test$p.value
  result$method.test <- "Fisher's Exact Test for Count Data"
  if (test$p.value < alpha & nlevels(fact) > 2) {
    result$p.adjust.method <- p.method
    result$p.value.multcomp <- fisher.multcomp(tab.cont)$p.value
    result$method.multcomp <- "Fisher's exact tests for count data"
  }
  class(result) <- "RVtest"
  return(test)
}

#### # Lift tests t-test ####
t.test2 <- function(x, y) t.test(x, y)
liftedTT <- purrr::lift(t.test2, .unnamed = TRUE)



# Wilcoxon rank-sum test for continuous outcome variables.
# wilcoxon(subd[subd$Shares=='High', ]$EscThreshold,
# subd[subd$Shares=='Equal',]$EscThreshold) Also
# 'randomization statistical inference'? (Mosaic package or
# 'ri' package?)
wilcox.test2 <- function(x, y) wilcox.test(x, y, exact = FALSE)
liftedWilcox <- purrr::lift(wilcox.test2, .unnamed = TRUE)

# Bayesian test coding ####

#(Coded By Scott Dickerson)

bayesian_test_me <- function(g1, g1pos, g2, g2pos,i,j,a,b){
  #Function which takes the same inputs as the fishertestme function, turns them into a matrix
  #for a simple bayesian test, comparing the difference in proportions. Follows on from the work
  #by Andrew Gelman and Bob Carpenter (references below). This function returns the output to a list.
  #This list contains three elements: the first element is a vector containing the 1,000,000 elements
  #of the difference in the two posterior samples - we can call this delta (i.e. the theta2 vector-theta1 vector);
  #element two is a scalar containing the probability that theta2 > theta1; and element three are the credible
  #intervals for the value of delta.
  mat <- matrix(c(g1-g1pos, g1pos, g2-g2pos, g2pos),
                nrow = 2,
                dimnames = list(control = c("no", "yes"), treat = c("no", "yes")))

  X              <- t(apply(mat, 2, rev)) #Gets the matrix into the right format as required

  y1             <- X[1,1] #Selects the elements
  y2             <- X[2,1]
  n1             <- X[1,1]+X[1,2]
  n2             <- X[2,1]+X[2,2]
  nsim           <- 1000000 #Number of samples

  set.seed(1) #CHANGE BEFORE PUBLICATION
  theta1         <- rbeta(nsim,y1+a,(n1-y1)+b) #Exact posterior densities for our random quantities
  set.seed(1) #CHANGE BEFORE PUBLICATION
  theta2         <- rbeta(nsim,y2+a,(n2-y2)+b)

  #Note this is flipped - looking at y2 (lower left cell, i.e. treatment)
  difference     <- theta2-theta1

  #Monte Carlo approximation for the probability that theta2 is greater than theta1
  prob           <- mean(theta2>theta1)
  prob2          <- prob*100

  #Credible intervals for the difference
  quantiles      <- quantile(difference,c(0.005,0.025,0.05,0.1,0.9,0.95,0.975,0.995))
  quantiles_99LB <- quantiles[1]
  quantiles_99UB <- quantiles[8]
  quantiles_95LB <- quantiles[2]
  quantiles_95UB <- quantiles[7]
  quantiles_90LB <- quantiles[3]
  quantiles_90UB <- quantiles[6]
  quantiles_80LB <- quantiles[4]
  quantiles_80UB <- quantiles[5]

  output         <- list(prob2, quantiles_99LB, quantiles_99UB, quantiles_95LB, quantiles_95UB,
                         quantiles_90LB, quantiles_90UB, quantiles_80LB,quantiles_80UB, difference)
  listnames      <- c("Probability","99LB","99UB","95LB","95UB","90LB","90UB","80LB","80UB","Differences")
  names(output)  <- listnames
  output
}



#### Data work ########

###... Function to filter by given string: ####

filter_parse <- function(df, x) {
 {{df}} %>%
   filter(rlang::eval_tidy(rlang::parse_expr({{x}})))
}

# compare column types across two df (e.g., in advance of a
# merge); from

# https://stackoverflow.com/questions/45743991/r-compare-column-types-between-two-dataframes
compareColumns <- function(df1, df2) {
  commonNames <- names(df1)[names(df1) %in% names(df2)]
  data.frame(Column = commonNames, df1 = sapply(df1[, commonNames],
    class), df2 = sapply(df2[, commonNames], class))
}


#### # Join and update, take nonmissing values from - ####
# https://alistaire.rbind.io/blog/coalescing-joins/

coalesce_join <- function(x, y, by = NULL, suffix = c(".x", ".y"),
  join = dplyr::full_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))

  to_coalesce <- names(joined)[!names(joined) %in% cols]
      if(length(to_coalesce) >0){

  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]),
    1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(to_coalesce, 1, nchar(to_coalesce) -
    nchar(suffix_used)))

  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(joined[[paste0(.x,
    suffix[1])]], joined[[paste0(.x, suffix[2])]]))
  names(coalesced) <- to_coalesce

    joined <- dplyr::bind_cols(joined, coalesced)[cols]
  return(joined)} else{return(joined)}
}

# Adapting function suggested by JohnH on Slack, to merge
# tables with overlapping columns and reconcile them ...(will
# take nonmissing value if only one present, takes value from
# 'x' if otherwise conflicting) arguments: y and x are table
# names, by is shared key(s?) (in quotes)
merge_cols <- function(x, y, by) {

  # Find column names in y that are also in x.
  shared_columns <- intersect(colnames(y), colnames(x))
  duplicated_columns <- setdiff(shared_columns, by)  # as above, removing shared key(s)

  # JohnH didn't want any extra columns from y; but we do!.
  # y_select <- dplyr::select(y, shared_columns) We don't want
  # NAs in the by columns of y, to avoid ballooning NA joins.
  y_select_filter <- dplyr::select(y, by)
  y_select_filter <- y_select_filter %>% dplyr::filter_all(dplyr::all_vars(!is.na(.)))  #doesn't seem to change anything for us
  # y_select <- y_select %>%
  y <- y %>% dplyr::inner_join(y_select_filter)

  # JohnH: Join x to the shared columns in y.  joined_tibbles
  # <- dplyr::left_join(x, y_select, by) %>% dplyr::distinct()
  # US: Join x to ALL columns in y.
  joined_tibbles <- dplyr::left_join(x, y, by) %>% dplyr::distinct()  # DR: 'distinct' is superfluous in our case

  # For each duplicated column, find the 'best' results
  # (replace NAs in x with values in y).  US: ... AND
  # vice/versa (if possible)
  best_columns <- purrr::map_dfc(duplicated_columns, function(column_name) {
    #-- applies this function to each duplicated column
    internal_return <- dplyr::tibble(col1 = dplyr::coalesce(joined_tibbles[[paste0(column_name,
      ".x")]], joined_tibbles[[paste0(column_name, ".y")]]))
    #-- for each duplicated column, for this column name, coalesce the '.x' and '.y' versions
    colnames(internal_return) <- column_name  # rename column back to original
    internal_return
  })

  # Bind the tibbles back together, and get rid of the interim
  # columns.
  dplyr::bind_cols(joined_tibbles, best_columns) %>% # dplyr::select(colnames(x)) %>% JohnH selects x columns
  # only; I think we want both
  dplyr::select(union(colnames(x), colnames(y))) %>% dplyr::distinct()
}


#### TODO function to Coalesce all .x and .y columns, taking .x as default, removing extensions

#### Just keep .x after a join ####
just_x  <- function(df) {
    {{df}} %>%
        select(!matches("\\.y$")) %>%
               select_all(~gsub("\\.x$", "", .))
}


#### Simple recoding assistants ####

zero_to_missing <- function(x){
  x[x == 0] <- NA
  return(x)
}

missing_to_zero <- function (v)
{
  v[is.na(v) == TRUE] <- 0
  return(v)
}




#### Labelling values and variables ####

# Rename a variable with it's label

rename_to_var_label <- function(df){
  # Extract variable label
  labels <- lapply(df, function(x) attributes(x)$label)
  assertthat::assert_that(!list(NULL) %in% labels,
                          msg = "Each column must have a corresponding label")
  # Set names of variables as their label
  names(df) <- unlist(labels)
  return(df)
}


#### Basic setup and codebooks ####

rdr_cbk <- function(cbfile) {
  #Convenience function to make codebooks with options
  rmarkdown::render(
    here("codebooks", cbfile),
    output_dir = here("docs","codebooks"),
    intermediates_dir = here("docs","codebooks"),
    knit_root_dir = here("docs", "codebooks")
  )
}


####  SUMMARY tables function(s) ####

#intended for donation data:
.summ <- hijack(vtable::sumtable,
                summ=c('notNA(x)', 'sum(x != 0)', 'mean(x)', 'sd(x)', 'pctile(x)[50]', 'pctile(x)[90]'),
                summ.names = c('N Responses', 'N positive', 'Mean', 'Sd', "Median", "90th pct"),
                digits=1,
                labels = TRUE, #uses assigned in Hmisc or sjlabelled
                simple.kable = TRUE)


.summk <- hijack(vtable::sumtable,
                summ=c('notNA(x)', 'sum(x != 0)', 'mean(x)', 'sd(x)', 'pctile(x)[50]', 'pctile(x)[90]'),
                summ.names = c('N Responses', 'N positive', 'Mean', 'Sd', "Median", "90th pct"),
                digits=1,
                labels = TRUE, #uses assigned in Hmisc or sjlabelled
                simple.kable = TRUE,
                out="kable")


#### ... Sumtabs by 'treatment' ... from substitution project

sumtab_func_full <- function(df = ADSX, depvar = donation, treatvar = TreatFirstAsk,
  caption = "") {
  df %>%
    ungroup() %>%
    filter(!is.na({{depvar}})) %>%
    group_by({{treatvar}}) %>%
    dplyr::summarize(N = n(),
                     `share > 0` = sum({{depvar}} >0)/n(),
                     share_10 = sum({{depvar}}== 10)/n(),
                     Mean = round(mean({{depvar}}, na.rm = T), 2),
                     Median = round(median({{depvar}}, na.rm = T),2),
                     P80 = round(quantile({{depvar}}, 0.8, na.rm = T), 2),
                     Std.dev. = glue::glue("(", {round(sd({{depvar}}, na.rm = T), 2) }, ")")) %>%
    kable(caption = caption) %>% kable_styling()
}

sumtab <- function(df = ADSX, depvar = donation, treatvar = TreatFirstAsk,
                             caption = "", digits=3, label = TRUE) {
  df %>%
    ungroup() %>%
    filter(!is.na({{depvar}})) %>%
    group_by({{treatvar}}) %>%
    dplyr::summarize(N = n(),
                     `share > 0` = sum({{depvar}} >0)/n(),
                     #share_10 = sum({{depvar}}== 10)/n(),
                     Mean = round(mean({{depvar}}, na.rm = T), 2),
                     Median = round(median({{depvar}}, na.rm = T),2),
                     P80 = round(quantile({{depvar}}, 0.8, na.rm = T), 2),
                    # P99 = round(quantile({{depvar}}, 0.99, na.rm = T), 2),
                     Std.dev. = glue::glue("(", {round(sd({{depvar}}, na.rm = T), 2) }, ")")) %>%
    kable(caption = caption, digits=digits, label = label) %>%
    kable_styling()
}

sumtab_func <- function(df = ADSX, depvar = donation, treatvar = TreatFirstAsk,
  caption = "") {
  treatvar <- enquo(treatvar)
  depvar <- enquo(depvar)
  df %>% ungroup() %>% filter(!is.na(!!depvar)) %>% group_by(!!treatvar) %>%
    dplyr::summarize(N = n(), Mean = round(mean(!!depvar,
      na.rm = T), 2), Std.dev. = glue("(", {
      round(sd(!!depvar, na.rm = T), 2)
    }, ")")) %>% kable(caption = caption)
}

sumtab2_func <- function(df = ADSX, depvar = donation, treatvar = TreatFirstAsk,
  treatvar2 = Stage, caption = "", col.names = "") {
  treatvar <- enquo(treatvar)
  treatvar2 <- enquo(treatvar2)
  depvar <- enquo(depvar)
  df %>% ungroup() %>% filter(!is.na(!!depvar)) %>% group_by(!!treatvar,
    !!treatvar2) %>% dplyr::summarize(N = n(), Mean = round(mean(!!depvar,
    na.rm = T), 2), Std.dev. = glue("(", {
    round(sd(!!depvar, na.rm = T), 2)
  }, ")")) %>% unite(Results, Mean, "Std.dev.", N, sep = " ") %>%
    spread(!!treatvar2, Results) %>% kable(caption = caption,
    escape = F) %>% kable_styling("striped", full_width = F)
}


sumtab2_func_plus <- function(df = ADSX, depvar = donation, treatvar = TreatFirstAsk,
  treatvar2 = Stage, caption = "pos %| mean, med--p75, (sd), [N]",
  col.names = "") {
  treatvar <- enquo(treatvar)
  treatvar2 <- enquo(treatvar2)
  depvar <- enquo(depvar)
  df %>% ungroup() %>% filter(!is.na(!!depvar)) %>% group_by(!!treatvar,
    !!treatvar2) %>% dplyr::summarize(gt0 = glue(" ", {
    round(mean(!!depvar > 0, na.rm = T) * 100, 0)
  }, "% | "), mean = glue(" ", {
    round(mean(!!depvar, na.rm = T), 1)
  }, ", "), med = glue({
    round(median(!!depvar, na.rm = T), 1)
  }, "--"), p75 = round(quantile(!!depvar, 0.75, na.rm = T),
    1), Std.dev. = glue(" | (", {
    round(sd(!!depvar, na.rm = T), 1)
  }, ")"), N = glue(" [", {
    n()
  }, "]")) %>% unite(Results, gt0, mean, med, p75, "Std.dev.",
    N, sep = "") %>% spread(!!treatvar2, Results) %>% knitr::kable(caption = caption,
    escape = F) %>% kable_styling("striped", full_width = F)
}



# filter(!is.na(donation)) %>% group_by(Treatment, Stage) %>%
# dplyr::ummarize(N = n(), Mean = round(mean(donation, na.rm
# = T), 2), 'Std.dev.' = glue('(', { round(sd(donation, na.rm
# = T), 2) }, ')')

# ) %>% unite(Results, Mean, 'Std.dev.', N, sep = ' ') %>%
# spread(Stage, Results) %>% rename('Stage 1' = `1`, 'Stage
# 2' = `2` )%>% separate('Stage 1', c('Mean', 'Std.dev.',
# 'N'), sep = ' ') %>%


#### simple tables and tabsum ####

# tabg: tabyl one way plus sort by descening frequency -- the version we normally want
tabg <- function(df, col) {
    janitor::tabyl({{df}},{{col}}) %>%
        arrange(-`n`)
}

# ... formatting default options for tabyl ####
tabylstuff <- function(df, cap = "") {
  adorn_totals(df, c("row", "col")) %>% adorn_percentages("row") %>%
    adorn_pct_formatting(digits = 1) %>% adorn_ns() %>% kable(caption = cap) %>%
    kable_styling(latex_options = "scale_down")
}

tabylstuff_nocol <- function(df,cap=""){
  adorn_totals(df,c("row")) %>%
    adorn_percentages("row") %>%
    adorn_pct_formatting(digits = 1) %>%
    adorn_ns() %>%
    kable(caption=cap) %>%
    kable_styling(latex_options = "scale_down")
}

tabyl_ow_plus <- function(df, var, caption=NULL, title_case = FALSE) {
  df <- {{df}} %>%
    tabyl({{var}}) %>%
    dplyr::arrange(desc(n)) %>%
    adorn_totals()

  if (title_case == TRUE){
    df <- df %>% rename_with(snakecase::to_title_case)
  }
  df %>%
    kable(caption = caption, padding=0) %>%
    kable_styling()
}

tabylme <- function(df = ADSX, rowvar = TreatFirstAsk, colvar = treat_second_ask,
  adorn = "row") {
    {{df}} %>%
  tabyl({{rowvar}}, {{colvar}}) %>% adorn_percentages({{adorn}}) %>%
    adorn_pct_formatting(digits = 2) %>% adorn_ns() %>% kable() %>%
    kable_styling()
}

adornme <- function(atabyl, adorn = "row", digits = 2, cap = "",
                    title = "") {
  atabyl %>% adorn_totals("row") %>% # adorn_totals(c('row', 'col')) %>%
    adorn_percentages(adorn) %>% adorn_pct_formatting(digits = digits) %>%
    adorn_ns() %>% adorn_title(title, placement = "top") %>%
    kable(caption = cap) %>% kable_styling()
}

adornme_not <- function(atabyl, adorn = "row", digits = 2, cap = "",
                    title = "") {
  atabyl %>% adorn_totals("row") %>% # adorn_totals(c('row', 'col')) %>%
    adorn_percentages(adorn) %>% adorn_pct_formatting(digits = digits) %>%
    adorn_ns() %>%
    kable(caption = cap) %>% kable_styling()
}
tabylme <- function(df = ADSX, rowvar = TreatFirstAsk, colvar = treat_second_ask,
  adorn = "row") {
    {{df}} %>%
  tabyl({{rowvar}}, {{colvar}}) %>% adorn_percentages({{adorn}}) %>%
    adorn_pct_formatting(digits = 2) %>% adorn_ns() %>% kable() %>%
    kable_styling()
}

adornme <- function(atabyl, adorn = "row", digits = 2, cap = "",
                    title = "") {
  atabyl %>% adorn_totals("row") %>% # adorn_totals(c('row', 'col')) %>%
    adorn_percentages(adorn) %>% adorn_pct_formatting(digits = digits) %>%
    adorn_ns() %>% adorn_title(title, placement = "top") %>%
    kable(caption = cap) %>% kable_styling()
}

adornme_not <- function(atabyl, adorn = "row", digits = 2, cap = "",
                    title = "") {
  atabyl %>% adorn_totals("row") %>% # adorn_totals(c('row', 'col')) %>%
    adorn_percentages(adorn) %>% adorn_pct_formatting(digits = digits) %>%
    adorn_ns() %>%
    kable(caption = cap) %>% kable_styling()
}

tabsum <- function(df = ADSX, yvar = donation, xvar = Stage, treatvar = Treatment) {
  yvar <- enquo(yvar)
  xvar <- enquo(xvar)
  treatvar <- enquo(treatvar)
  df %>% ungroup() %>% # mutate(xvar = as.factor(!!xvar)) %>%
  dplyr::group_by(!!xvar, !!treatvar) %>% # drop_na(!!yvar, !!treatvar) %>%
  dplyr::select(!!yvar, !!treatvar, !!xvar) %>% dplyr::summarise(meanyvar = mean(!!yvar,
    na.rm = TRUE))
}


#shortcut -- summarize data by group
summarise_by <- function(data, ..., by) {
  data %>%
    group_by({{ by }}) %>%
    summarise(...)
}


summ_by <- function(data, groupvar, ...) {
    data %>%
group_by({{ groupvar }}) %>%
    summarise(across(everything(), list({{ ... }})))
}

# MACHINE learning related functions ###

get_var_importance <- function(fit){
  extracted <- workflowsets::extract_fit_parsnip(fit)

  vip::vi(extracted)
}

# For scaling variable importance
scale_var <- function(x){
  scale(x)[,1]
}


# VISUALISATION functions: ####

#TODO -- add ggrepel to these? geom_label_repel() and geom_text_repel

plot_histogram <- function(df, feature) {
  chart_title <- substitute(paste("Histogram of ", feature,
    sep = ""))
  plt <- ggplot(df, aes(x = eval(parse(text = feature)))) +
    geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#33AADE",
      color = "black") + geom_density(alpha = 0.3, fill = "red") +
    geom_vline(aes(xintercept = mean(eval(parse(text = feature)))),
      color = "black", linetype = "dashed", size = 1) +
    labs(x = feature, y = "Density")
  print(plt)
}

# ... Multiple histogram: ####

plot_multi_histogram <- function(df, feature, label_column) {
  chart_title <- substitute(paste("Histograms of ", feature,
    " by ", label_column, sep = ""))
  plt <- ggplot(df, aes(x = eval(parse(text = feature)), fill = eval(parse(text = label_column)))) +
    geom_histogram(alpha = 0.3, position = "identity", aes(y = ..density..),
      color = "black") + geom_density(alpha = 0.3) + geom_vline(aes(xintercept = mean(eval(parse(text = feature)))),
    color = "black", linetype = "dashed", size = 1) + labs(title = chart_title,
    subtitle = "dashed line = mean", x = feature, y = "Density")
  plt + guides(fill = guide_legend(title = label_column))
}


dotplot_func <- function(df = ADSX, yvar = donation, xvar = Stage,
  treatvar = Treatment, title = "") {
  yvar <- enquo(yvar)
  xvar <- enquo(xvar)
  treatvar <- enquo(treatvar)
  df %>% ungroup() %>% # mutate(xvar = as.factor(!!xvar)) %>%
  dplyr::group_by(!!xvar, !!treatvar) %>% # drop_na(!!yvar, !!treatvar) %>%
  dplyr::select(!!yvar, !!treatvar, !!xvar) %>% dplyr::summarise(meanyvar = mean(!!yvar,
    na.rm = TRUE)) %>% ggplot(aes(y = meanyvar, x = !!xvar,
    color = !!treatvar, group = !!treatvar, shape = !!treatvar)) +
    geom_point(size = 6) + geom_line() + expand_limits(y = 0) +
    scale_x_continuous(breaks = c(1, 2)) + scale_y_continuous(yvar) +
    theme(panel.grid.major.y = element_line(color = "white",
      size = 0.3)) + theme(panel.grid.minor.y = element_line(color = "white",
    size = 0.1)) + theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.minor.x = element_blank()) + theme(axis.ticks.x = element_blank()) +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) +
    theme(legend.text = element_text(size = 12), legend.title = element_text(size = 15,
      face = "bold")) + ggtitle(title)
}

geom_mean <- function() {
  list(stat_summary(fun.y = "mean", geom = "point", fill = "red"),
    stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",
      width = 0.4))
}

boxplot_func <- function(df = ADSX, yvar = donation, treatvar = Treatment, facetfunc = 1,
  comparisons = list(c("No ask-Domestic", "Domestic-Domestic"))) {
  yv <- enquo(yvar)
  tv <- enquo(treatvar)
  ungroup(df) %>%
      group_by({{treatvar}}) %>%
      drop_na({{yvar}}, {{treatvar}}) %>%
    ggplot(aes({{treatvar}}, {{yvar}})) +
    geom_boxplot() +
    facet_grid({{facetfunc}}) +
    theme(axis.title = element_text(size = 14),
    axis.text = element_text(size = 14)) +
    theme(axis.text.x = element_text(size = 12)) +
    labs(title = tv, y = yv, caption = "p-values of Wilcox-(below) and  t-test (above brackets)") +
    geom_signif(comparisons = comparisons, step_increase = c(0.4),
      vjust = 1.7, margin_top = 0.5, textsize = 5) +
    geom_signif(comparisons = comparisons,
    step_increase = c(0.4), vjust = 0, margin_top = 0.5,
    textsize = 5, test = "t.test") +
     stat_summary(
    fun.y = mean, geom = "point", shape = 18,
      size = 3, color = "red")
}

# https://stackoverflow.com/questions/13407236/remove-a-layer-from-a-ggplot2-chart
# Delete layers from ggplot
remove_geom <- function(ggplot2_object, geom_type) {
  # Delete layers that match the requested type.
  layers <- lapply(ggplot2_object$layers, function(x) {
    if (class(x$geom)[1] == geom_type) {
      NULL
    } else {
      x
    }
  })
  # Delete the unwanted layers.
  layers <- layers[!sapply(layers, is.null)]
  ggplot2_object$layers <- layers
  ggplot2_object
}

# Model building #####

# @David: Repeating + renaming for clarity
make_formula <- function(lhs, rhs) {
    stats::reformulate(response = lhs, termlabels = rhs,
              env = globalenv()) # Formula is defined in the global environment
}


# Shorter but less clear version -- remove?
m_f <- function(lhs, rhs) {
  as.formula(paste(lhs," ~ ", paste(rhs, collapse= "+")))
}


make_formula_df <- function(outcome_vars, indep_vars, dfs){
  # Make all associated formulas
  formulas <- mapply(function(x, y) make_formula(x, y), x = outcome_vars, y = indep_vars)

  tibble(outcome = as.character(outcome_vars),
         formulas = formulas,
         dfs = dfs)
}


# Options and formatting code elements ####

sidebyside <- function(..., width = 60) {
  l <- list(...)
  p <- lapply(l, function(x) {
    xx <- capture.output(print(x, width = width))
    xx <- gsub("\"", "", xx)
    format(xx, justify = "left", width = width)
  })
  p <- do.call(cbind, p)
  sapply(seq_len(nrow(p)), function(x) paste(p[x, ], collapse = ""))
}

huxoptions <- function(df) {
  as_hux(df) %>% set_bold(everywhere, 1, TRUE) %>% # set_background_color(where(. < 0.1), 'grey') %>%
  set_all_borders(1) %>% huxtable::add_colnames() %>% set_number_format(3)
}

huxreg_opts  <- function(df) {
 df %>%
    set_bold(1, everywhere)             %>%
    set_bottom_border(1, everywhere) %>%
    map_background_color(by_rows("grey87", "white"))  %>%
    set_caption_pos("bottom") %>%
    set_col_width(c(1.8, rep(.6, times=length(.)-1)))
}

# Todo: make function to create our preferred types of
# summary statistics table Editing data functions FixNA and
# debug from:
# #https://stackoverflow.com/questions/44200195/how-to-debug-contrasts-can-be-applied-only-to-factors-with-2-or-more-levels-er

NA_preproc <- function(dat) {
  for (j in 1:ncol(dat)) {
    x <- dat[[j]]
    if (is.factor(x) && anyNA(x))
      dat[[j]] <- base::addNA(x)
    if (is.character(x))
      dat[[j]] <- factor(x, exclude = NULL)
  }
  dat
}

# Parallel gather for multiple values syntax:
# parallel_gather(sample_data, key = 'param', value =
# ends_with('mean'), sd = ends_with('sd'))
parallel_gather <- function(x, key, ..., convert = FALSE, factor_key = FALSE) {
  # enquos arguments
  lst <- rlang::quos(...)

  # check arguments
  if (length(lst) == 0)
    stop("Must pass at least one value = columns in parallel_gather()")
  if (is.null(names(lst)) || any(names(lst) == "")) {
    stop("All arguments to parallel_gather() must be named")

  }

  # use a hack to get column names as character using tidyeval
  # and dplyr
  col_names <- tibble::as_tibble(stats::setNames(as.list(colnames(x)),
    colnames(x)))
  lst_as_colnames <- lapply(lst, function(name_quo) {
    dplyr::select(col_names, !!name_quo) %>% colnames()
  })

  # check length (each argument should refer to the same number
  # of columns)
  arg_col_count <- vapply(lst_as_colnames, length, integer(1))
  if (!length(unique(arg_col_count)) == 1) {
    stop("All named arguments must refer to the same number of columns")
  }

  # id variables are those not mentioned in ...
  id_vars <- setdiff(colnames(x), unlist(lst_as_colnames))

  # do gather for each item in ..., using id_vars and cols
  # mentioned in each argument
  gathered <- lapply(seq_along(lst_as_colnames), function(i) {
    tidyr::gather_(x[c(id_vars, lst_as_colnames[[i]])], key = key,
      value = names(lst_as_colnames)[i], gather_cols = lst_as_colnames[[i]],
      na.rm = FALSE, convert = convert, factor_key = factor_key)
  })

  # get id data
  id_data <- gathered[[1]][c(id_vars, key)]

  # select non-id vars for each melt operation
  gathered <- lapply(gathered, function(df) df[setdiff(colnames(df),
    c(id_vars, key))])

  # return cbind operation
  dplyr::bind_cols(id_data, gathered)
}

# TABLE HELPER FUNCTIONS ####

treat_recode <- function(df) {
  mutate(TreatRow = fct_recode(as.factor(TreatRow), `Do-Do` = "1",
    `Do-Int` = "2", `Int-Do` = "3", `Int-Int` = "4", `No-Do` = "5"))
}

TreatCombinations <- function(df = ADSX, unique_treatments = 2,
  combos = 2) {
  1:unique_treatments %>% combn(combos) %>% as.data.frame() %>%
    as.list()
}

unite_spread <- function(df) {
  unite(df, Results, estimate, p.value, sep = " ") %>% spread(TreatCol,
    Results)
}

################# Treatment assignment functions ####

# Power test simulation functions


################# Pre-model data cleaning functions
### The 'kitchen sink' of cleans and imputes:

clean_sink <- function(df) {
  require("dplyr")
  step_meanimpute(all_numeric(), -all_outcomes()) %>%
      step_knnimpute(all_nominal()) %>%
    step_center(all_numeric(), -all_outcomes()) %>%
    step_scale(all_numeric(),
    -all_outcomes()) %>%
    step_other(all_nominal())
}


############### Formatting stuff ####

# Number formatting

op <- function(x, d=3){
    format(x, format="f", big.mark=",", digits=d,
           scientific=FALSE)}

ops <- function(x, d=3, ns=2){
    format(x, format="f", big.mark=",", digits=d, nsmall=ns, scientific = FALSE)
options(scipen=999)
}

# Color options for either version of markdown slides

colFmt = function(x, color) {
  outputFormat = knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (outputFormat == "latex")
    paste("\\textcolor{", color, "}{", x, "}", sep = "") else if (outputFormat == "html")
    paste("<font color='", color, "'>", x, "</font>", sep = "") else x
}

# TODO: formatting options for bookdown etc

comb2pngs <- function(imgs, bottom_text = NULL){
  img1 <-  grid::rasterGrob(as.raster(readPNG(imgs[1])),
                            interpolate = FALSE)
  img2 <-  grid::rasterGrob(as.raster(readPNG(imgs[2])),
                            interpolate = FALSE)
  grid.arrange(img1, img2, ncol = 2, bottom = bottom_text)
}


#multi-output text color
#https://dr-harper.github.io/rmarkdown-cookbook/changing-font-colour.html#multi-output-text-colour
#We can then use the code as an inline R expression format_with_col("my text", "red")

format_with_col = function(x, color){
  if(knitr::is_latex_output())
    paste("\\textcolor{",color,"}{",x,"}",sep="")
  else if(knitr::is_html_output())
    paste("<font color='",color,"'>",x,"</font>",sep="")
  else
    x
}

## More formatting stuff ###..#

.kable_styling <- hijack(kableExtra::kable_styling, full_width=FALSE)
.kable <- hijack(knitr::kable, format.args = list(big.mark = ",", scientific = FALSE))


################# Coding shortcuts ####

#help find where a column with a certain string name takes values across years (or other grouping)
yfind <- function(df = eas_all, text, n=3, y=year) {
  df %>%
    dplyr::select(year, matches({text})) %>%
    group_by({{y}}) %>%
    sample_n(size = n)
}


#Case insensitive string_detect
str_det <- function(string, pattern, negate = FALSE) {
  str_detect(string, regex(pattern, ignore_case = T))
}

grp_n <- function (df, groupvar) {
df %>%
  group_by({{groupvar}}) %>%
  summarise(across(.cols = everything(),
                   .fns = list(n = ~ sum(!is.na(.x)))
  )
  )
}
#Note: the above doesn't capture cases where 'all values are the same within a group'


grp_uniq <- function (df, groupvar) {
df %>%
  group_by({{groupvar}}) %>%
  summarise(across(.cols = everything(),
                   .fns = list(uniq = ~ n_distinct(.x))
    )
  )
}

Sm <- function(df, X) dplyr::select(df, matches({X},  ignore.case = FALSE))  # Sm<t_?X>("x") selects variables matching string 'x', case-sensitive
sm <- function(df, X) dplyr::select(df, matches({X})) # ... not case-sensitive

Snotm <- function(df, X) dplyr::select(df, -matches({X},  ignore.case = FALSE)) # ...  case-sensitive
snotm <- function(df, X) dplyr::select(df, -matches({X})) # ... selects variables *not* matching that string, not case-sensitive


Smn <- function(df, X) dplyr::select(df, matches({X}, ignore.case = FALSE)) %>% names() #  Smn("x") creates vector of *names* of variables matching string 'x', case-sensitive
smn <- function(df, X) dplyr::select(df, matches({X})) %>% names() # not case-sensitive


# Added by Oska

remove_str_list <- function(list, string){
  list <- Filter(function(x) !any(grepl(string, x)), list)
  return(list)
}

lab_list_to_text <- function(df) {
  df %>%
    var_label %>% unname %>% unlist() %>%
    paste(collapse = ', ')
}

## Group by and summarise
# Quick group by function to look at NA or 0 values for each year
group_by_sum <- function(df, col, group=year, value=NA, name="n_NA"){
  # col = column to summarise
  # value = values to aggregate, i.e value = NA means summarise the NA values in a column by year
  # name = output column name

  # Column name for proportion of col == value
  prop_name = paste("prop", name, sep="_")

  assertthat::assert_that(class(name) == "character", msg="Name must be a string")

  df %>% dplyr::group_by({{group}}) %>%
    dplyr::summarise(!!name := dplyr::if_else(is.na(value), # If value if NA then use is.na
                                            sum(is.na({{col}})),
                                            sum({{col}} == value, na.rm=TRUE)), # Else sum col == value
                     n = n()) %>%
    mutate(!!prop_name := !!parse_expr(name)/n)
}

group_mean_conf_int <- function(df, var, groups = NULL, se_func = se, ...){
      # Function to calculate confidence intervals for a variable given grouping variables
      ci <- function(x, se, lower = TRUE){
        x + 1.96*se
      }

      var_s <- rlang::as_string(rlang::ensym(var))
      df %>%
      group_by(across({{groups}})) %>%

      summarise(across({{ var }},
                       .fns = list(mean = ~mean(.x, na.rm=TRUE),
                                   se = se_func),
                       .names = "{.col}_{.fn}")) %>%
      mutate("upper_ci_{{var}}" := .data[[stringr::str_c(var_s, "_mean")]] + 1.96*.data[[stringr::str_c(var_s, "_se")]],
             "lower_ci_{{var}}" := .data[[stringr::str_c(var_s, "_mean")]] - 1.96*.data[[stringr::str_c(var_s, "_se")]])

}
