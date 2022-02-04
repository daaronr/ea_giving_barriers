#NOTE: THESE plotting functions were mainly geared for EA survey work

### Histograms and vertical lines ####

# function for histogram with specified breakpoints
hist_plot <- function(d,x){
  ggplot(d, aes(x = {{x}})) +
    geom_histogram(fill = "white", color = "black")   +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) }

# function for log scaled histogram with specified breakpoints
hist_plot_lscale <- function(d,x,breaks,prefix=""){
  ggplot(d, aes(x = x)) +
    geom_histogram(fill = "white", color = "black") +
    theme_classic() +
    scale_x_continuous(trans = pseudo_log_trans(base=10), breaks=breaks, labels=label_number_si(prefix=prefix)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) }

# function for adding a mean line and label to a histogram
# Todo: this code is crappy -- make it simply take function arguments .. so we don't need 3+ versions of this
geom_vline_mean <- function( x, tgap=1.3, label="Mean"){ list(geom_vline(xintercept = base::mean(x, na.rm = TRUE),
                                                                         colour = "blue"),
                                                              geom_text( aes( x = base::mean(x + tgap, na.rm = TRUE),
                                                                              label = label, y = 100 ),
                                                                         colour = "blue", angle = 45, size = 3.5 )) }

# function for adding a median line and label to a histogram
geom_vline_med <- function(x, tgap = 1.3, label = "Median") {
  list(
    geom_vline(
      xintercept = median(x, na.rm = TRUE),
      colour = "forestgreen",
      linetype = "dashed"
    ),
    geom_text(
      aes(
        x = median(x + tgap, na.rm = TRUE),
        label = label,
        y = 120,
      ),
      colour = "forestgreen",
      angle = 45,
      size = 3.5
    )
  )
}

geom_hline_med <- function(y, tgap = 1.3, label = "Median") {
  list(
    geom_hline(
      yintercept = median(y, na.rm = TRUE),
      colour = "forestgreen",
      linetype = "dashed"
    ),
    geom_text(
      aes(
        y = median(y, na.rm = TRUE),
        label = label
      ),
      colour = "forestgreen",
      angle = 45,
      size = 3.5    )
  )
}

# function for adding a 90th percentile line and label to a histogram
geom_vline_90 <- function(x, tgap = 1.3, label = "90th pct") {
  list(
    geom_vline(
      xintercept = quantile(x + tgap, 0.9, na.rm=TRUE),
      colour = "red",
      linetype = "dashed"
    ),
    geom_text(
      aes(
        x = quantile(x + tgap, 0.9, na.rm=TRUE),
        label = label,
        y = 200,
      ),
      colour = "red",
      angle = 45,
      size = 3.5
    )
  )
}

### Forest plot (of coefficients and confidence intervals) ####

# takes a particular tidy dataframe coming out of a model, with particular labels, as the argument

forest_plot <- function(coefs, ordered = TRUE, intercept=FALSE, ylab="", xlab = "", vline = 0){

    create_plot <- function(coefs){
        plot <- coefs %>% ggplot2::ggplot(aes(x = estimate, y = term)) +
        ggplot2::geom_pointrange(aes(xmin = conf.low, xmax = conf.high))
        return(plot)
    }

  if (intercept == FALSE){
    coefs <- coefs %>% dplyr::filter(term != "(Intercept)")
  }

  if (ordered != TRUE){
    forest <- create_plot(coefs)
  }
  else {
    forest <- coefs %>% dplyr::arrange(estimate) %>%
      dplyr::mutate(term = factor(term, levels = unique(term))) %>%
      create_plot()
  }

  forest <- forest +
    ylab(ylab) +
    xlab(xlab) +
    geom_vline(xintercept={{vline}}, colour="grey")


  return(forest)

}

grouped_forest_plot <- function(coefs, groups, ordered = TRUE,
                                intercept=FALSE, ylab="", xlab = "", vline = 0){

  create_plot <- function(coefs){
    plot <- coefs %>% ggplot2::ggplot(aes(x = estimate, y = term, colour = {{groups}})) +
      ggplot2::geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                               position = position_dodge(width = 0.8))
    return(plot)
  }

  if (intercept == FALSE){
    coefs <- coefs %>% dplyr::filter(term != "(Intercept)")
  }

  if (ordered != TRUE){
    forest <- create_plot(coefs)
  }
  else {
    forest <- coefs %>% dplyr::arrange(estimate) %>%
      dplyr::mutate(term = factor(term, levels = unique(term))) %>%
      create_plot()
  }

  forest <- forest +
    ylab(ylab) +
    xlab(xlab) +
    geom_vline(xintercept={{vline}}, colour="grey")


  return(forest)

}
