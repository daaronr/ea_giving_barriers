#### Options for general Reinstein code ####

# No package loading, use renv instead!!

knitr::opts_chunk$set(echo = TRUE,include=TRUE, warning=FALSE)
#knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

#Set function defaults
where <- pryr::where

#summ  <- surveytools2::data_summary
summ <- vtable::sumtable

vsumm <- surveytools2::var_summary
tab <- surveytools2::tab

remove <- base::remove

#removed rsample because it interfered with codebook

options(kableExtra.latex.load_packages = FALSE)

# options(warning.length = 100)
options(nwarnings = 1) # trying to limit display of  warnings; I don't think  it is working!
options(max.print = 1000)

# options(scipen = 1, digits = 2)

# set important functions to correct package
select <- dplyr::select
fill <- tidyr::fill
as_factor <- forcats::as_factor
rename <- dplyr::rename
count <- dplyr::count
filter <- dplyr::filter
group_by <- dplyr::group_by
coalesce <- dplyr::coalesce
here <- here::here


all_numeric <- gtsummary::all_numeric


first <- dplyr::first
last  <- dplyr::last
mutate <- dplyr::mutate


# Lazy man's typing shortcuts and composite functions
pp <- base::print
sel <- dplyr::select
ft <- dplyr::filter
gb <- dplyr::group_by
summarise <- dplyr::summarise
summ <- base::summary

#handy negation

'%ni%' <- Negate('%in%')


