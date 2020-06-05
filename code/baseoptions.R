#Options and sourcing for general Reinstein code

knitr::opts_chunk$set(echo = TRUE,include=TRUE, warning=FALSE)
#knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

library(pacman)
#p_load(knitr, dplyr, tidyverse, here, janitor, citr, reporttools, magrittr, glue, experiment, estimatr, broom, kableExtra, purrr, ggsignif, recipes, pwr,lubridate,huxtable,sandwich,randomizr)

#p_load_gh("acoppock/attrition", "ngreifer/cobalt") # Alexander Coppock package to calculate trimming bounds. Not on CRAN
p_load(arsenal, DescTools, blockTools, broom, car, citr, cobalt, codebook, coefplot, data.table, dataMaid, dplyr, estimatr, experiment, forcats, furniture, ggsignif, glmnet, glmnetcr, glue, here, huxtable, janitor, kableExtra, knitr, lmtest, lubridate, magrittr, paramtest, plyr, psych,  purrr, purrr, pwr, pwr, randomizr, readxl, recipes, recipes, reporttools, rlang, rsample, sandwich, sjlabelled, sjmisc, skimr, snakecase, statmod, statmod, summarytools, tidyverse, todor)

options(kableExtra.latex.load_packages = FALSE)

options(warning.length = 100)
options(nwarnings = 1) # trying to limit display of  warnings; I don't think  it is working!
options(max.print = 1000)

options(scipen = 1, digits = 2)

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


