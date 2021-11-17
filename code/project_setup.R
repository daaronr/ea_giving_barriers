#### Script for sourcing basic options and functions ####

# No package loading, use renv instead!!

library(here)

here <- here::here

dir.create(here("code"))

##WARNING: do not run the script below within the `/dr-rstuff/master/` itself, at least not before commiting

try_download(
  "https://raw.githubusercontent.com/daaronr/dr-rstuff/master/functions/baseoptions.R",
  here::here("code", "baseoptions.R")
)

try_download(
  "https://raw.githubusercontent.com/daaronr/dr-rstuff/master/functions/functions.R",
  here::here("code", "functions.R")
)


source(here("code", "baseoptions.R")) # Basic options used across files and shortcut functions, e.g., 'pp()' for print

source(here("code", "functions.R")) # functions grabbed from web and created by us for analysis/output



