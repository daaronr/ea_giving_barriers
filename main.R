# 'main.R': this single file should (ideally) source and build all content (at least the main bookdown) ####

## Note: 17 Nov 2021  -- I'm changing the system here from the 'just build' process. This may require us to change the tutorial bit

#### Setup ####

try_download <- function(url, path) {
  new_path <- gsub("[.]", "X.", path)
  tryCatch({
    download.file(url = url,
                  destfile = new_path)
  }, error = function(e) {
    print("You are not online, so we can't download")
  })
  tryCatch(
    file.rename(new_path, path)
  )
}

library(here)
here <- here::here()


#... Import setup for this project using template from dr-rstuff  ####

#devtools::install_github("rethinkpriorities/rp-r-package")
#library(rethinkpriorities)

dir.create(here::here("code"))

try_download(
  "https://raw.githubusercontent.com/daaronr/dr-rstuff/master/functions/project_setup.R",
  here::here("code", "project_setup.R")
)

try_download(
  "https://raw.githubusercontent.com/daaronr/dr-rstuff/master/functions/download_formatting.R",
  here::here("code", "download_formatting.R")
)

# Note: I used to do the 'install a set of packages thing here' ... but with renv we can just have renv search for and install these (in Rstudio it reminds you; otherwise use call `renv::dependencies()` or `renv::hydrate` I think. )

library(pacman)
knitr::opts_chunk$set(echo = TRUE)

pacman::p_load(
  bookdown, broom, dplyr, grid, gtsummary, huxtable, janitor, purrr, rlang, stats, tibble, tidyr, here, pryr, revealjs,
               install=FALSE) #first time set = TRUE and then renv::snapshot it

if (!require("devtools")) install.packages("devtools")
devtools::install_github("peterhurford/surveytools2") #installing this here bc renv doesn't detect it

## You MUST run this for anything else to work ####
source(here::here("code", "project_setup.R"))

##NOTE: these sourced files seem to need some packages to be installed.
#Todo -- 'embed' that somehow? (I just used Renv to add these for now)

#remotes::install_github("claudiozandonella/trackdown", build_vignettes = TRUE)
# trying 'trackdown' (https://bookdown.org/yihui/rmarkdown-cookbook/google-drive.html) to help collaborate dynamically
#library(trackdown)

source(here::here("code", "download_formatting.R"))

print("project_setup creates 'support' folder and downloads tufte_plus.css, header.html into it")
print("project_setup creates 'code' folder and downloads baseoptions.R, and functions.R into it, and sources these")

### Source model-building tools/functions

#Pulling in key files from other repos; don't edit them here
#dir.create(here("remote")) ..
#Not done here (yet)

p_load("bettertrace") #better tracking after bugs

#### BUILD the bookdown ####
#The line below should 'build the bookdown' in the order specified in `_bookdown.yml`

{
  options(knitr.duplicate.label = "allow")
  rmarkdown::render_site(output_format = 'bookdown::gitbook', encoding = 'UTF-8')
}

# trackdown command examples ####

#p_load(googledrive)
#remotes::install_github("claudiozandonella/trackdown", build_vignettes = TRUE)
#library(trackdown)

#see https://app.getguru.com/card/cd469abi/collab-writing-sessions-working-this-into-Github-and-Rmarkdown and https://rethinkpriorities.slack.com/archives/C027CUXNQTD/p1637074537043600 and https://claudiozandonella.github.io/trackdown/

# trackdown::upload_file(
#   file = here("power_analysis_framework_2_COLLAB.Rmd"),
#   shared_drive = "Research", #this works -- name looked up with googledrive::shared_drive_find()
#   hide_code = FALSE) #hide_code=TRUE is usually better but I want to see it for now

# Moving to Quarto ####

# Parsing command
p_load(rex)
source_url("https://raw.githubusercontent.com/daaronr/dr-rstuff/master/functions/parse_rp_bookdown_to_quarto.R")

# list of files taken from _bookdown.yml
rmd_files <- c("outline_work.Rmd", "present_puzzle.Rmd",  "substitution.Rmd",  "barriers_breakdown.Rmd", "BARRIERS_FUNDAMENTAL.md", "doesimpactmatter.Rmd", "aware-distance.Rmd", "identity.Rmd", "social.Rmd", "BARRIERS_INFO.md", "eval-aversion.Rmd", "impact_analytical.Rmd",  "BARRIERS_JUDGEMENT.md",  "quant-biases.Rmd", "factual.Rmd", "PATH_FORWARD.md", "tools.Rmd", "conclusion-agenda.Rmd", "appendix_tutorial.Rmd", "inertia.Rmd", "references.Rmd")

# apply all parsing commands and put it into 'chapters' folder
system("mkdir chapters")
map2(rmd_files, rmd_files,
  ~ rp_rmd_to_quarto(here::here("sections", .y), here::here("chapters", .y)))

newName <- sub(".Rmd", ".qmd", here::here("chapters", rmd_files))

file.rename(here::here("chapters", rmd_files), newName)

##TODO:
# - index.Rmd file needs will need adjusting,
# - you need to create a _quarto.yml file




