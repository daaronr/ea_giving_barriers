#### Script for importing data & importing packages ####

#This script aims to remove some of the repetition that occurs throughout these files
#Ideally we want to create all necessary objects - for performing analysis - in this file rather than creating objects in Rmd files

#### Setup ####

library(here)
#library(checkpoint) #TODO ... in to avoid differential processing from different package versions

# moved all sourcing/loading of packages (using pacman) to baseoptions.R

#Set function defaults
here <- here::here
where <- pryr::where


#### Sourcing R scripts and HTML formatting ####

#Function to try and download
try_download <- function(url, path) {
  new_path <- gsub("[.]", "X.", path)
  tryCatch({
    download.file(url = url,
                  destfile = new_path)
  }, error = function(e) {
    print("You are not online, so we can't download")
  })
  tryCatch(
    file.rename(new_path, path
    )
  )
}

try_download(
  "https://raw.githubusercontent.com/daaronr/dr-rstuff/master/bookdown_template/support/header.html",
  here::here("support", "header.html")
)

try_download(
  "https://raw.githubusercontent.com/daaronr/dr-rstuff/master/bookdown_template/support/tufte_plus.css",
  here::here("support", "tufte_plus.css")
)

# ... Downloading Bib files ####

try_download(
             "https://www.dropbox.com/s/3i8bjrgo8u08v5w/reinstein_bibtex.bib?raw=1",
             here::here("support", "reinstein_bibtex_dropbox.bib")
)


#Source R functions and baseoptions

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


