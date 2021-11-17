#### Script for downloading R functions and bookdown formatting ####

#### Setup ####

library(here)
#library(checkpoint) #TODO ... in to avoid differential processing from different package versions

#Set function defaults
here <- here::here

#### Downloading R scripts and HTML formatting ####

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

dir.create(here("support"))

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



