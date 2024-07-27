# install all libraries to execute the main.R file.

rm(list=ls())

libraries <- c("readr", "stringr", "logger", "tidyr",
               "dplyr", "ggplot2", "lubridate", "eurostat")


# load libraries
lapply(libraries, function(x) {
  if (!(x %in% installed.packages())) {
    install.packages(x)
  }
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)