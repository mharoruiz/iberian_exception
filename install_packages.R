rm(list=ls())

# install necessary packages to execute main.R
packages <- c("readr", "stringr", "logger", "tidyr","dplyr", "ggplot2", 
              "eurostat", "scinference", "lubridate", "scales", "limSolve")

# load packages
lapply(packages, function(x) {
  if (!(x %in% installed.packages())) {
    install.packages(x)
  }
})
lapply(packages, library, quietly = TRUE, character.only = TRUE)