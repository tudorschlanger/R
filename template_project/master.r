##### Master file ######


## Define some paths based on username
info <- Sys.info()

if (info["user"] == "schl") {
  output_graphs <- ""
  output_tables <- ""
} else if (info["user"] == "") {
  
}


## Run these at the beginning of the project   
# install.packages("renv")
# library(renv)
# renv::init()


## Run this from time to time to lock a set of packages 
# renv::snapshot()

## Setting the working directory relative to the base folder of the Project directory\
install.packages("here")
library(here)
packages <- c("devtools", "tidyverse", "tibble", "collapse", 
              "ggplot2", "readxl", "stringr", "psych", "labelled", "fastDummies", 
              "jsonlite", "stargazer", "xtable", "knitr", "kableExtra", "lubridate", 
              "patchwork", "fixest", "readr")
source(here("code/src/check_packages.r"))
check_packages(packages)

## Load other packages using devtools
# Align Assign is an Addin for Rstudio which allows user to align equal signs 
devtools::install_github("seasmith/AlignAssign")

source(here("code/src/functions.R"))



