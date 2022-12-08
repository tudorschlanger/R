##### Master file ######

## Setting the working directory relative to the base folder of the Project directory
    install.packages("devtools")
    install.packages("here")
    library(here, devtools)
    here()

## Load all files in the src folder ; this folder contains useful custom-made R functions 
    
    # Load all necessary functions in the src folder 
    packages <- c("miceadds")
    source(here("code/src/check_packages.r"))
    check_packages(packages)
    
    source.all(here("code/src"), grepstring = "\\.r", print.source = TRUE, file_sep="")
    


