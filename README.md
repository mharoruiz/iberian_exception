# replication

This repository contains files to replicate the figures and tables in *Haro-Ruiz, M., Schult, C. & Wunder, C. (2023). The effects of the Iberian Exception mechanism on wholesale electricity prices and consumer inflation. A synthetic control approach.*

This project lives in an `renv` reproducible environment which uses R version 4.2.3. It is recommended that you adjust your R version accordingly to execute the files successfully. 

To set up the virtual environment and replicate the results, follow these instructions:

1. Open `replication.Rproj` and execute the following commands:

        install.packages("renv") # if necessary
        library(renv) 
        renv::restore()

2. Execute `replicate_results.R`.

**Note**: The `renv` environment keeps track of the packages needed to execute the scripts as well as their version. While this helps with reproducibility, there are other aspects -such as the R version or the operating system- which may lead to issues in trying to execute the files.
