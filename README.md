# replication

This repostiory contains files to replicate the figures and tables in *Twelve months after the Iberian exception: A synthetic-controls estimation of its effect on inflation.* 

1. Begin by opening the .Rproj file and setting up up the virtual environment with the following commands:

        library(renv) # install.packages("renv") if necessary
        renv::activate()
        renv::restore()

2. Execute *replicate_results.R.*

**Note:** This project lives in and renv reproducible environment, which keeps track of the packages needed to execute the scripts and their version. While this helps with reproducibility, there are other aspects, such as the R version or the operating system, which may lead to issues while trying to execute the project.
