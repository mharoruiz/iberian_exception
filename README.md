# replication

This repostiory contains files to replicate the figures and tables in *Haro-Ruiz, M., Schult, C. & Wunder, C. (2023). The effects of the Iberian Exception mechanism on
wholesale electricity prices and consumer inflation. A
synthetic control approach* 

1. Open the .Rproj file and set up the virtual environment using the following commands:

        library(renv) # install.packages("renv") if necessary
        renv::activate()
        renv::restore()

2. Execute *replicate_results.R.*

*Note:* This project lives in and renv reproducible environment, which keeps track of the packages needed to execute the scripts as well as their version. While this helps with reproducibility, there are other aspects -such as the R version or the operating system- which may lead to issues in trying to execute the files.
