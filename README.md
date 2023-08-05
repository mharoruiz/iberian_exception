# Replication for "Twelve months after the Iberian exception: A synthetic-controls estimation of its effect on inflation"

This repostiory contains files to replicate the plots and tables in *Haro-Ruiz, M. & Schult, C. (2023). Twelve months after the Iberian exception: A synthetic-controls estimation of its effect on inflation.* 

The project lives in a Repodrucible R environment, which uses R version 4.2.3. Note that some of the dependencies for certain packages may not be available in different R versions. Consider adjusting your R version accordingly to successfully replicate the results. 

In order to set up the virtual environment in your local machine, type the following lines in your R command line: 

    library(renv) # install.packages("renv") if necesasry
    renv::activate()
    renv::restore()
