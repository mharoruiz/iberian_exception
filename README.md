# replication

This repostiory contains files to replicate the figures and tables in *Twelve months after the Iberian exception: A synthetic-controls estimation of its effect on inflation.* 

1. Begin by setting up the virtual environment using the following commands:

        library(renv) # install.packages("renv") if necessary
        renv::activate()
        renv::restore()

3. Open the  .Rproj file and execute *replicate_results.R.*

This project lives in a repodrucible R environment, which uses R version 4.2.3. Note that some of the dependencies for certain packages may not be available in different R versions. In most cases, you won't have to, but consider adjusting you R version accordingly to successfully replicate the results.
