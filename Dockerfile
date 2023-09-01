# STAGE 1: renv-related code
FROM rocker/rstudio:4.2.3 AS base

# install renv
ENV RENV_VERSION 1.0.2
RUN R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/renv/renv_${RENV_VERSION}.tar.gz', repos=NULL, type='source')"

WORKDIR /home/rstudio/project

# Copy renv files
RUN mkdir -p renv
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# change default location of cache to project folder
RUN mkdir renv/.cache
ENV RENV_PATHS_CACHE renv/.cache

# restore packages as defined in lockfile
RUN R -e "renv::restore()"

# STAGE 2
FROM rocker/rstudio:4.2.3

WORKDIR /home/rstudio/project
COPY --from=base /home/rstudio/project .

# Copy project files
COPY /01_functions /home/rstudio/project/01_functions
COPY /02_data /home/rstudio/project/02_data
COPY /03_results /home/rstudio/project/03_results
RUN mkdir -p 04_output
COPY main.R main.R

# 
RUN R -e "setwd('/home/rstudio/project')"
RUN R -e "source('main.R')"
