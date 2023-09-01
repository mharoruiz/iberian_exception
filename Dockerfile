# STAGE 1: renv-related code
FROM rocker/rstudio:4.2.3 AS base

LABEL maintainer="Miguel Haro Ruiz <m.haroruiz@gmail.com>" \
      info="Replication for Haro-Ruiz, M., Schult, C. & Wunder, C. (2023)" \
      code="https://github.com/mharoruiz/iberian_exception" 

# install renv
ENV RENV_VERSION 1.0.2
RUN R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/renv/renv_${RENV_VERSION}.tar.gz', repos=NULL, type='source')"

WORKDIR /home/rstudio/iberian_exception

# Copy renv files
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
RUN mkdir -p renv
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# change default location of cache to project folder
RUN mkdir renv/.cache
ENV RENV_PATHS_CACHE renv/.cache

# restore packages as defined in lockfile
RUN R -e "renv::restore()"

# STAGE 2: project-related code
FROM rocker/rstudio:4.2.3

WORKDIR /home/rstudio/iberian_exception
COPY --from=base /home/rstudio/iberian_exception .

# Copy project files
COPY main.R main.R
COPY /01_functions /home/rstudio/iberian_exception/01_functions
COPY /02_data /home/rstudio/iberian_exception/02_data
RUN mkdir -p 03_results
RUN mkdir -p 04_output

# Execute main.R and save workspace
RUN R -e "setwd('/home/rstudio/iberian_exception')"
RUN R -e "source('main.R')"
RUN R -e "save.image('ibex_workspace.RData')"
