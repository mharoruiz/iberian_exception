# The effects of the Iberian exception

This repository contains files to replicate the figures and tables in *Haro-Ruiz, M., Schult, C. & Wunder, C. (2023). The effects of the Iberian Exception mechanism on wholesale electricity prices and consumer inflation. A synthetic-controls approach.*

You can replicate the results by running `main.R` locally. However, this is likely to produce different results depending on the operating system or versions of system libraries installed, among others factors. A more robust approach is to execute the scripts in a Docker container. To do so, you must have [Docker](https://www.docker.com/) installed in you local machine.

Begin by cloning this repository from the command line: 

```shell
git clone https://github.com/mharoruiz/iberian_exception && \
    cd iberian_exception
```

Next, run a Docker container from an image that contains all the necessary packages: 

```shell
docker run --rm \ # run docker image
    -p 8787:8787 \ 
    -e DISABLE_AUTH=true \
    -v $(pwd):/home/rstudio/iberian_exception \ # mount repository directory into container
    -v /home/rstudio/iberian_exception/renv \ # mount renv directory into container
    mharoruiz/ibex:v0.1 # image to run container from
```

Now you can access the image by pointing your browser to `localhost:8787`. This will open an instance of RStudio which is ready to reproduce the results. To begin the replication, first open `iberian_exception.Rproj`, and then execute `main.R` by pressing `ctrl`/`cmd`+`tab`+`S`.