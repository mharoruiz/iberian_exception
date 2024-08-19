# ibex

A tool to explore the effect of the Iberian exception mechanism on inflation. It can be used to replicate the results of **Haro Ruiz, M, Schult, C., Wunder, C. (2023). The effects of the Iberian Exception mechanism on wholesale electricity prices and consumer inflation. A synthetic-controls approach**.


## Deployment

The are two options to execute ibex; either locally or via a Docker container. 

### Locally

To run the tool locally, clone the repository and make sure that all the necessary R packages are installed. To do so, you may run `install_packages.R` before executing `main.R`. This option is likely to result in issues due to conflicting R versions and package versions. 

### Docker

As an alternative, ibex can be deployed in a Docker container. To do so, you must have [Docker](https://www.docker.com/) installed on your computer.

Begin by cloning this repository from the command line:

```shell
git clone https://github.com/mharoruiz/ibex && \
cd ibex
```

Make sure your working directory points to the cloned repository. Depending on your command line, you can check this with `pwd` or `cd`.

Next, run a Docker container from an image that includes all the necessary dependencies:

```shell
docker run --rm \
-p 8787:8787 \
-e DISABLE_AUTH=true \
-v "$(pwd):/home/rstudio/ibex" \
-v /home/rstudio/ibex/renv \
mharoruiz/ibex:0.1
```

The above command creates and runs a new container (`docker run`) from the image `mharoruiz/ibex:0.1`, and mounts the project files (`-v`) from your local directory as source (`$(pwd)`) to the container directory as destination (`/home/rstudio/ibex`).

---

**Troubleshooting** 


Depending on the command line in your computer, you may have to use `%cd%` instead of `$(pwd)`. If none of these methods work, you can substitute the source with the absolute path to the cloned repository in your local machine (something like `/Users/user/ibex` or `C:\Users\user\ibex`). 

Another source for potential errors are the `\` at the end of every line in the `docker run` command above. These allow the execution of multiline and make the code block above easier to read than a single, long line of code. Depending on your command line, you may have to use `^` instead of `\`. However, you can also remove the linebreaks and execute the command as a single line.

---

The first time you execute `docker run` successfully, the command line should inform you that the image is being pulled from a Docker repository with the following message:

```
Unable to find image 'mharoruiz/ibex:0.1' locally
0.1: Pulling from mharoruiz/ibex
```

This process may take some time. However, subsequent executions of `docker run` will use a locally saved image to run the container. When this happens, your command line should print messages indicating that the container is being initialized. You can now access the container by pointing your browser to `localhost:8787`. This will open a ready-to-use instance of RStudio. 

Open `ibex/ibex.Rproj` in the RStudio session to load the project dependencies. The following message should appear in your R console:

```r
- Project '~/ibex' loaded. [renv 1.0.2]
```

## Replication

The script `ibex/main.R` replicates the results in Haro Ruiz, M., Schult, C. & Wunder, C. (2023). Execute the script by pressing Cmd/Ctrl + Shift + S or using the following command:

```r
source("~/ibex/main.R")
```

Once `main.R` has successfully run, the figures and tables will be accessible as variables in your R environment, i.e. `fig_1`. Additionally, if constant `SAVE_ANALYSIS=TRUE`, the figures will be saved to `04_analysis/` as .png files.

The runtime of the `main.R` depends on constant `PRC_STEP`, which is defined in line 22 of the script and determines the precision of the confidence intervals for the treatment effect. By default, `PRC_STEP=.1`, which allows for a relatively quick execution. Note that the results presented in the paper were obtained with `PRC_STEP=.001` (These results are saved to `03_results/sc_series_001.csv`). 
