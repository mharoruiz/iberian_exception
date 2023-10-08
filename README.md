# ibex

A tool to explore the effect of the Iberian exception mechanism on inflation. It can be used to replicate the figures and tables in *Haro-Ruiz, M., Schult, C. & Wunder, C. (2023). The effects of the Iberian Exception mechanism on wholesale electricity prices and consumer inflation. A synthetic-controls approach*. Additionally, users can explore the effect of the intervention on a variety of inflation indicators.

![](.img/ibex_img_01.png)

## Deployment

ibex can be deployed in a Docker container. To do so, you must have [Docker](https://www.docker.com/) installed on your computer.

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

```R
- Project '~/ibex' loaded. [renv 1.0.2]
```

## Replication

The script `ibex/main.R` replicates the figures and tables in Haro-Ruiz, M., Schult, C. & Wunder, C. (2023). Execute the script by pressing Cmd/Ctrl + Shift + S or using the following command:

```R
source("~/ibex/main.R")
```

Once `main.R` has successfully run, the figures and tables will be accessible as variables in your R environment, i.e. `fig_1`, `table_A1`. Additionally, if constant `SAVE_ANALYSIS=TRUE`, the figures and tables will be saved to `04_analysis/` as .png and .csv files.

The runtime of the `main.R` is regulated by constant `PRC_STEP`, which is defined in line 21 of the script and determines the precision of the confidence intervals for the treatment effect. By default, `PRC_STEP=.1`, which allows for a relatively quick execution. Note that the results presented in the paper were obtained with `PRC_STEP=.001` (These results are saved to `03_results/sc_series_001.csv`). 

## Exploration

The script `ibex/explore.R` outlines a framework to further explore the effect of the Iberian exception mechanism on inflation using the ibex tool. In particular, it estimates the effect of the intervention on goods-only CPI and services-only CPI, as well as the inflation rates of these indicators. 

Like `main.R`, `explore.R` begins defining a series of constants used throughout the script. `SUB_VARS` is a vector with CPI variables which make up `WHOLE_VAR`. In this case, the combination of GD (goods only) and SERV (services only) make up CP00 (all items). Furthermore, `PRE_TREATMENT_PERIODS` is a vector with the size of the pretreatment periods for the computation of synthetic controls for each of the variables in `INPUT_VARS`. Since `INPUT_VARS` has length 3, `PRE_TREATMENT_PERIODS` must also have length 3. 

Moreover, `CONFIDENCE_INTERVALS=TRUE` and `PRC_STEP=.025`. This will result in more precise confidence intervals than the ones computed in `main.R`, where `PRC_STEP=.1`. The downside of more precise CIs is that it takes longer to compute them. You are encouraged to try different `PRC_STEP` values to figure out a good compromise between precision and runtime.

The function `estimate_sc()` is called in line 42 to estimate the synthetic controls for each of the CPI indicators and pre-treatment period sizes defined above. This returns a dataframe called `sc_series_explo` which will be used for visualizing the results.

Next, `explore.R` uses function `plot_results()` in lines 53 and 62 to plot the observed and synthetic series as well as the difference between them, for outcomes GD and SERV. If `SAVE_OUTPUT=TRUE`, the plots will be saved in `05_exploration/` as `fig_gd.png` and `fig_serv.png`. 

The script also calls function `plot_decomposition()` in lines 74 and 84 to visualize the effect of the Iberian exception mechanism on overall CPI as a share of the effect on goods-only and services-only CPI. The resulting plots for Spain and Portugal are saved in `05_exploration/` as `fig_decomp_es.png` and `fig_decomp_pt.png`.

---

**Challenge #1**

Create a dataframe that summarizes the results for each input variable and treated country over the entire post-treatment period. The dataframe should  show estimates in absolute and percentage terms, as well as the change in the inflation rate. Refer to lines 117-171 of explore.R for a walk-through solution of this exercise. 

**Challenge #2**

The figure below shows different aggregations of CPI indices and the relationship between them. For example, NRG (energy CPI) and xNRG (all-items CPI excluding energy) make up CP00 (all-items CPI). Similarly, we know from `explore.R` that GD and SERV make up CP00.

![](.img/ibex_img_02.png)

For this challenge, pick any two CPI aggregations that you would like to explore. The two variables of your choice should combine into a third, broader CPI category. For example, NRG and IGDxNRG combine to form IGD. 

Next, create a new R script and name it `my_exploration.R`. The script should first estimate synthetic control units for each of these CPI aggregations using function `estimate_sc()`.

Use the resulting dataframe to plot the results for each of these variables with `plot_results()`. You can also use `plot_decomposition()` to understand how the Iberian exception affected the two CPI aggregations of your choice with respect to the broader category; Which of the outcomes, if any, can better explain the effect on the third one? Are the results for Spain and Portugal similar? How do they differ?

---
