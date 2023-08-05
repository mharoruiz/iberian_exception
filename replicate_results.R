#'
#' This file replicates the figures and tables in Haro-Ruiz, M. and Shcult C.
#' (2023). Twelve Months After the Iberian Exception: A synthetic-control 
#' estimation of the effect on inflation. 
#' 
#' This project lives in an renv reproducible environment, which uses R version
#' 4.2.3. Note that some of the dependencies for certain packages may not be 
#' available in different R versions. Consider adjusting your R version 
#' accordingly to successfully execute this file. 
#' 
#' In order to setup the virtual environment in your local machine, type the 
#' following commands in the R console:
#' library(renv) # install.packages("renv") if necessary
#' renv::activate()
#' renv::restore()
#' 
#' To run the script, first restart your R session by pressing Ctrl/Cmd + Shift 
#' + 0 followed by Ctrl/Cmd + Shift + Return to execute the file as a whole. 
#'

# Import functions
functions = c(
  "estimate_sc",
  "inference_sc", 
  "plot_results", 
  "plot_weightedCPI",
  "get_ate_table", 
  "get_pval_table"
  )
invisible(
  lapply(paste0("utils/", functions, ".R"), source)
  )

# Load required packages
library(readr)

# Set seed to replicate results
set.seed(51231)

### Estimate results 

# Estimate synthetic controls 
estimate_sc(
  outcomes = c("DAA", "CP00", "NRG", "TOT_X_NRG"),
  T0s = c(89, 108, 108, 108),
  precision = 0.01, # Set precision higher to reduce run-time
  compute_ci = FALSE,
  save_csv = TRUE
)
# Estimate p-values for full post-treatment period
inference_sc(
  outcomes = c("DAA", "CP00", "NRG", "TOT_X_NRG"),
  T0s = c(89, 108, 108, 108),
  save_csv = TRUE
)
# Estimate p-values for 07/2022-12/2022 and 01/2023-06/2023 sub-periods
inference_sc(
  outcomes = c("DAA", "CP00", "NRG", "TOT_X_NRG"),
  T0s = c(89, 108, 108, 108),
  T1_breaks = c(as.Date("2022-12-01")),
  save_csv = TRUE
)

# Import SC results
sc_trends = read_csv("results/sc_trends_01.csv", show_col_types = FALSE) 
sc_inf_12 = read_csv("results/sc_inference_12.csv", show_col_types = FALSE)
sc_inf_6_6 = read_csv("results/sc_inference_6_6.csv", show_col_types = FALSE)

### Effect Plots 

# Plot DAA results 
figure_1 = plot_results(df = sc_trends, var = "DAA", plot_ci = TRUE)
# Plot NRG results 
figure_2 = plot_results(df = sc_trends, var = "NRG", plot_ci = TRUE)
# Plot CP00 results 
figure_A1 = plot_results(df = sc_trends, var = "CP00", plot_ci = TRUE)
# Plot CP00xNRG results 
figure_A2 = plot_results(df = sc_trends, var = "TOT_X_NRG", plot_ci = TRUE)

### Weighted CPI plots

# Spain
figure_3 = plot_weightedCPI(df = sc_trends, treated_unit = "ES", plot_ci = TRUE)
# Portugal
figure_A3 = plot_weightedCPI(df = sc_trends, treated_unit = "PT", plot_ci = TRUE)

### Result tables

# ATE table
table_A1 = get_ate_table(df = sc_trends)
# Inference table
table_A2 = get_pval_table(dfs = list(sc_inf_12, sc_inf_6_6))
