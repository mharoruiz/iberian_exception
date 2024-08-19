#' 
#' This script reproduces the figures in Haro Ruiz, M., Schult, C. & 
#' Wunder, C. (2024). 
#' 
#' The runtime of the script depends on constant PRC_STEP, which is defined in 
#' line 22 and determines the precision of the confidence intervals for the 
#' treatment effect. By default, PRC_STEP = .01, allowing for a relatively quick
#' execution. Note that the results presented in the paper were obtained with 
#' PRC_STEP = .001 (These results are saved to 03_results/sc_series_001.csv). 
#'

rm(list=ls())
set.seed(61876)

# Define constants 
SUB_VARS = c("NRG", "xNRG")
WHOLE_VAR = "CP00"
CPI_VARS = c(SUB_VARS, WHOLE_VAR)
INPUT_VARS = c("DAP", CPI_VARS)
CONFIDENCE_INTERVALS = TRUE
# Define step size for confidence interval grid-search
PRC_STEP = if (CONFIDENCE_INTERVALS) .1 else NA
INFER_METHOD = "ttest"
SAVE_SERIES = TRUE
SAVE_INFER = TRUE
SAVE_WEIGHTS = TRUE
SAVE_ANALYSIS = TRUE

# Load required packages and functions
library(readr)
library(stringr)
library(logger)
library(tidyr)
library(dplyr)
library(ggplot2)
functions = c("estimate_sc", "plot_results")
invisible(lapply(paste0("01_functions/", functions, ".R"), source))

### Synthetic controls estimation

estimate_sc(outcomes = INPUT_VARS, compute_ci = CONFIDENCE_INTERVALS, 
            precision = PRC_STEP, infer_method = INFER_METHOD,
            save_series = SAVE_SERIES, save_infer = SAVE_INFER, 
            save_weights = SAVE_WEIGHTS)

### In-time placebo tests

estimate_sc(outcomes = INPUT_VARS, compute_ci = CONFIDENCE_INTERVALS,
            precision = PRC_STEP, infer_method = INFER_METHOD,
            placebo_time = 48)

### Replicate figures and tables

# Determine sc_series path
if (CONFIDENCE_INTERVALS) {
  suffix = paste0("_", str_split(PRC_STEP, "\\.")[[1]][2])
} else {
  suffix = ""
}
series_path = sprintf("03_results/sc_series%s.csv", suffix)
# Import SC results
sc_series = read_csv(series_path, show_col_types = FALSE) 
sc_inf = read_csv("03_results/sc_inference_ttest.csv", show_col_types = FALSE)

# Compute synthetic and observed year-on-year inflation rate from CPI series
sc_inflation_rate = sc_series |>
  filter(outcome %in% CPI_VARS & date <= as.Date("2023-06-01")) |>
  group_by(outcome, treated) |>
  mutate(obs_rate = (obs - lag(obs, n = 12L))/lag(obs, n = 12L)*100,
         synth_rate = case_when(
           date > as.Date("2022-06-01") ~ (synth - lag(obs, n=12L))/lag(obs, n=12L)*100,
           TRUE ~ (obs - lag(obs, n = 12L))/lag(obs, n = 12L)*100
         ),
         gaps_rate = obs_rate - synth_rate) |>
  ungroup() |>
  select(date, obs_rate, synth_rate, gaps_rate, outcome, treated)

### Fig 1.
fig1 = plot_results(df = sc_series, var = INPUT_VARS[1], 
                    plot_ci = CONFIDENCE_INTERVALS) +
  labs(title = "Effect of Iberian exception on day-ahead price",
       subtitle = "Euros/MWh, 2015=100 - 90% confidence intervals")

### Fig 2. 
fig2 = plot_results(df = sc_series, var = INPUT_VARS[2], 
                    plot_ci = CONFIDENCE_INTERVALS) +
  labs(title = "Effect of Iberian exception on energy CPI",
       subtitle = "Index, 2015=100 - 90% confidence intervals")

### Fig. 3.
fig3 = plot_results(df = sc_series, var = INPUT_VARS[3],
                    plot_ci = CONFIDENCE_INTERVALS) +
  labs(title = "Effect of Iberian exception on all-items CPI excluding energy",
       subtitle = "Index, 2015=100 - 90% confidence intervals")

### Fig 4.
fig4 = plot_results(df = sc_series, var = INPUT_VARS[4], 
                    plot_ci = CONFIDENCE_INTERVALS) +
  labs(title = "Effect of Iberian exception on all-items CPI", 
       subtitle = "Index, 2015=100 - 90% confidence intervals")

### Save output
if (SAVE_ANALYSIS) {
  if (!dir.exists("05_analysis")) dir.create("05_analysis") 
  
  # Save figures
  log_info("Saving figures in 05_analysis/")
  figures = as.list(c("fig1", "fig2", "fig3", "fig4"))
    for (f in figures) {
      ggsave(filename = paste0(as.character(f), ".png"), plot = get(f),
             path = "05_analysis/", height = 5.5, width = 10)
    }
}
