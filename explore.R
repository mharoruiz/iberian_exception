#'
#' This script outlines a framework to further explore the effect of the Iberian 
#' exception mechanism on inflation using the ibex tool. In particular, it 
#' estimates the effect of the intervention on goods-only CPI and services-only 
#' CPI, as well as the inflation rates of these indicators. 
#' 
#' Refer to the README.md file in https://github.com/mharoruiz/ibex for a 
#' detailed description on what this script does, as well as challenges to 
#' deepen your understanding of the effect of the Iberian exception mechanism
#' on inflation. 
#'
rm(list=ls())
set.seed(93479)

### Define constants 
SUB_VARS = c("GD", "SERV")
WHOLE_VAR = "CP00"
INPUT_VARS = c(SUB_VARS, WHOLE_VAR)
PRE_TREATMENT_PERIODS = rep(108, 3)
CONFIDENCE_INTERVALS = TRUE
if (CONFIDENCE_INTERVALS) PRC_STEP = .025 # Define step size for confidence interval grid-search
SAVE_RESULTS = FALSE
SAVE_OUTPUT = TRUE

# Load required packages and functions
library(tidyr)
library(dplyr)
library(logger)
functions = c(
  "estimate_sc",
  "inference_sc", 
  "plot_results", 
  "plot_decomposition"
)
invisible(
  lapply(paste0("01_functions/", functions, ".R"), source)
)

### Compute results

# Estimate synthetic controls 
sc_series_xpl = estimate_sc(
  outcomes = INPUT_VARS,
  T0s = PRE_TREATMENT_PERIODS,
  precision = PRC_STEP,
  compute_ci = CONFIDENCE_INTERVALS,
  save_csv = SAVE_RESULTS
)
# Estimate p-values for full post-treatment period
sc_inf_xpl = inference_sc(
  outcomes = INPUT_VARS,
  T0s = PRE_TREATMENT_PERIODS,
  save_csv = SAVE_RESULTS
)

# Compute synthetic and observed year-on-year inflation rate from CPI series
sc_inflation_rate_xpl = sc_series_xpl |>
  group_by(outcome, treated) |>
  mutate(
    obs = (obs - lag(obs, n = 12L))/lag(obs, n = 12L)*100, 
    synth = (synth - lag(synth, n = 12L))/lag(synth, n = 12L)*100,
    gaps = obs-synth
  ) |>
  ungroup() |>
  drop_na(obs) |>
  select(date, obs, synth, gaps, outcome, treated)

### Plot results 

# Observed and synthetic goods-only CPI series, and difference between them.
fig_gd = plot_results(
  df = sc_series_xpl, 
  var = SUB_VARS[1], 
  plot_ci = CONFIDENCE_INTERVALS
  ) +
  labs(
    title = "Effect of the IbEx on goods-only CPI",
    subtitle = "Index, 2015=100 - 90% confidence intervals"
  )
# Observed and synthetic services-only CPI series, and difference between them.
fig_serv = plot_results(
  df = sc_series_xpl, 
  var = SUB_VARS[2], 
  plot_ci = CONFIDENCE_INTERVALS
  ) +
  labs(
    title = "Effect of the IbEx on services-only CPI",
    subtitle = "Index, 2015=100 - 90% confidence intervals"
  )
# Observed and synthetic goods-only inflation rate series, and difference between them.
fig_gd_rate = plot_results(
  df = sc_inflation_rate_xpl, 
  var = SUB_VARS[1]
  ) +
  labs(
    title = "Effect of the IbEx on goods-only inflation rate",
    subtitle = "%, year-on-year inflation rate"
  )
# Observed and synthetic services-only inflation rate series, and difference between them.
fig_serv_rate = plot_results(
  df = sc_inflation_rate_xpl, 
  var = SUB_VARS[2]
  ) +
  labs(
    title = "Effect of the IbEx on services-only inflation rate",
    subtitle = "%, year-on-year inflation rate"
  )
# Decomposition of the effect on Spain’s inflation rate.
fig_decomp_es = plot_decomposition(
  df = sc_inflation_rate_xpl, 
  whole_var = WHOLE_VAR,
  sub_vars = SUB_VARS,
  treated_unit = "ES"
  ) +
  labs(
    title = "Decomposition of the effect of the IbEx on Spain's inflation rate",
    subtitle = "%, year-on-year inflation rate"
  )
# Decomposition of the effect on Portugal’s inflation rate.
fig_decomp_pt = plot_decomposition(
  df = sc_inflation_rate_xpl, 
  whole_var = WHOLE_VAR,
  sub_vars = SUB_VARS,
  treated_unit = "PT"
  ) +
  labs(
    title = "Decomposition of the effect of the IbEx on Portugals's inflation rate",
    subtitle = "%, year-on-year inflation rate"
  )

### Save output
if (SAVE_OUTPUT) {
  if (!dir.exists("05_exploration")) dir.create("05_exploration") 
  # Save figures
  log_info("Saving figures in 05_exploration/")
  figures = as.list(
    c(
      "fig_gd", "fig_serv", 
      "fig_gd_rate", "fig_serv_rate",
      "fig_decomp_es", "fig_decomp_pt"
    )
  )
  for (f in figures) {
    ggsave(
      filename = paste0(as.character(f), ".png"),
      plot = get(f), 
      path = "05_exploration/",
      height = 5.5,
      width = 10
    )
  }
}



