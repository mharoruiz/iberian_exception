#' 
#' This script reproduces the figures and tables in Haro-Ruiz, M., Schult, C. & 
#' Wunder, C. (2023). 
#' 
#' The runtime of the script is regulated by constant PRC_STEP, which is defined 
#' in line 21 and determines the precision of the confidence intervals for the 
#' treatment effect. By default, PRC_STEP = .1, allowing for a relatively quick
#' execution. Note that the results presented in the paper were obtained with 
#' PRC_STEP=.001 (These results are saved to 03_results/sc_series_001.csv). 
#'
rm(list=ls())
set.seed(51231)

# Define constants 
SUB_VARS = c("NRG", "xNRG")
WHOLE_VAR = "CP00"
CPI_VARS = c(SUB_VARS, WHOLE_VAR)
INPUT_VARS = c("DAP", CPI_VARS)
PRE_TREATMENT_PERIODS = c(89, 108, 108, 108)
CONFIDENCE_INTERVALS = TRUE
if (CONFIDENCE_INTERVALS) PRC_STEP = .1 # Define step size for confidence interval grid-search
SAVE_RESULTS = TRUE
SAVE_ANALYSIS = TRUE

# Load required packages and functions
library(readr)
library(stringr)
library(logger)
library(tidyr)
library(dplyr)
library(ggplot2)
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
estimate_sc(
  outcomes = INPUT_VARS,
  T0s = PRE_TREATMENT_PERIODS,
  precision = PRC_STEP,
  compute_ci = CONFIDENCE_INTERVALS,
  save_csv = SAVE_RESULTS
)
# Conduct inference on estimates
inference_sc(
  outcomes = INPUT_VARS,
  T0s = PRE_TREATMENT_PERIODS,
  method = "ttest",
  save_csv = SAVE_RESULTS
)

# Determine sc_series path
if (CONFIDENCE_INTERVALS) {
  suffix = paste0("_", str_split(PRC_STEP, "\\.")[[1]][2])
} else {
  suffix = ""
}
series_path = sprintf(
  "03_results/sc_series%s.csv",
  suffix
)
# Import SC results
sc_series = read_csv(series_path, show_col_types = FALSE) 
sc_inf = read_csv("03_results/sc_inference_ttest.csv", show_col_types = FALSE)

# Compute synthetic and observed year-on-year inflation rate from CPI series
sc_inflation_rate = sc_series |>
  filter(outcome %in% CPI_VARS) |>
  group_by(outcome, treated) |>
  mutate(
    obs_rate = (obs - lag(obs, n = 12L))/lag(obs, n = 12L)*100, 
    synth_rate = case_when(
      date > as.Date("2022-06-01") ~ (synth - lag(obs, n=12L))/lag(obs, n=12L)*100,
      TRUE ~ (obs - lag(obs, n = 12L))/lag(obs, n = 12L)*100
    ),
    gaps_rate = obs_rate - synth_rate
  ) |>
  ungroup() |>
  select(date, obs_rate, synth_rate, gaps_rate, outcome, treated)

### Replicate figures and tables

### Fig 1.
fig_1 = plot_results(
  df = sc_series, 
  var = INPUT_VARS[1], 
  plot_ci = CONFIDENCE_INTERVALS
  ) +
  labs(
    title = "Effect of the IbEx on day-ahead price",
    subtitle = "Index, 2015=100 - 90% confidence intervals"
  )

### Fig 2. 
fig_2 = plot_results(
  df = sc_series, 
  var = INPUT_VARS[2], 
  plot_ci = CONFIDENCE_INTERVALS
  ) +
  labs(
    title = "Effect of the IbEx on energy CPI",
    subtitle = "Index, 2015=100 - 90% confidence intervals"
  )

### Fig. 3.
fig_3 = plot_results(
  df = sc_series, 
  var = INPUT_VARS[3], 
  plot_ci = CONFIDENCE_INTERVALS
) +
  labs(
    title = "Effect of the IbEx on all-items CPI excluding energy",
    subtitle = "Index, 2015=100 - 90% confidence intervals"
  )

### Fig 4.
fig_4 = plot_results(
  df = sc_series, 
  var = INPUT_VARS[4], 
  plot_ci = CONFIDENCE_INTERVALS
  ) +
  labs(
    title = "Effect of the IbEx on all-items CPI",
    subtitle = "Index, 2015=100 - 90% confidence intervals"
  )

### Fig 5. Decomposition of the effect on Spain’s inflation rate.
fig_5 = plot_decomposition(
  df = sc_series, 
  whole_var = WHOLE_VAR,
  sub_vars = SUB_VARS, 
  treated_unit = "ES"
) +
  labs(
    title = "Decomposition of the effect of the IbEx on Spain's CP00"
  )

### Table A1.
# Absolute ATTs, SEs and CIs
att_abs_es = sc_inf |>
  filter(treated == "ES") |>
  select(outcome, att, se, lb, ub)
att_abs_pt = sc_inf |>
  filter(treated == "PT") |>
  select(outcome, att, se, lb, ub)
att_abs = inner_join(
  att_abs_es, att_abs_pt,
  by = "outcome",
  suffix = c("_ES", "_PT")
  ) |>
  mutate(unit = "abs")
# Percentage ATTs
att_pct_raw = sc_series |>
  filter(date > as.Date("2022-06-01")) |>
  group_by(outcome, treated) |>
  summarise_at("obs", mean) |>
  inner_join(
    sc_inf |> 
      select(outcome, treated, att),
    by = c("outcome", "treated")
  ) |>
  mutate(
    att = (att / obs) * 100
  ) |> 
  select(outcome, treated, att)
att_pct_es = att_pct_raw |>
  filter(treated == "ES") |>
  select(-treated)
att_pct_pt = att_pct_raw |>
  filter(treated == "PT") |>
  select(-treated)
att_pct = inner_join(
  att_pct_es,
  att_pct_pt,
  by = "outcome",
  suffix = c("_ES", "_PT")
  ) |>
  mutate(unit = "pct")
# Percentage-point ATTs for inflation rate series
att_rate_raw = sc_inflation_rate |>
  filter(date > as.Date("2022-06-01")) |>
  group_by(outcome, treated) |>
  summarise_at("gaps_rate", mean) |>
  select(outcome, treated, att = gaps_rate)
att_rate_es = att_rate_raw |>
  filter(treated == "ES") |>
  select(-treated)
att_rate_pt = att_rate_raw |>
  filter(treated == "PT") |>
  select(-treated)  
att_rate = inner_join(
  att_rate_es,
  att_rate_pt,
  by = "outcome",
  suffix = c("_ES", "_PT")
  ) |>
  mutate(unit = "rate")
# Combine all three ATT tables to create Table A1
table_A1 = bind_rows(att_abs, att_pct, att_rate) |>
  mutate(outcome = factor(outcome, levels = INPUT_VARS)) |>
  arrange(outcome, unit) |>
  select(
    outcome, unit, 
    att_ES, se_ES, lb_ES, ub_ES,
    att_PT, se_PT, lb_PT, ub_PT
    )

### Save output
if (SAVE_ANALYSIS) {
  if (!dir.exists("04_analysis")) dir.create("04_analysis") 
  # Save figures
  log_info("Saving figures in 04_analysis/")
  figures = as.list(
    c(
      "fig_1", "fig_2", "fig_3", "fig_4", "fig_5"
      )
    )
    for (f in figures) {
    ggsave(
      filename = paste0(as.character(f), ".png"),
      plot = get(f), 
      path = "04_analysis/",
      height = 5.5,
      width = 10
    )
  }
  # Save tables
  log_info("Saving tables in 04_analysis/")
  tables = c(
    "table_A1"
  )
  for (t in tables) {
    write_csv(
      get(t), 
      paste0("04_analysis/", as.character(t), ".csv")
    )
  }
}
