#'
#' This file replicates the figures and tables in Haro-Ruiz, M.,  Shcult C., and
#' Wunder, C. (2023). The effects of the Iberian exception mechanism on 
#' wholesale electricity prices and consumer inflation. A synthetic-controls 
#' approach.
#' 
#' This project lives in an renv reproducible environment, which uses R 
#' version 4.2.3. 
#' 
#' To set up the virtual environment and replicate the results, follow these 
#' instructions:
#'   
#' 1. Execute the following commands:
#'   
#'         install.packages("renv") # if necessary
#'         library(renv) 
#'         renv::restore()
#' 
#' 2. Version 5.0.2 of package curl will be installed by default. However, 
#' this version will conflict with package eurostat. To resolve this conflict,
#' change the version manually with: 
#'   
#'         install("curl@5.0.1")
#'
#' 3. Execute this file by pressing Ctrl/Cmd + Shift + Return to execute the 
#' file as a whole. 
#'

rm(list=ls())

# Import functions
functions = c(
  "estimate_sc",
  "inference_sc", 
  "plot_results", 
  "plot_decomposition",
  "get_ate_table", 
  "get_pval_table"
  )
invisible(
  lapply(paste0("functions/", functions, ".R"), source)
  )

# Load required packages
require(tidyverse)

# Set seed to replicate results
set.seed(51231)

### Estimate results 

# Estimate synthetic controls 
estimate_sc(
  outcomes = c("DAA", "CP00", "NRG", "TOT_X_NRG"),
  T0s = c(89, 108, 108, 108),
  precision = 0.01, # Reduce the number of decimal figures to reduce computational time
  compute_ci = TRUE,
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
sc_series = read_csv("results/sc_series_001.csv", show_col_types = FALSE) 
sc_inf_12 = read_csv("results/sc_inference_12.csv", show_col_types = FALSE)
sc_inf_6_6 = read_csv("results/sc_inference_6_6.csv", show_col_types = FALSE)

# Compute synthetic and observed year-on-year inflation rate from CPI series
sc_inflation_rate = sc_series |>
  filter(outcome != "DAA") |>
  group_by(outcome, treated) |>
  mutate(
    obs = (obs - lag(obs, n = 12L))/lag(obs, n = 12L)*100, 
    synth = (synth - lag(synth, n = 12L))/lag(synth, n = 12L)*100,
    gaps = obs-synth
  ) |>
  ungroup() |>
  drop_na(obs) |>
  select(date, obs, synth, gaps, outcome, treated)

### Fig 1. Observed and Synthetic day-ahead price series, and difference between them with 90% CIs.
fig_1 = plot_results(df = sc_series, var = "DAA", plot_ci = TRUE) +
  labs(
    title = "Effect of the IbEx day-ahead price",
    subtitle = "Index, 2015=100 - 90% confidence intervals"
  )

### Fig 2. Observed and Synthetic energy CPI series, and difference between them with 90% CIs.
fig_2 = plot_results(df = sc_series, var = "NRG", plot_ci = TRUE) +
  labs(
    title = "Effect of the IbEx energy CPI",
    subtitle = "Index, 2015=100 - 90% confidence intervals"
  )

### Fig 3. Observed and Synthetic overall CPI series, and difference between them with 90% CIs.
fig_3 = plot_results(df = sc_series, var = "CP00", plot_ci = TRUE) +
  labs(
    title = "Effect of the IbEx overall CPI",
    subtitle = "Index, 2015=100 - 90% confidence intervals"
  )

### Fig 4. Decomposition of the effect on Spain’s inflation rate.
fig_4 = plot_decomposition(df = sc_inflation_rate, treated_unit = "ES") +
  labs(
    title = "Decomposition of the effect of the IbEx on Spain's inflation rate",
    subtitle = "%, year-on-year inflation rate"
  )
  
### Table A1
# ATEs and confidence intervals
ate_tab = get_ate_table(df = sc_series, unit = "idx")
# P-values
pval_tab = get_pval_table(dfs = list(sc_inf_12, sc_inf_6_6))
# Replicate Table A1
table_A1 = inner_join(ate_tab, pval_tab, by = c("outcome", "period")) |>
  select(
    outcome, period, 
    ate_ES, ate_lower_ES, ate_upper_ES, pval_ES,
    ate_PT, ate_lower_PT, ate_upper_PT, pval_PT
    ) |>
  mutate(
    outcome = 
      factor(
        outcome,
        levels = c("DAA", "NRG", "CP00", "TOT_X_NRG")
      ),
    period = 
      factor(
        period,
        levels = c("07/2022 - 06/2023", "07/2022 - 12/2022", "01/2023 - 06/2023")
      )
  ) |>
  arrange(outcome, period)

### Fig. B1. Observed and synthetic energy inflation rate series, and difference between them.
fig_B1 = plot_results(df = sc_inflation_rate, var = "NRG", plot_ci = FALSE) +
  labs(
    title = "Effect of the IbEx energy inflation rate",
    subtitle = "%, year-on-year inflation rate"
    )

### Fig. B2. Observed and synthetic overall inflation rate series, and difference between them.
fig_B2 = plot_results(df = sc_inflation_rate, var = "CP00", plot_ci = FALSE) +
  labs(
    title = "Effect of the IbEx overall inflation rate",
    subtitle = "%, year-on-year inflation rate"
  )

### Fig. B3. Observed and synthetic overall inflation rate series, and difference between them.
fig_B3 = plot_results(df = sc_inflation_rate, var = "TOT_X_NRG", plot_ci = FALSE) +
  labs(
    title = "Effect of the IbEx overall inflation rate excluding energy",
    subtitle = "%, year-on-year inflation rate"
  )

### Table B1
table_B1 = get_ate_table(df = sc_inflation_rate, unit = "rate")

### Fig. C1. Observed and Synthetic overall CPI excluding energy series, and difference between them with 90% CIs.
fig_C1 = plot_results(df = sc_series, var = "TOT_X_NRG", plot_ci = TRUE) +
  labs(
    title = "Effect of the IbEx overall CPI excluding energy",
    subtitle = "Index, 2015=100 - 90% confidence intervals"
  )

### Fig. C2. Decomposition of the effect on Spain’s inflation rate.
fig_C2 = plot_decomposition(df = sc_inflation_rate, treated_unit = "PT") +
  labs(
    title = "Decomposition of the effect of the IbEx on Spain's inflation rate",
    subtitle = "%, year-on-year inflation rate"
  )

