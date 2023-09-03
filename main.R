#' 
#' This script reproduces the figures and tables in Haro-Ruiz, M., Schult, C. & 
#' Wunder, C. (2023). 
#' 
#' The runtime of the script is regulated by variable prc, which is defined in 
#' line 15 and determines the precision of the confidence intervals for the 
#' treatment effect. By default, prc=.1, allowing for a relatively quick 
#' execution. Note that the results presented in the paper were obtained with 
#' prc=.001 (These results are saved in 03_results/sc_series_001.csv). 
#'
rm(list=ls())

### Script settings 
set.seed(51231)
ci = TRUE
if (ci) prc = .1 # Define step size for confidence interval grid-search
save_results = TRUE
save_output = TRUE

# Load required packages and functions
suppressMessages(require(readr))
suppressMessages(require(stringr))
suppressMessages(require(logger))
suppressMessages(require(tidyr))
suppressMessages(require(dplyr))
suppressMessages(require(ggplot2))
functions = c(
  "estimate_sc",
  "inference_sc", 
  "plot_results", 
  "plot_decomposition",
  "get_ate_table", 
  "get_pval_table"
)
invisible(
  lapply(paste0("01_functions/", functions, ".R"), source)
)

### Compute results

# Estimate synthetic controls 
estimate_sc(
  outcomes = c("DAA", "CP00", "NRG", "TOT_X_NRG"),
  T0s = c(89, 108, 108, 108),
  precision = prc,
  compute_ci = ci,
  save_csv = save_results
)
# Estimate p-values for full post-treatment period
inference_sc(
  outcomes = c("DAA", "CP00", "NRG", "TOT_X_NRG"),
  T0s = c(89, 108, 108, 108),
  save_csv = save_results
)
# Estimate p-values for 07/2022-12/2022 and 01/2023-06/2023 sub-periods
inference_sc(
  outcomes = c("DAA", "CP00", "NRG", "TOT_X_NRG"),
  T0s = c(89, 108, 108, 108),
  T1_breaks = c(as.Date("2022-12-01")),
  save_csv = save_results
)

# Determine sc_series path
if (ci) {
  suffix = paste0("_", str_split(prc, "\\.")[[1]][2])
} else {
  suffix = ""
}
series_path = sprintf(
  "03_results/sc_series%s.csv",
  suffix
)
# Import SC results
sc_series = read_csv(series_path, show_col_types = FALSE) 
sc_inf_12 = read_csv("03_results/sc_inference_12.csv", show_col_types = FALSE)
sc_inf_6_6 = read_csv("03_results/sc_inference_6_6.csv", show_col_types = FALSE)

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

### Replicate figures and tables

### Fig 1. Observed and Synthetic day-ahead price series, and difference between them with 90% CIs.
fig_1 = plot_results(df = sc_series, var = "DAA", plot_ci = ci) +
  labs(
    title = "Effect of the IbEx day-ahead price",
    subtitle = "Index, 2015=100 - 90% confidence intervals"
  )

### Fig 2. Observed and Synthetic energy CPI series, and difference between them with 90% CIs.
fig_2 = plot_results(df = sc_series, var = "NRG", plot_ci = ci) +
  labs(
    title = "Effect of the IbEx energy CPI",
    subtitle = "Index, 2015=100 - 90% confidence intervals"
  )

### Fig 3. Observed and Synthetic overall CPI series, and difference between them with 90% CIs.
fig_3 = plot_results(df = sc_series, var = "CP00", plot_ci = ci) +
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
fig_B1 = plot_results(df = sc_inflation_rate, var = "NRG") +
  labs(
    title = "Effect of the IbEx energy inflation rate",
    subtitle = "%, year-on-year inflation rate"
    )

### Fig. B2. Observed and synthetic overall inflation rate series, and difference between them.
fig_B2 = plot_results(df = sc_inflation_rate, var = "CP00") +
  labs(
    title = "Effect of the IbEx overall inflation rate",
    subtitle = "%, year-on-year inflation rate"
  )

### Fig. B3. Observed and synthetic overall inflation rate series, and difference between them.
fig_B3 = plot_results(df = sc_inflation_rate, var = "TOT_X_NRG") +
  labs(
    title = "Effect of the IbEx overall inflation rate excluding energy",
    subtitle = "%, year-on-year inflation rate"
  )

### Table B1
table_B1 = get_ate_table(df = sc_inflation_rate, unit = "rate")

### Fig. C1. Observed and Synthetic overall CPI excluding energy series, and difference between them with 90% CIs.
fig_C1 = plot_results(df = sc_series, var = "TOT_X_NRG", plot_ci = ci) +
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

### Save output
if (save_output) {
  if (!dir.exists("04_output")) dir.create("04_output") 
  # Save figues
  log_info("Saving figures in 04_output/")
  figures = as.list(
    c(
      "fig_1", "fig_2", "fig_3", "fig_4", 
     "fig_B1", "fig_B2", "fig_B3",
      "fig_C1", "fig_C2"
      )
    )
    for (f in figures) {
    ggsave(
      filename = paste0(as.character(f), ".png"),
      plot = get(f), 
      path = "04_output/",
      height = 5.5,
      width = 10
    )
  }
  #Save tables
  log_info("Saving tables in 04_output/")
  tables = c(
    "table_A1", 
    "table_B1"
  )
  for (t in tables) {
    write_csv(
      get(t), 
      paste0("04_output/", as.character(t), ".csv")
    )
  }
}