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
sc_series_explo = estimate_sc(
  outcomes = INPUT_VARS,
  T0s = PRE_TREATMENT_PERIODS,
  precision = PRC_STEP,
  compute_ci = CONFIDENCE_INTERVALS,
  save_csv = SAVE_RESULTS
)

### Plot results 

# Observed and synthetic goods-only CPI series, and difference between them.
fig_gd = plot_results(
  df = sc_series_explo, 
  var = INPUT_VARS[1], 
  plot_ci = CONFIDENCE_INTERVALS
  ) +
  labs(
    title = "Effect of the IbEx on goods-only CPI",
    subtitle = "Index, 2015=100 - 90% confidence intervals"
  )
# Observed and synthetic services-only CPI series, and difference between them.
fig_serv = plot_results(
  df = sc_series_explo, 
  var = INPUT_VARS[2], 
  plot_ci = CONFIDENCE_INTERVALS
  ) +
  labs(
    title = "Effect of the IbEx on services-only CPI",
    subtitle = "Index, 2015=100 - 90% confidence intervals"
  )

# Decomposition of the effect on Spain’s inflation rate.
fig_decomp_es = plot_decomposition(
  df = sc_series_explo, 
  whole_var = WHOLE_VAR,
  sub_vars = SUB_VARS,
  treated_unit = "ES"
  ) +
  labs(
    title = "Decomposition of the effect of the IbEx on Spain's CP00"
  )
# Decomposition of the effect on Portugal’s inflation rate.
fig_decomp_pt = plot_decomposition(
  df = sc_series_explo, 
  whole_var = WHOLE_VAR,
  sub_vars = SUB_VARS,
  treated_unit = "PT"
  ) +
  labs(
    title = "Decomposition of the effect of the IbEx on Portugals's CP00"
  )

### Save output
if (SAVE_OUTPUT) {
  if (!dir.exists("05_exploration")) dir.create("05_exploration") 
  # Save figures
  log_info("Saving figures in 05_exploration/")
  figures = as.list(
    c(
      "fig_gd", "fig_serv",
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

### Challenge #1
# Create a dataframe that summarizes the results for each input variable and 
# treated country over the entire post-treatment period. The dataframe should 
# show estimates in absolute and percentage terms, as well as the change in 
# inflation rate.

# Begin with using function inference_sc(), which returns average effects on the
# treated (ATTs) in absolute terms, as well as standard errors (SEs) and 90% 
# confidence intervals (CIs).

sc_infer_explo = inference_sc(
  outcomes = INPUT_VARS,
  T0s = PRE_TREATMENT_PERIODS,
  method = "ttest",
  save_csv = SAVE_RESULTS
)

# To improve readability of the results, your output should follow the same 
# wide format as dataframe att_abs, which is defined in line 156 of main.R.

# Now, compute estimates in percentage terms. This can be done by dividing the 
# ATTs in sc_infer_explo by the gap included in the sc_series_explo dataframe. 
# Make sure you take the post-treatment average of gap before dividing. Make 
# the resulting dataframe is in wide format, such as att_pct defined in line 182
# of main.R. 

# Next, compute estimates in terms of change in the inflation rate. To do so, 
# you first have to compute the observed and synthetic inflation rate series as
# shown below. Note that in the pre-treatment period, the observed and synthetic
# series are the same, for which they only differ in the post-treatment period.

sc_inflation_explo = sc_series_explo |>
  filter(outcome %in% INPUT_VARS) |>
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

# use sc_infaltion_explo to obtain the ATT in terms of inflation rate. Make sure
# the format of the resulting dataframe is the same as att_rate, defined in line
# 201 of main.R. 

# At this point, you should have three dataframes analogous to att_abs, att_pct
# and att_rate defined in main.R. The final step is to combine all three into a
# single dataframe to display the results in terms of three different units. The
# resulting table should be analogous to table_A1, defined in line 209 of main.R
