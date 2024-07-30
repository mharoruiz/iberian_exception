#'
#' CPI decomposition plots
#'
#' @description This function replicates figure 3 and A3 of Haro-Ruiz, M.,
#' Schult C., and Wunder, C. (2023), which shows the disaggreated effect of the 
#' Iberian exception mechanism between energy and non-energy inflation for Spain 
#' and Portugal.
#'
#' @param df Dataframe returned by estimate_sc(). The values in the outcome 
#' column must be "CP00" as well as a series of disaggregated CPI series whose 
#' assigned weight add up to 1.
#' @param sub_vars Vector containing the string names of 2 CPI aggregation 
#' codes included in the outcome column of the provided df. The 2 indices in 
#' sub_vars must add up to the index in whole_var.
#' @param whole_var String with the name of 1 CPI aggregation code included in 
#' the outcome column of the provided df. This index must result from the 
#' combination of the two indices in sub_vars.
#' @param treated_unit String indicating the treated unit to plot results for.
#' "ES" to plot results for Spain or "PT" to plot results for Portugal.
#' @param plot_ci Boolean indicating whether to plot confidence intervals. 
#' Default: FALSE
#'
#' @return ggplot object showing the effect of the IbEx on the overall inflation
#' rate decomposed between energy and non-energy inflation.
#'
plot_decomposition = function(df, sub_vars, whole_var, treated_unit, plot_ci = FALSE) {
  
  # Attach required packages
  library(readr)
  library(ggplot2)
  library(dplyr)
  library(lubridate)
  library(scales)

  # Raise errors
  if (length(SUB_VARS) != 2) {
    stop("sub_vars must be a vector of length 2.")
  }
  if (length(WHOLE_VAR) != 1 ) {
    "whole_var must of length 1."
  }
  expected_colnames = c("date", "outcome", "diff", "treated")
  missing_colnames = !(expected_colnames %in% colnames(df))
  if (sum(missing_colnames) != 0) {
    stop(sprintf("Variables %s must be columns in provided dataframe.",
        paste(expected_colnames[which(missing_colnames)], collapse = ", ")) )
  }
  if (plot_ci) {
    expected_colnames_ci = c("upper_ci", "lower_ci")
    missing_colnames_ci = !(expected_colnames_ci %in% colnames(df))
    if (sum(missing_colnames_ci) != 0) {
      stop(
        sprintf(
          "Variables %s must be columns in provided dataframe if plot_ci=TRUE.",
          paste(expected_colnames_ci[which(missing_colnames_ci)], collapse = ", ")
          ) 
        )
    }
    if (sum(is.na(df$upper_ci)) == length(df$upper_ci) |
        sum(is.na(df$lower_ci)) == length(df$lower_ci) ) {
      stop("The dataframe provided contains empty lower_ci/upper_ci columns. Set plot_ci=FALSE to suppress this message.")
    }
  }
  if (treated_unit != "ES" & treated_unit != "PT") {
    stop(sprintf("Supported treated_unit are 'ES' and 'PT'. Got %s.",
                 treated_unit) )
  }
  all_vars = c(whole_var, sub_vars)
  if (sum(!(all_vars %in% unique(df$outcome))) != 0){
    stop(sprintf("outcome(s) %s must be present in df$outcome",
                 all_vars[which(!(all_vars %in% unique(df$outcome)))]) )
  }
  
  # Import CPI weights weights data
  if (file.exists("02_data/cpi_weights.csv")) {
    w_raw = read_csv("02_data/cpi_weights.csv", show_col_types = FALSE)
  } else {
    w_raw = get_eurostat("prc_hicp_inw", time_format = "num")
    log_info("Saving CPI weights data to data/cpi_weights.csv")
    write_csv(w_raw, "02_data/cpi_weights.csv")
  }
  # Filter and process weights
  w_df = w_raw |>
    mutate(
      coicop = case_when(coicop == "TOT_X_NRG" ~ "xNRG",
                         coicop == "NNRG_IGD" ~ "IGDxNRG",
                         TRUE ~ coicop) ) |>
    filter(geo == treated_unit) |>
    mutate(w = values * 0.001) |>
    select(outcome = coicop, w, year = TIME_PERIOD)
  
  # Raise errors
  if (sum(!(all_vars %in% unique(w_df$outcome))) != 0) {
    stop(sprintf("The following outcomes are not supported vars: %s",
                 all_vars[which(!(all_vars %in% unique(w_df$outcome)))]) )
  }
  w_sub_vars = w_df |>
    filter(year == 2023 & outcome %in% sub_vars)
  w_whole_var = w_df |>
    filter(year == 2023 & outcome %in% whole_var)
  if (round(sum(w_sub_vars$w), 3) != round(w_whole_var$w, 3)) {
    stop(
      sprintf(
        "The sum of the weights assinged to 'sub_vars'and the weight assinged to 'whole_var' must be equal. 
        - Sum of sub_vars weights: %s
        - whole_var weight: %s",
        sum(w_sub_vars$w),
        w_whole_var$w
      )
    )
  }
  
  
  if (plot_ci) {
    # Filter and process SC results
    sc_results = df |>
      filter(treated == treated_unit & date >= as.Date("2022-06-01") & 
               outcome %in% c(whole_var, sub_vars) )|>
      mutate(date = as.Date(date),
             year = year(date) ) |>
      select(date, outcome, diff, upper_ci, lower_ci, year)
    # Merge SC results and weights datasets
    sc_w = inner_join(sc_results, w_df, by = c("outcome", "year")) |>
      mutate(w_diff = w * diff, w_diff_u = w * upper_ci, 
             w_diff_l = w * lower_ci) |>
      select(date, outcome, w_diff, w_diff_u, w_diff_l)
    
  } else {
    # Filter and process SC results
    sc_results = df |>
      filter(treated == treated_unit & date >= as.Date("2022-06-01") &
               outcome %in% c(whole_var, sub_vars) )|>
      mutate(date = as.Date(date), year = year(date)) |>
      select(date, outcome, diff, year)
    # Merge SC results and weights datasets
    sc_w = inner_join(sc_results, w_df, by = c("outcome", "year")) |>
      mutate(w_diff = w * diff) |>
      select(date, outcome, w_diff)
  }
  
  ### Prepare data for plotting
  # Define treatment date
  treatment = as.Date("2022-07-01")
  # Create dataframe with whole var
  d_plot = sc_w |>
    filter(date >= treatment) |>
    mutate(
      outcome = case_when(
        outcome == sub_vars[1] ~ paste0("w*Delta*", sub_vars[1]),
        outcome == sub_vars[2] ~ paste0("(1-w)*Delta*", sub_vars[2]),
        outcome == whole_var ~ paste0("Delta*", whole_var)
        ),
      outcome = factor(outcome, levels = c(paste0("w*Delta*", sub_vars[1]),
                                           paste0("(1-w)*Delta*", sub_vars[2]),
                                           paste0("Delta*", whole_var))) )
      
  ### Plot
  
  # Create plot
  plot = d_plot |>
    ggplot() +
    # Horizontal line at 0
    geom_hline(yintercept = 0) +
    # Vertical line to indicate treatment
    geom_vline(xintercept = treatment, linetype = "dashed") +
    # 90% confidence intervals
    {
      if (plot_ci == TRUE) {
        geom_ribbon(
          aes(x = date, ymax = w_diff_u, ymin = w_diff_l, fill = outcome),
          alpha = .25
        )
      }
    } +
    # Customize intervals shading
    scale_fill_manual(values = c("dodgerblue3","goldenrod","black")) +
    # Plot lines
    geom_line(aes(x = date, y = w_diff, color = outcome), linewidth = 1) +
    # Customize line colors
    scale_color_manual(values = c("dodgerblue3","goldenrod","black"),
                       labels = parse_format() ) +
    # Remove axis labels
    labs(x = "", y = paste("Effect of the IbEx on", whole_var)) +
    # Format x-axis labels
    scale_x_date(date_labels = "%b") +
    # Customize theme
    theme_minimal(base_size = 15) +
    theme(plot.background = element_rect(color = "white", fill = "white"),
          legend.position="bottom",
          legend.title = element_blank() )

  return(plot)
  
}
