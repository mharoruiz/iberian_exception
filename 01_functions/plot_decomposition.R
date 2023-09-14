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
#'
#' @return ggplot object showing the effect of the IbEx on the overall inflation
#' rate decomposed between energy and non-energy inflation.
#'
plot_decomposition = function(df, whole_var, sub_vars, treated_unit, plot_ci=FALSE) {
  
  # Attach required packages
  library(readr)
  library(ggplot2)
  library(dplyr)
  library(lubridate)

  # Raise errors
  if (length(SUB_VARS) != 2) {
    stop(
      "sub_vars must be a vector of length 2."
      )
  }
  if (length(WHOLE_VAR) != 1 ) {
    "whole_var must of length 1."
  }
  expected_colnames = c(
    "date", "outcome", "gaps", "treated"
  )
  missing_colnames = !(expected_colnames %in% colnames(df))
  if (sum(missing_colnames) != 0) {
    stop(
      sprintf(
        "Variables %s must be columns in provided dataframe.",
        paste(
          expected_colnames[which(missing_colnames)],
          collapse = ", "
        )
      )
    )
  }
  if (plot_ci) {
    expected_colnames_ci = c(
      "upper_ci", "lower_ci"
    )
    missing_colnames_ci = !(expected_colnames_ci %in% colnames(df))
    if (sum(missing_colnames_ci) != 0) {
      stop(
        sprintf(
          "Variables %s must be columns in provided dataframe if plot_ci=TRUE.",
          paste(
            expected_colnames_ci[which(missing_colnames_ci)],
            collapse = ", "
          )
        )
      )
    }
    if (
      sum(is.na(df$upper_ci)) == length(df$upper_ci) |
      sum(is.na(df$lower_ci)) == length(df$lower_ci)
    ) {
      stop(
        "The dataframe provided contains empty lower_ci/upper_ci columns. Set plot_ci=FALSE to suppress this message."
      )
    }
  }
  if (treated_unit != "ES" & treated_unit != "PT") {
    stop(
      sprintf(
        "Supported treated_unit are 'ES' and 'PT'. Got %s.",
        treated_unit
      )
    )
  }
  all_vars = c(whole_var, sub_vars)
  if (sum(!(all_vars %in% unique(df$outcome))) != 0){
    stop(
      sprintf(
        "outcome(s) %s must be present in df$outcome",
        all_vars[which(!(all_vars %in% unique(df$outcome)))]
      )
    )
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
      coicop =
        case_when(
          coicop == "TOT_X_NRG" ~ "CP00xNRG",
          coicop == "NNRG_IGD" ~ "IGDxNRG",
          TRUE ~ coicop
        )
    ) |>
    filter(geo == treated_unit) |>
    mutate(w = values * 0.001) |>
    select(outcome = coicop, w, year = time)
  
  # Raise errors
  if (sum(!(all_vars %in% unique(w_df$outcome))) != 0) {
    stop(
      sprintf(
        "The following outcomes are not supported vars: %s",
        all_vars[which(!(all_vars %in% unique(w_df$outcome)))]
      )
    )
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
  
  # Filter and process SC results
  sc_results = df |>
    filter(
      treated == treated_unit &
      date >= as.Date("2022-06-01") &
      outcome %in% c(whole_var, sub_vars)  
      )|>
    mutate(
      date = as.Date(date),
      year = year(date)
    ) |>
    select(date, outcome, gaps, year)
  
  # Merge SC results and weights datasets
  sc_w = inner_join(sc_results, w_df, by = c("outcome", "year")) |>
    mutate( w_gaps = w * gaps) |>
    select(date, outcome, w_gaps)

  ### Prepare data for plotting
  # Define series names
  series_names = NULL
  for (v in 1:length(sub_vars)) {
    series_names = c(
      series_names, 
      paste0(sub_vars[v], "(w)")
      )
    if (v == 1) {
      last_series_name = paste0(sub_vars[v], "(w)")
    } else {
      last_series_name = paste(
        last_series_name,
        paste0(sub_vars[v], "(w)"),
        sep = "+"
      )
    }
  }
  series_names = c(
    series_names,
    last_series_name
  )
  # Define subplot names
  subplot_names = paste(whole_var, "vs", series_names)
  # Define treatment date
  treatment = as.Date("2022-07-01")
  # Create dataframe with whole var
  whole_var_w = sc_w |>
    filter(outcome == whole_var)
  # Create dataframe for plotting
  d_plot = NULL
  for (v in 1:(length(sub_vars)+1)) {
    if (v == length(sub_vars)+1) {
      subplot= sc_w |>
        filter(outcome != whole_var) |>
        group_by(date) |>
        summarise_at(c("w_gaps"), sum) |>
        ungroup() |>
        mutate(outcome = series_names[v]) |>
        rbind(whole_var_w) |>
        mutate(subplot = subplot_names[v])
    } else {
      subplot = sc_w |>
        filter(outcome ==  sub_vars[v]) |>
        mutate(outcome = series_names[v]) |>
        rbind(whole_var_w) |>
        mutate(subplot = subplot_names[v])
    }
    d_plot = rbind(d_plot, subplot)
  }
  d_plot = d_plot |>
    filter(date >= treatment) |>
    mutate(
      subplot = factor(
        subplot,
        levels = subplot_names
      ),
      outcome = factor(
        outcome,
        levels = c(whole_var, series_names)
      )
    )

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
          aes(x = date, ymax = w_ate_u, ymin = w_ate_l, fill = outcome),
          alpha = .25
        )
      }
    } +
    # Customize intervals shading
    scale_fill_manual(
      values =
        c(
         "black",
         "dodgerblue3",
         "goldenrod",
         "springgreen4"
        )
    ) +
    # Plot lines
    geom_line(aes(x = date, y = w_gaps, color = outcome), linewidth = 1) +
    # Customize line colors
    scale_color_manual(
      values =
        c(
           "black",
           "dodgerblue3",
           "goldenrod",
           "springgreen4"
        )
    ) +
    # Remove axis labels
    labs(x = "", y = "") +
    # Format x-axis labels
    scale_x_date(date_labels = "%b") +
    # Create subplots
    facet_wrap(~subplot) +
    # Customize theme
    theme_minimal(base_size = 15) +
    theme(
      plot.background = element_rect(
        color = "white",
        fill = "white"
      ),
      legend.position="bottom",
      legend.title = element_blank()
    )

  return(plot)
  
}
