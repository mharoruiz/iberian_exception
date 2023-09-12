#'
#' CPI decomposition plots
#'
#' @description This function replicates figure 3 and A3 of Haro-Ruiz, M.,
#' Schult C., and Wunder, C. (2023), which shows the disaggreated effect of the 
#' Iberian exception mechanism between energy and non-energy inflation for Spain 
#' and Portugal.
#'
#' @param df Dataframe returned by estimate_sc().
#' @param treated_unit String indicating the treated unit to plot results for.
#' "ES" to plot results for Spain or "PT" to plot results for Portugal.
#' @param plot_ci Boolean indicating whether to plot confidence intervals.
#'
#' @return ggplot object showing the effect of the IbEx on the overall inflation
#' rate decomposed between energy and non-energy inflation.
#'
plot_decomposition = function(df, treated_unit, vars, plot_ci=FALSE) {
  
  # Attach required packages
  library(readr)
  library(ggplot2)
  library(dplyr)
  library(lubridate)

  # Raise errors
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

  # Filter and process SC results
  sc_results = df |>
    filter(
      treated == treated_unit &
      date >= as.Date("2022-06-01") &
      outcome %in% c("CP00", vars)  
      )|>
    mutate(
      date = as.Date(date),
      year = year(date)
    ) |>
    select(date, outcome, gaps, year)
  # Import CPI weights weights data
  if (file.exists("02_data/cpi_weights.csv")) {
    w_raw = read_csv("02_data/cpi_weights.csv", show_col_types = FALSE)
  } else {
    w_raw = get_eurostat("prc_hicp_inw", time_format = "num")
    log_info("Saving CPI weights data to data/cpi_weights.csv")
    write_csv(w_raw, "02_data/cpi_weights.csv")
  }
  
  # Filter and process weights
  w = w_raw |>
    filter(geo == treated_unit) |>
    mutate(w = values * 0.001) |>
    select(outcome = coicop, w, year = time)
  # Merge SC results and weights datasets
  sc_w = inner_join(sc_results, w, by = c("outcome", "year")) |>
    mutate( w_gaps = w * gaps) |>
    select(date, outcome, w_gaps)

  ### Prepare data for plotting
  
  series_names = c(
    paste0(vars[1], "(w)"),
    paste0(vars[2], "(w)"),
    paste0(vars[1], "(w)+", vars[2], "(w)")
  )
  
  subplot_names = paste("CP00 vs", series_names)

  # Define treatment date
  treatment = as.Date("2022-07-01")
  # Create dataframe with only var1
  sc_w_var1 = sc_w |>
    filter(outcome == "CP00")
  # Create dataframe with only var2 and var1
  subplot_1 = sc_w |>
    filter(outcome ==  vars[1]) |>
    mutate(outcome = series_names[1]) |>
    rbind(sc_w_var1) |>
    mutate(subplot = subplot_names[1])
  # Create dataframe with only var3 and merge with var1
  subplot_2 = sc_w |>
    filter(outcome == vars[2]) |>
    mutate(outcome = series_names[2]) |>
    rbind(sc_w_var1) |>
    mutate(subplot = subplot_names[2])
  # Create dataframe with only var2+var3 and merge with var1
  subplot_3 = sc_w |>
    filter(outcome != "CP00") |>
    group_by(date) |>
    summarise_at(c("w_gaps"), sum) |>
    ungroup() |>
    mutate(outcome = series_names[3]) |>
    rbind(sc_w_var1) |>
    mutate(subplot = subplot_names[3])
  # Merge all individual dataframes
  d_plot = rbind(subplot_1, subplot_2, subplot_3) |>
    filter(date >= treatment) |>
    mutate(
      subplot = factor(
        subplot,
        levels = subplot_names
      ),
      outcome = factor(
        outcome,
        levels = c("CP00", series_names)
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
