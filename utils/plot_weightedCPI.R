#'
#' Weighted CPI plots: Twelve Months After the Iberian Exception.
#'
#' @description This function replicates figure 3 and A3 of Haro-Ruiz, M. and
#' Schult C. (2023), which shows the dissagreated effect of the Iberian
#' exception mechanism between energy and non-energy inflation for Spain and
#' Portugal.
#'
#' @param df A dataframe returned by estimate_sc().
#' @param treated_unit "ES" to plot results for Spain or "PT" to plot results
#' for Portugal.
#'
#' @return A plot showing the dissagreated effect of the Iberian exception
#' mechanism between energy and non-energy inflation for a given treated unit.
#'
plot_weightedCPI = function(df, treated_unit, plot_ci = FALSE) {
  # Attach required packages
  library(readr)
  library(dplyr)
  library(lubridate)
  library(ggplot2)

  # Raise errors
  expected_colnames = c(
    "date", "outcome", "obs", "gaps", "upper_ci", "lower_ci", "treated"
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
  if (
    plot_ci &
      (
        sum(is.na(df$upper_ci)) == length(df$upper_ci) |
          sum(is.na(df$lower_ci)) == length(df$lower_ci)
      )
  ) {
    stop(
      "The dataframe provided contains empty lower_ci/upper_ci columns. Set plot_ci=FALSE to suppress this message."
    )
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
    filter(treated == treated_unit &
      date >= as.Date("2022-06-01") &
      (
        outcome == "CP00" |
          outcome == "NRG" |
          outcome == "TOT_X_NRG"
      )) |>
    mutate(
      date = as.Date(date),
      ate = gaps / obs * 100,
      ate_u = upper_ci / obs * 100,
      ate_l = lower_ci / obs * 100,
      time = year(date)
    ) |>
    select(date, outcome, ate, ate_u, ate_l, time)
  # Import Eurostat CPI weights dataframe
  w_raw = read_csv("data/hicp_weights.csv", show_col_types = FALSE)
  # Filter and process weights
  w = w_raw |>
    filter(geo == treated_unit) |>
    mutate(w = values * 0.001) |>
    select(outcome = coicop, w, time)
  # Merge SC results and weights datasets
  sc_w = inner_join(sc_results, w, by = c("outcome", "time")) |>
    mutate(
      w_ate = w * ate,
      w_ate_u = w * ate_u,
      w_ate_l = w * ate_l
    ) |>
    select(date, outcome, w_ate, w_ate_u, w_ate_l)

  ### Prepare data for plotting

  # Define treatment date
  treatment = as.Date("2022-07-01")
  # Create dataframe with only CP00
  sc_w_cp00 = sc_w |>
    filter(outcome == "CP00")
  # Create dataframe with only NRG and merge with CP00
  subplot_1 = sc_w |>
    filter(outcome == "NRG") |>
    mutate(outcome = "NRG(w)") |>
    rbind(sc_w_cp00) |>
    mutate(subplot = "CP00 vs NRG(w)")
  # Create dataframe with only CP00xNRG and merge with CP00
  subplot_2 = sc_w |>
    filter(outcome == "TOT_X_NRG") |>
    mutate(outcome = "CP00xNRG(w)") |>
    rbind(sc_w_cp00) |>
    mutate(subplot = "CP00 vs CP00xNRG(w)")
  # Create dataframe with only NRG+CP00xNRG and merge with CP00
  subplot_3 = sc_w |>
    filter(outcome != "CP00") |>
    group_by(date) |>
    summarise_at(c("w_ate", "w_ate_u", "w_ate_l"), sum) |>
    mutate(outcome = "NRG(w)+CP00xNRG(w)") |>
    rbind(sc_w_cp00) |>
    mutate(subplot = "CP00 vs NRG(w)+CP00xNRG(w)")
  # Merge all individual dataframes
  d_plot = rbind(subplot_1, subplot_2, subplot_3) |>
    filter(date >= treatment) |>
    mutate(
      subplot = factor(
        subplot,
        levels =
          c(
            "CP00 vs NRG(w)",
            "CP00 vs CP00xNRG(w)",
            "CP00 vs NRG(w)+CP00xNRG(w)"
          )
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
          "CP00" = "black",
          "NRG(w)" = "dodgerblue3",
          "CP00xNRG(w)" = "goldenrod",
          "NRG(w)+CP00xNRG(w)" = "springgreen4"
        )
    ) +
    # Plot lines
    geom_line(aes(x = date, y = w_ate, color = outcome), linewidth = 1) +
    # Customize line colors
    scale_color_manual(
      values =
        c(
          "CP00" = "black",
          "NRG(w)" = "dodgerblue3",
          "CP00xNRG(w)" = "goldenrod",
          "NRG(w)+CP00xNRG(w)" = "springgreen4"
        )
    ) +
    # Remove axis labels
    labs(x = "", y = "") +
    # Format x-axis labels
    scale_x_date(date_labels = "%b") +
    # Create subplots
    facet_wrap(~subplot) +
    # Customize theme
    theme_minimal(base_size = 15)

  return(plot)
}
