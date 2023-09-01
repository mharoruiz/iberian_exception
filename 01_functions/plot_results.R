#'
#' Effect plots
#'
#' @description This function replicates figures 1, 2, 3, B1, B2, B3, and C1 in
#' Haro-Ruiz, M., Schult C., and Wunder, C. (2023), which shows the effects of 
#' the Iberian exception mechanism on different price outcomes for Spain and
#' Portugal.
#'
#' @param df A dataframe returned by estimate_sc().
#' @param var A string with the name of an outcome variable in df.
#' @param plot_ci logical indicating whether to plot confidence intervals.
#'
#' @return A plot showing the effects of the Iberian exception mechanism on a
#' given outcome for Spain and Portugal.
#'
plot_results = function(df, var, plot_ci=FALSE) {
  
  # Install/attach required packages
  require(ggplot2)
  require(dplyr)

  # Raise errors
  expected_colnames = c(
    "date", "outcome", "obs", "synth", "gaps", "treated"
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
  if (!(var %in% unique(df$outcome))) {
    stop(
      sprintf(
        "var must be a variable included in df$outcome Got %s.",
        var
      )
    )
  }

  ### Prepare data for plotting

  # Create dataframe with observed values only
  d_obs = df |>
    filter(outcome == var) |>
    mutate(
      date = as.Date(date),
      value = obs,
      upper_ci = NA,
      lower_ci = NA,
      series = "Observed",
      subplot = var,
      country =
        case_when(
          treated == "ES" ~ "Spain",
          TRUE ~ "Portugal"
        ),
      color = treated
    ) |>
    select(date, value, upper_ci, lower_ci, series, subplot, country, color)
  # Create dataframe with synthetic values only
  d_synth = df |>
    filter(outcome == var) |>
    mutate(
      date = as.Date(date),
      value = synth,
      upper_ci = NA,
      lower_ci = NA,
      series = "Synthetic",
      subplot = var,
      country =
        case_when(
          treated == "ES" ~ "Spain",
          TRUE ~ "Portugal"
        ),
      color =
        case_when(
          treated == "ES" ~ "SC_es",
          TRUE ~ "SC_pt"
        )
    ) |>
    select(date, value, upper_ci, lower_ci, series, subplot, country, color)
  # Create dataframe with gap values only
  if (plot_ci) {
    d_gaps = df |>
      filter(outcome == var) |>
      mutate(
        date = as.Date(date),
        value = gaps,
        series = "Gaps",
        subplot = "Gaps",
        country =
          case_when(
            treated == "ES" ~ "Spain",
            TRUE ~ "Portugal"
          ),
        color = "Gap"
      ) |>
      select(date, value, upper_ci, lower_ci, series, subplot, country, color)
  } else {
    d_gaps = df |>
      filter(outcome == var) |>
      mutate(
        date = as.Date(date),
        value = gaps,
        upper_ci = NA,
        lower_ci = NA,
        series = "Gaps",
        subplot = "Gaps",
        country =
          case_when(
            treated == "ES" ~ "Spain",
            TRUE ~ "Portugal"
          ),
        color = "Gap"
      ) |>
      select(date, value, upper_ci, lower_ci, series, subplot, country, color)
  }
  # Merge all dataframes
  d_plot = rbind(d_obs, d_synth, d_gaps) |>
    mutate(
      color = factor(
        color, 
        levels = c("ES", "SC_es", "PT", "SC_pt", "Gap"))
    )

  ### Plot

  # Define treatment date
  treatment = as.Date("2022-06-01")
  # Define horizontal lines in gap plots
  hlines = data.frame(
    value = rep(0, 2),
    subplot = rep("Gaps", 2)
  )
  # Create plot
  plot = d_plot |>
    ggplot() +
    # Vertical line to indicate treatment
    geom_vline(xintercept = treatment, linetype = "dashed") +
    # Horizontal line at 0 in gap plots
    geom_hline(data = hlines, aes(yintercept = value)) +
    # 90% confidence intervals
    {
      if (plot_ci == TRUE) {
        geom_ribbon(
          data = subset(d_plot, date >= treatment & series == "Gaps"),
          aes(x = date, ymin = lower_ci, ymax = upper_ci),
          fill = "aquamarine4", alpha = .5
        )
      }
    } +
    # Plot lines
    geom_line(aes(x = date, y = value, color = color), linewidth = .75) +
    # Customize line colors
    scale_color_manual(
      values = c(
        "ES" = "indianred1",
        "SC_es" = "slateblue4",
        "PT" = "springgreen3",
        "SC_pt" = "slateblue4",
        "Gap" = "grey25"
      )
    ) +
    # Remove axis labels
    labs(x = "", y = "") +
    # Create and arrange subplots
    facet_grid(
      factor(subplot, levels = c(var, "Gaps"))
      ~ factor(country, levels = c("Spain", "Portugal")),
      scales = "free_y",
      # independent="y",
      switch = "y"
    ) +
    # Customize theme
    theme_minimal(base_size = 15) +
    theme(
      strip.placement="out",
      legend.title = element_blank(),
      plot.background = element_rect(
        color = "white",
        fill = "white"
        )
      )

  return(plot)
  
}
