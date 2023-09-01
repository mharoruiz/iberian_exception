#'
#' Average treatment effects table
#'
#' @description This function replicates table A1 of Haro-Ruiz, M., Shcult C.,
#' and Wunder, C. (2023), which outlines the estimated treatment effects of the 
#' Iberian exception mechanism on different price indicators for Spain and 
#' Portugal, as well as the p-values of each estimate.
#'
#' @param df A dataframe returned by estimate_sc().
#' @param unit String indicating unit of "obs", and "gaps" columns. Accepted 
#' units are "idx",  for the CPI index and "rate", for the year-on-year rate of 
#' inflation.
#'
#' @return A dataframe with average treatment effects across periods for
#' different outcomes and treated units.
#'
get_ate_table = function(df, unit) {
  
  # Attach required packages
  suppressPackageStartupMessages(require(tidyverse))

  # Raise errors
  supported_units = c("idx", "rate")
  if (!(unit %in% supported_units)) {
    stop(
      sprintf(
        "%s is not a supported unit. Supported units are 'idx' and 'rate'",
        unit
      )
    )
  }
  expected_colnames = c(
    "date", "outcome", "obs", "gaps", "treated"
  )
  if (unit == "idx") {
    expected_colnames_ci = c(
      "upper_ci", "lower_ci"
    )
    missing_colnames_ci = !(expected_colnames_ci %in% colnames(df))
    if (sum(missing_colnames_ci) != 0) {
      stop(
        sprintf(
          "Variables %s must be columns in provided dataframe if include_ci=TRUE.",
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
        "The dataframe provided contains empty lower_ci/upper_ci columns. Set incude_ci=FALSE to suppress this message."
      )
    }
  }
  
  if (unit == "idx") { 
    # Preprocess ATE table with full post-treatment period
    ate_12 = df |>
      filter(
        date >= as.Date("2022-07-01")
      ) |>
      mutate(period = "07/2022 - 06/2023") |>
      group_by(outcome, treated, period) |>
      mutate(
        ate = gaps / obs * 100,
        ate_upper = upper_ci / obs * 100,
        ate_lower = lower_ci / obs * 100
      ) |>
      summarise_at(c("ate", "ate_upper", "ate_lower"), mean) |>
      ungroup()
    # Preprocess ATE table with two sub-periods in the post-treatment
    ate_6_6 = df |>
      filter(
        date >= as.Date("2022-07-01")
      ) |>
      mutate(
        period =
          case_when(
            year(date) == 2022 ~ "07/2022 - 12/2022",
            year(date) == 2023 ~ "01/2023 - 06/2023"
          )
      ) |>
      group_by(outcome, treated, period) |>
      mutate(
        ate = gaps / obs * 100,
        ate_upper = upper_ci / obs * 100,
        ate_lower = lower_ci / obs * 100
      ) |>
      summarise_at(c("ate", "ate_upper", "ate_lower"), mean) |>
      ungroup()
    # Concatenate both tables
    ate_table_raw = rbind(ate_12, ate_6_6) |>
      arrange(outcome, treated, period)
    # Reformat table
    t1 = ate_table_raw |>
      select(-c(ate_upper, ate_lower)) |>
      pivot_wider(
        names_from = treated,
        values_from = ate,
        names_prefix = "ate_"
      )
    t2 = ate_table_raw |>
      select(-c(ate, ate_lower)) |>
      pivot_wider(
        names_from = treated,
        values_from = ate_upper,
        names_prefix = "ate_upper_"
      )
    t3 = ate_table_raw |>
      select(-c(ate, ate_upper)) |>
      pivot_wider(
        names_from = treated,
        values_from = ate_lower,
        names_prefix = "ate_lower_"
      )
    ate_table = inner_join(t1, t2, by = c("outcome", "period")) |>
      inner_join(t3, by = c("outcome", "period")) |>
      select(
        outcome, period,
        ate_ES, ate_lower_ES, ate_upper_ES,
        ate_PT, ate_lower_PT, ate_upper_PT
      )
  } else if (unit == "rate") {
    # Preprocess ATE table with full post-treatment period
    ate_12 = df |>
      filter(
        date >= as.Date("2022-07-01")
      ) |>
      mutate(period = "07/2022 - 06/2023") |>
      rename(ate = gaps) |>
      group_by(outcome, treated, period) |>
      summarise_at(c("ate"), mean) |>
      ungroup() 
    # Preprocess ATE table with two sub-periods in the post-treatment
    ate_6_6 = df |>
      filter(
        date >= as.Date("2022-07-01")
      ) |>
      mutate(
        period =
          case_when(
            year(date) == 2022 ~ "07/2022 - 12/2022",
            year(date) == 2023 ~ "01/2023 - 06/2023"
          )
      ) |>
      rename(ate = gaps ) |>
      group_by(outcome, treated, period) |>
      summarise_at(c("ate"), mean) |>
      ungroup()
    # Concatenate both tables
    ate_table_raw = rbind(ate_12, ate_6_6) |>
      arrange(outcome, treated, period)
    # Format table
    ate_table = pivot_wider(
      ate_table_raw,
      names_from = treated, 
      values_from = ate,
      names_prefix = "ate_"
      )
  } else {
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

  return(ate_table)
  
}
