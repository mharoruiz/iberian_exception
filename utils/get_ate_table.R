#'
#' ATE table: Twelve Months After the Iberian Exception.
#'
#' @description This function replicates table 1 of Haro-Ruiz, M. and Schult C.
#' (2023), which outlines the estimated ATEs of the Iberian exception mechanism
#' on different price indicators for Spain and Portugal.
#'
#' @param df A dataframe returned by estimate_sc().
#'
#' @return A dataframe with average treatment effects across periods for
#' different outcomes and treated units.
#'
get_ate_table = function(df) {
  # Attach required packages
  library(dplyr)
  library(tidyr)
  library(lubridate)

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

  # Preprocess ATE table with full post-treatment period
  ate_12 = df |>
    filter(
      date >= as.Date("2022-07-01")
    ) |>
    mutate(period = "07/2022-06/2023") |>
    group_by(outcome, treated, period) |>
    mutate(
      ate = gaps / obs * 100,
      ate_u = upper_ci / obs * 100,
      ate_l = lower_ci / obs * 100
    ) |>
    summarise_at(c("ate", "ate_u", "ate_l"), mean) |>
    ungroup()
  # Preprocess ATE table with two sub-periods in the post-treatment
  ate_6_6 = df |>
    filter(
      date >= as.Date("2022-07-01")
    ) |>
    mutate(
      period =
        case_when(
          year(date) == 2022 ~ "07/2022-12/2022",
          year(date) == 2023 ~ "01/2023-06/2022"
        )
    ) |>
    group_by(outcome, treated, period) |>
    mutate(
      ate = gaps / obs * 100,
      ate_u = upper_ci / obs * 100,
      ate_l = lower_ci / obs * 100
    ) |>
    summarise_at(c("ate", "ate_u", "ate_l"), mean) |>
    ungroup()
  # Concatenate both tables
  ate_table_raw = rbind(ate_12, ate_6_6) |>
    arrange(outcome, treated, period)
  # Reformat table
  t1 = ate_table_raw |>
    select(-c(ate_u, ate_l)) |>
    pivot_wider(
      names_from = treated,
      values_from = ate,
      names_prefix = "ate_"
    )
  t2 = ate_table_raw |>
    select(-c(ate, ate_l)) |>
    pivot_wider(
      names_from = treated,
      values_from = ate_u,
      names_prefix = "ate_u_"
    )
  t3 = ate_table_raw |>
    select(-c(ate, ate_u)) |>
    pivot_wider(
      names_from = treated,
      values_from = ate_l,
      names_prefix = "ate_l_"
    )
  ate_table = inner_join(t1, t2, by = c("outcome", "period")) |>
    inner_join(t3, by = c("outcome", "period")) |>
    select(
      outcome, period,
      ate_ES, ate_l_ES, ate_u_ES,
      ate_PT, ate_l_PT, ate_u_PT
    )

  return(ate_table)
}
