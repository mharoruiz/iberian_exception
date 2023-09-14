#'
#' Average treatment effects table
#'
#' @description This function replicates table A1 of Haro-Ruiz, M., Schult C.,
#' and Wunder, C. (2023), which outlines the estimated treatment effects of the 
#' Iberian exception mechanism on different price indicators for Spain and 
#' Portugal, as well as the p-values of each estimate.
#'
#' @param df Dataframe returned by estimate_sc().
#' @param T1_breaks Object of class "Date" or a vector containing "Date"
#' class objects. Break points will be used to divide the post-treatment
#' period into sub-periods and compute average treatment effects. The post-
#' treatment period ranges from  07/2022 to  06/2023 so T1_breaks must be
#' within this range. For example, setting T1_breaks=as.Date("2022-12-01")
#' will compute treatment effects for the sub-periods 07/2022-12/2022 
#' and 01/2023-06/2023, as well as the full post-treatment period.
#' @param unit String indicating unit of "obs", and "gaps" columns. Accepted 
#' units are "idx",  for the CPI index and "rate", for th ∫e year-on-year rate of 
#' inflation.
#'
#' @return Dataframe with average treatment effects across periods for
#' different outcomes and treated units. If unit="idx", the resulting treatment 
#' effects will be expressed in percentage, whereas if unit="rate", the
#' treatment effects will be expressed in percentage points.
#'
get_ate_table = function(df, T1_breaks=NULL, unit) {
  
  # Attach required packages
  library(lubridate)
  library(tidyr)
  library(dplyr)
  
  # Define treatment and end date
  treatment_date = as.Date("2022-06-01")
  end_date = as.Date("2023-06-01")

  # Raise errors
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
  if (!(is.null(T1_breaks))) {
    if (!(inherits(T1_breaks, "Date"))) {
      stop(
        "T1_breaks must be objects of class 'Date'."
      )
    }
    for (date in T1_breaks) {
      date = as.Date(date, origin = "1970-01-01")
      if (day(date) != 1) {
        new_date = paste(
          year(date),
          ifelse(nchar(month(date)) == 1, paste0(0, month(date)), month(date)),
          "01",
          sep = "-"
        )
        T1_breaks[which(T1_breaks == date)] = as.Date(new_date)
        warning(
          sprintf(
            "Changing %s to %s.\n",
            date, new_date
          ),
          immediate. = TRUE
        )
      }
    }
    if (
      sum(!(T1_breaks %in% seq(treatment_date, end_date, by = "month"))) != 0
    ) {
      stop(
        sprintf(
          "T1_breaks must be whitin %s and %s. Got %s",
          treatment_date, end_date,
          paste(
            T1_breaks[
              which(
                !(T1_breaks %in% seq(treatment_date, end_date, by = "month"))
              )
            ],
            collapse = ", "
          )
        )
      )
    }
  }
  supported_units = c("idx", "rate")
  if (!(unit %in% supported_units)) {
    stop(
      sprintf(
        "%s is not a supported unit. Supported units are 'idx' and 'rate'",
        unit
      )
    )
  }
  
  if (unit == "idx") { 
    # Preprocess ATE table with full post-treatment period
    ate_full = df |>
      filter(
        date > treatment_date
      ) |>
      mutate(
        from = min(date),
        to = max(date)
      ) |>
      group_by(outcome, treated, from, to) |>
      mutate(
        ate = gaps / obs * 100,
        ate_upper = upper_ci / obs * 100,
        ate_lower = lower_ci / obs * 100
      ) |>
      summarise_at(c("ate", "ate_upper", "ate_lower"), mean) |>
      ungroup()
    
    # Define dates to break T1 into sub-periods
    date_breaks = c(treatment_date, T1_breaks, end_date)
    # One iteration for each sub-period within T1
    ate_sub = NULL
    for (sp in 1:(length(date_breaks) - 1)) {
      # Remove observations in T1 outside of this sub-period
      current_ate = df |>
        filter(
          date > date_breaks[sp] & date <= date_breaks[(sp + 1)]
        ) |>
        mutate(
          from = min(date),
          to = max(date)
        ) |>
        group_by(outcome, treated, from, to) |>
        mutate(
          ate = gaps / obs * 100,
          ate_upper = upper_ci / obs * 100,
          ate_lower = lower_ci / obs * 100
        ) |>
        summarise_at(c("ate", "ate_upper", "ate_lower"), mean) |>
        ungroup()
      
      ate_sub = rbind(ate_sub, current_ate)
    }
    # Concatenate both tables
    ate_table_raw = rbind(ate_full, ate_sub) |>
      arrange(outcome, treated, from, desc(to))
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
    ate_table = inner_join(t1, t2, by = c("outcome", "from", "to")) |>
      inner_join(t3, by = c("outcome", "from", "to")) |>
      select(
        outcome, from, to,
        ate_ES, ate_lower_ES, ate_upper_ES,
        ate_PT, ate_lower_PT, ate_upper_PT
      )
  } else if (unit == "rate") {
    # Preprocess ATE table with full post-treatment period
    ate_full = df |>
      filter(
        date >= treatment_date
      ) |>
      mutate(
        from = min(date),
        to = max(date)
      ) |>
      rename(ate = gaps) |>
      group_by(outcome, treated, from, to) |>
      summarise_at(c("ate"), mean) |>
      ungroup() 
    
    # Define dates to break T1 into sub-periods
    date_breaks = c(treatment_date, T1_breaks, end_date)
    # One iteration for each sub-period within T1
    ate_sub = NULL
    for (sp in 1:(length(date_breaks) - 1)) {
      # Remove observations in T1 outside of this sub-period
      current_ate = df |>
        filter(
          date > date_breaks[sp] & date <= date_breaks[(sp + 1)]
        ) |>
        mutate(
          from = min(date),
          to = max(date)
        ) |>
        rename(ate = gaps ) |>
        group_by(outcome, treated, from, to) |>
        summarise_at(c("ate"), mean) |>
        ungroup()
      
      ate_sub = rbind(ate_sub, current_ate)
    }
    # Concatenate both tables
    ate_table_raw = rbind(ate_full, ate_sub) |>
      arrange(outcome, treated, from, desc(to))
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
