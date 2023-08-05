#'
#' Estimation: Twelve Months After the Iberian exception.
#'
#' @description This function reproduces the results from Haro-Ruiz & Schult
#' (2023), which estimates the effects of the Iberian Exception mechanism on
#' a different price outcomes.
#'
#' The effect of the intervention is estimated via synthetic controls for
#' Spain and Portugal given outcome variables and length of pre-treatment
#' periods.
#'
#' @param outcomes A list of outcomes to compute synthetic controls for
#' @param T0s  A list of sizes of pre-treatment periods. Must be same length
#' as outcomes.
#' @param precision A float between 0 and 1 which defines the step of the grid-
#' search space to find confidence intervals.
#' @param compute_ci logical to compute 90 %confidence intervals or not. If
#' set to TRUE, run-time will increase significantly. Default: FALSE.
#' @param save_csv logical to save results as csv file. The name of the file
#' will be set depending on the precision. For example, setting precision=.01
#' will return a file named sc_trends_01.csv. Default: TRUE.
#'
#' @return A dataframe with synthetic and observed trends for the given
#' outcomes and T0s, either as a csv or as a output.
#'
estimate_sc = function(
    outcomes, T0s, precision = .1, compute_ci = TRUE, save_csv = TRUE) {
  # Attach required packages
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(logger)
  library(scinference)

  # Raise errors
  if (length(outcomes) != length(T0s)) {
    stop(
      sprintf(
        "T0s must be the same size as outcomes. Got %s outcomes and %s T0s.",
        length(outcomes),
        length(T0s)
      )
    )
  }
  if ((!(is.numeric(precision))) | (!(precision > 0 & precision < 1))) {
    stop(
      sprintf(
        "precision must be a number between 0 and 1. Got %s.",
        precision
      )
    )
  }
  for (out in outcomes) {
    current_T0 = T0s[which(outcomes == out)]
    if ((out == "DAA" & current_T0 > 89) | (out != "DAA" & current_T0 > 114)) {
      stop(
        sprintf(
          "%s supports T0 up to %s. Got %s.",
          out,
          ifelse(out == "DAA", "89", "114"),
          current_T0
        )
      )
    }
  }

  # Import function to compute synthetic controls
  source("utils/sc.R")
  # Define treated and control units
  treated_units = c("ES", "PT")
  control_units = c(
    "AT", "BE", "BG", "CZ", "DE", "DK", "EE", "EL",
    "FI", "HR", "HU", "IE", "IT", "LT", "LU", "LV",
    "NL", "NO", "PL", "RO", "SE", "SI", "SK"
  )
  # Define treatment and end date
  treatment_date = as.Date("2022-06-01")
  end_date = as.Date("2023-06-01")
  # Import day-ahead auction data
  log_info("Loading data")
  daa_df = read_csv("data/DAA.csv", show_col_types = FALSE)
  # Import CPI at constant taxes
  hicp_df_raw = read_csv("data/hicp_ct.csv", show_col_types = FALSE)

  # Raise error
  not_supported = NULL
  for (out in outcomes) {
    if (out != "DAA" & !(out %in% hicp_df_raw$coicop)) {
      not_supported = cbind(not_supported, out)
    }
  }
  if (!(is.null(not_supported))) {
    if (length(not_supported) == 1) {
      stop(
        sprintf(
          "Outcome '%s' is not supported.\nSupported outcomes are: %s",
          paste(not_supported, collapse = ", "),
          paste(c("DAA", unique(hicp_df_raw$coicop)), collapse = ", ")
        )
      )
    } else {
      stop(
        sprintf(
          "Outcomes %s are not supported.\nSupported outcomes are: %s",
          paste(not_supported, collapse = ", "),
          paste(c("DAA", unique(hicp_df_raw$coicop)), collapse = ", ")
        )
      )
    }
  }

  # Preprocess CPI data
  hicp_df = hicp_df_raw |>
    mutate(
      donor_pool =
        case_when(
          geo %in% treated_units | geo %in% control_units ~ TRUE,
          TRUE ~ FALSE
        ),
      vars =
        case_when(
          coicop %in% outcomes ~ TRUE,
          TRUE ~ FALSE
        ),
      post_treatment =
        case_when(
          time >= treatment_date ~ TRUE,
          TRUE ~ FALSE
        )
    ) |>
    filter(
      donor_pool == TRUE &
        vars == TRUE &
        time <= end_date
    ) |>
    select(date = time, country = geo, outcome = coicop, values, post_treatment) |>
    pivot_wider(names_from = outcome, values_from = values) |>
    arrange(country, date)
  # Create empty data containers to store results
  agg_trends = NULL

  # One iteration for each treated unit
  for (tu in treated_units) {
    log_info(paste("Treated unit:", tu))
    # Identify the other treated unit
    n_treated = ifelse(tu == "ES", "PT", "ES")

    # One iteration for each outcome variable
    for (i in 1:length(outcomes)) {
      # Define outcome
      out = outcomes[i]
      # Define length of pre-treatment period
      T0 = T0s[i]
      log_info(paste0("  Outcome: ", out, " - T0: ", T0))
      # Final data processing
      if (out == "DAA") {
        # Remove other treated unit from sample + units with inconsistent data
        sc_df = daa_df |>
          filter(country != n_treated)
      } else {
        # Remove other treated unit from sample
        sc_df = hicp_df |>
          filter(country != n_treated)
      }
      # Define length of post-treatment period
      T1 = length(unique(sc_df$date[sc_df$post_treatment == TRUE]))
      # Define total length of period
      T01 = T0 + T1
      # Extract date range from data
      time_range = sc_df |>
        slice_tail(n = T01) |>
        select(date) |>
        as.matrix() |>
        unname()
      # Extract outcome for treated unit from data
      Y1 = sc_df |>
        filter(country == tu) |>
        select(all_of(out)) |>
        slice_tail(n = T01) |>
        as.matrix() |>
        unname()
      # Extract outcome for control units from data
      Y0 = sc_df |>
        filter(country != tu) |>
        select(date, country, all_of(out)) |>
        pivot_wider(names_from = country, values_from = all_of(out)) |>
        select(-date) |>
        slice_tail(n = T01) |>
        as.matrix() |>
        unname()
      # Estimate synthetic controls
      y1 = Y1[1:T0, ]
      y0 = Y0[1:T0, ]
      estimate = sc(
        y1 = y1, y0 = y0,
        Y1 = Y1, Y0 = Y0,
        lsei_type = 2
      )
      gaps = estimate$u.hat
      # Approximate range of CI from difference between observed and synthetic
      # trends
      if (compute_ci == TRUE) {
        sf = nchar(str_split(precision, "\\.")[[1]][2])
        max_gap = round(max(gaps[seq(T0 + 1, T01), ]), sf)
        min_gap = round(min(gaps[seq(T0 + 1, T01), ]), sf)
        range_gap = round(abs(max(gaps) - min(gaps)), sf)
        if (out == "DAA") {
          grid = seq(
            from = min_gap - (.2 * range_gap),
            to = max_gap + (.1 * range_gap),
            by = precision
          )
        } else {
          grid = seq(
            from = min_gap - (.4 * range_gap),
            to = max_gap + (.2 * range_gap),
            by = precision
          )
        }
      }
      ci_found = FALSE
      # Estimate CIs
      if (compute_ci) {
        log_info("    Searching CI...")
      }
      while (ci_found == FALSE) {
        result = scinference(
          Y1 = Y1,
          Y0 = Y0,
          T1 = T1,
          T0 = T0,
          inference_method = "conformal",
          alpha = .1,
          ci = compute_ci,
          theta0 = 0,
          estimation_method = "sc",
          permutation_method = "iid",
          n_perm = 5000,
          ci_grid = grid,
          lsei_type = 2
        )
        # Update grid as required or continue
        if (compute_ci) {
          if (min(grid) %in% result$lb & max(grid) %in% result$ub) {
            log_info("      Both bounds updated")
            grid = seq(
              from = round(min(grid) * 1.2, sf),
              to = round(max(grid) * 1.2, sf),
              by = precision
            )
          } else if (max(grid) %in% result$ub) {
            log_info("      Upper bound updated")
            grid = seq(
              from = min(grid),
              to = round(max(grid) * 1.2, sf),
              by = precision
            )
          } else if (min(grid) %in% result$lb) {
            log_info("      Lower bound updated")
            grid = seq(
              from = round(min(grid) * 1.2, sf),
              to = max(grid),
              by = precision
            )
          } else {
            ci_found = TRUE
            log_info("    CI found!")
          }
        } else {
          ci_found = TRUE
        }
      }
      # Store results
      trends = data.frame(
        date = time_range,
        obs = Y1,
        synth = estimate$Y0.hat,
        gaps = gaps,
        upper_ci = if (compute_ci) c(rep(NA, T0), result$ub) else NA,
        lower_ci = if (compute_ci) c(rep(NA, T0), result$lb) else NA,
        T0 = T0,
        outcome = out,
        treated = tu
      )
      agg_trends = rbind(agg_trends, trends)
    }
  }

  # Return results as saved csv or variable
  if (save_csv == TRUE) {
    if (compute_ci) {
      suffix = paste0("_", str_split(precision, "\\.")[[1]][2])
    } else {
      suffix = ""
    }
    file_path = sprintf(
      "results/sc_trends%s.csv",
      suffix
    )
    log_info(
      sprintf(
        "Saving results to %s",
        file_path
      )
    )
    write_csv(agg_trends, file_path)
  } else {
    return(agg_trends)
  }
}
