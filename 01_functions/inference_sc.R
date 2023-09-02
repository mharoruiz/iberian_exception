#'
#' Inference of synthetic control units.
#'
#' @description This function reproduces the results from Haro-Ruiz, M., Schult 
#' C., and Wunder, C. (2023) , which estimates the effects of the Iberian 
#' Exception mechanism on a different price outcomes.
#'
#' It computes p-values by testing the null-hypothesis that the effect is 0 for
#' a given set of outcomes and T0s.
#'
#' @param outcomes A list of outcomes to compute synthetic controls for.
#' @param T0s  A list of sizes of pre-treatment periods. Must be same length
#' as outcomes.
#' @param break_poitns An object of class "Date" or a list containing "Date"
#' class objects. Break points will be used to divide the post-treatment
#' period into sub-periods and compute p-values across them. The post-
#' treatment period ranges from  07/2022 to  06/2023 so T1_breaks must be
#' within this range. For example, setting T1_breaks=as.Date("2022-12-01")
#' will compute p-values for the sub-periods 07/2022-12/2022 and 01/2023-
#' 06/2023.
#' @param save_csv logical to save results as csv file. The name of the file
#' will be set depending on the number of sub-periods and their length. For
#' example, setting T1_breaks=as.Date("2022-12-01") will return a file named
#' sc_inference_6_6.csv because this computes p-values for two sup-periods of
#' 6 months each. Default: TRUE.
#'
#' @return A dataframe with p-values for the specified sub-periods given
#' outcomes and T0s, either as a csv or as a output.
#'
inference_sc = function(outcomes, T0s, T1_breaks = NULL, save_csv = TRUE) {
  
  # Attach required packages
  require(lubridate)
  require(tidyr)
  require(dplyr)
  require(readr)
  require(eurostat)
  require(logger)
  require(scinference)

  # Define treatment and end date
  treatment_date = as.Date("2022-06-01")
  end_date = as.Date("2023-06-01")

  # Raise errors
  if (length(outcomes) != length(T0s)) {
    stop(
      sprintf(
        "T0s must be the same length as outcomes. Got %s outcomes and %s T0s",
        length(outcomes), length(T0s)
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

  # Remove duplicates
  T1_breaks = unique(T1_breaks)
  # Define treated and control units
  treated_units = c("ES", "PT")
  control_units = c(
    "AT", "BE", "BG", "CZ", "DE", "DK", "EE", "EL",
    "FI", "HR", "HU", "IE", "IT", "LT", "LU", "LV",
    "NL", "NO", "PL", "RO", "SE", "SI", "SK"
  )

  log_info("Loading data")
  # Import day-ahead auction data
  daa_df_raw = read_csv("02_data/day_ahead_price.csv", show_col_types = FALSE)
  # Import CPI at constant taxes
  if (file.exists("02_data/cpi_index.csv")) {
    hicp_df_raw = read_csv("02_data/cpi_index.csv", show_col_types = FALSE)
  } else {
    hicp_df_raw = get_eurostat("prc_hicp_cind", time_format="date")
    log_info("Saving HICP data to data/cpi_index.csv")
    write_csv(hicp_df_raw, "02_data/cpi_index.csv")
  }

  # Raise errors
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

  # Preprocess day-ahead price data
  daa_df = daa_df_raw |>
    group_by(country) |>
    filter(!any(is.na(DAA))) |>
    ungroup()
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
    select(
      date = time,
      country = geo,
      outcome = coicop,
      values, post_treatment
    ) |>
    pivot_wider(names_from = outcome, values_from = values) |>
    arrange(country, date)
  
  # Create empty data container to store results
  agg_inference = NULL

  # One iteration for each treated unit
  for (tu in treated_units) {
    log_info(sprintf("Treated unit: %s", tu))
    # Identify the other treated unit
    n_treated = ifelse(tu == "ES", "PT", "ES")

    # One iteration for each outcome variable
    for (i in 1:length(outcomes)) {
      # Define outcome
      out = outcomes[i]
      # Define length of pre-treatment period
      T0 = T0s[i]
      log_info(
        sprintf("  Outcome: %s - T0: %s months", out, T0)
      )
      # Define dates to break T1 into sub-periods
      date_breaks = c(treatment_date, T1_breaks, end_date)

      # One iteration for each sub-period within T1
      for (sp in 1:(length(date_breaks) - 1)) {
        # Remove other treated unit from sample
        if (out == "DAA") {
          sc_df = daa_df |>
            filter(
              country != n_treated
            )
        } else {
          sc_df = hicp_df |>
            filter(
              country != n_treated
            )
        }
        # Remove observations in T1 outside of this sub-period
        sc_df = sc_df |>
          filter(
            country != n_treated &
              (
                post_treatment == 0 |
                  (
                    post_treatment == 1 &
                      date > date_breaks[sp] &
                      date <= date_breaks[(sp + 1)]
                  )
              )
          )

        # Define length of T1
        T1_dates = unique(sc_df$date[sc_df$post_treatment == TRUE])
        min_date = min(T1_dates)
        max_date = max(T1_dates)
        T1_range = paste0(
          ifelse( 
            nchar(month(min_date)) == 1, 
            paste0(0, month(min_date)),
            month(min_date)
            ),
          "/",
          year(min_date),
          " - ",
          ifelse( 
            nchar(month(max_date)) == 1, 
            paste0(0, month(max_date)),
            month(max_date)
          ), 
          "/",
          year(max_date)
        )
        log_info(
          sprintf(
            "    T1.%s: %s %s",
            sp,
            length(T1_dates),
            ifelse(length(T1_dates) > 1, "months", "month")
          )
        )
        T1 = length(T1_dates)
        if (sp == 1) suffix = T1 else suffix = paste(suffix, T1, sep = "_")
        # Define total length of period
        T01 = T0 + T1
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

        # Estimate p-values
        result = scinference(
          Y1 = Y1,
          Y0 = Y0,
          T1 = T1,
          T0 = T0,
          inference_method = "conformal",
          alpha = .1,
          ci = FALSE,
          theta0 = 0,
          estimation_method = "sc",
          permutation_method = "iid",
          n_perm = 5000,
          lsei_type = 2
        )

        # Store results
        inference = data.frame(
          p_val = result$p_val,
          T0 = T0,
          outcome = out,
          treated = tu,
          period = T1_range
        )
        agg_inference = rbind(agg_inference, inference)
      }
    }
  }

  # Save results or return them
  if (save_csv == TRUE) {
    if (!dir.exists("03_results")) dir.create("03_results")
    file_path = sprintf(
      "03_results/sc_inference_%s.csv",
      suffix
    )
    log_info(
      sprintf(
        "Saving results to %s",
        file_path
      )
    )
    write_csv(agg_inference, file_path)
  } else {
    return(agg_inference)
  }
  
}
