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
#' @param outcomes Matrix of outcomes to compute synthetic controls for.
#' @param T0s  Matrix of sizes of pre-treatment periods. Must be same length
#' as outcomes.
#' @param method String indicating the inference method. Available options are
#' "conformal" and "ttest". See scinference pacakge for details.
#' @param save_csv Boolean to save results as csv file. Default: TRUE.
#'
#' @return Dataframe with p-values for the specified sub-periods given
#' outcomes and T0s, either as a csv or as a output.
#'
inference_sc = function(outcomes, T0s, method, save_csv = TRUE) {
  
  # Attach required packages
  library(lubridate)
  library(tidyr)
  library(dplyr)
  library(readr)
  library(eurostat)
  library(logger)
  library(scinference)

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
    if ((out == "DAP" & current_T0 > 89) | (out != "DAP" & current_T0 > 114)) {
      stop(
        sprintf(
          "%s supports T0 up to %s. Got %s.",
          out,
          ifelse(out == "DAP", "89", "114"),
          current_T0
        )
      )
    }
  }

  # Define treated and control units
  treated_units = c("ES", "PT")
  control_units = c(
    "AT", "BE", "BG", "CZ", "DE", "DK", "EE", "EL",
    "FI", "HR", "HU", "IE", "IT", "LT", "LU", "LV",
    "NL", "NO", "PL", "RO", "SE", "SI", "SK"
  )

  log_info("Loading data")
  # Import day-ahead price data
  dap_df_raw = read_csv("02_data/day_ahead_price.csv", show_col_types = FALSE)
  # Import CPI at constant taxes
  if (file.exists("02_data/cpi_index.csv")) {
    hicp_df_raw = read_csv("02_data/cpi_index.csv", show_col_types = FALSE)
  } else {
    hicp_df_raw = get_eurostat("prc_hicp_cind", time_format="date")
    log_info("Saving HICP data to 02_data/cpi_index.csv")
    write_csv(hicp_df_raw, "02_data/cpi_index.csv")
  }

  # Raise errors
  hicp_df_raw = hicp_df_raw |>
    mutate(
      coicop =
        case_when(
          coicop == "TOT_X_NRG" ~ "xNRG",
          coicop == "NNRG_IGD" ~ "IGDxNRG",
          TRUE ~ coicop
        )
    )
  not_supported = NULL
  for (out in outcomes) {
    if (out != "DAP" & !(out %in% hicp_df_raw$coicop)) {
      not_supported = cbind(not_supported, out)
    }
  }
  if (!(is.null(not_supported))) {
    if (length(not_supported) == 1) {
      stop(
        sprintf(
          "Outcome '%s' is not supported.\nSupported outcomes are: %s",
          paste(not_supported, collapse = ", "),
          paste(c("DAP", unique(hicp_df_raw$coicop)), collapse = ", ")
        )
      )
    } else {
      stop(
        sprintf(
          "Outcomes %s are not supported.\nSupported outcomes are: %s",
          paste(not_supported, collapse = ", "),
          paste(c("DAP", unique(hicp_df_raw$coicop)), collapse = ", ")
        )
      )
    }
  }

  # Preprocess day-ahead price data
  dap_df = dap_df_raw |>
    group_by(country) |>
    filter(!any(is.na(DAP))) |>
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

      # Remove other treated unit from sample
      if (out == "DAP") {
        sc_df = dap_df |>
          filter(
            country != n_treated & date != treatment_date
          )
      } else {
        sc_df = hicp_df |>
          filter(
            country != n_treated & date != treatment_date
          )
      }

      # Define T1 properties
      T1_dates = unique(sc_df$date[sc_df$post_treatment == TRUE])
      min_T1 = min(T1_dates)
      max_T1= max(T1_dates)
      T1 = length(T1_dates)
      # Define total length of period
      T01 = T0 + T1
      # Extract outcome for treated unit from data
      Y1 = sc_df |>
        filter(country == tu) |>
        arrange(date) |>
        select({{out}}) |>
        slice_tail(n = T01) |>
        as.matrix() |>
        unname()
      # Extract outcome for control units from data
      Y0 = sc_df |>
        filter(country != tu) |>
        select(date, country, {{out}}) |>
        arrange(date) |>
        pivot_wider(names_from = country, values_from = {{out}}) |>
        select(-date) |>
        slice_tail(n = T01) |>
        select_if(~ !any(is.na(.))) |>
        as.matrix() |>
        unname()
      log_info(
      sprintf("  %s units in donor pool", dim(Y0)[2])
        )
      # Estimate p-values
      result = scinference(
        Y1 = Y1,
        Y0 = Y0,
        T1 = T1,
        T0 = T0,
        inference_method = method,
        alpha = .1,
        ci = FALSE,
        theta0 = 0,
        estimation_method = "sc",
        #permutation_method = "iid",
        #n_perm = 5000,
        lsei_type = 2
      )

      # Store results
      if (method == "conformal") {
        inference = data.frame(
          pval = result$p_val,
          T0 = T0,
          outcome = out,
          treated = tu,
          from = min_T1,
          to = max_T1
        )
        
      } else if (method == "ttest") {
        inference = data.frame(
          att = result$att,
          se = result$se,
          lb = result$lb,
          ub = result$ub,
          T0 = T0,
          outcome = out,
          treated = tu,
          from = min_T1,
          to = max_T1
        )
      }

      agg_inference = rbind(agg_inference, inference)
      
    }
  }

  # Return results as saved csv or dataframe
  if (save_csv) {
    if (!dir.exists("03_results")) dir.create("03_results")
    file_path = sprintf(
      "03_results/sc_inference_%s.csv",
      method
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
