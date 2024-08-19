#'
#' Estimation of synthetic control units
#'
#' @description This function reproduces the results from Haro Ruiz, M., Schult 
#' C., and Wunder, C. (2024), which estimates the effects of the Iberian 
#' Exception mechanism on different price outcomes.
#'
#' The effect of the intervention is estimated via synthetic controls for
#' Spain and Portugal given outcome variables. Additionally, the function 
#' returns dataframes with inferential results and optimized weights assigned to 
#' each country in the donor pool of control units.
#'
#' @param outcomes Matrix of outcomes to compute synthetic controls for.
#' @param compute_ci Boolean to compute 90 %confidence intervals or not. If
#' set to TRUE, run-time will increase significantly. Default: FALSE.
#' @param precision Float between 0 and 1 which defines the step of the grid-
#' search space to find confidence intervals.
#' @param infer_method String indicating the inference method. Available options 
#' are "conformal" and "ttest". See scinference package for details.
#' @param placebo_time Integer indicating the number of months that the 
#' intervention date should be set back in order to conduct placebo tests for 
#' the time dimension. A value of 0 indicates no placebo, whereas values larger 
#' than 0 will result in placebo tests. If placebo_time is larger than 0, the 
#' saved csv files will be named with the suffix "_placebo_time_X", where X is 
#' the value of the placebo_time variable. Default: 0.
#' @param save_series Boolean to save synthetic control series as csv file. The 
#' name of the file will be set depending on the precision variable. For 
#' example, setting precision=.01 will return a file named sc_series_01.csv. 
#' Default: TRUE.
#' @param save_infer Boolean to save inference results as a csv file. The name 
#' of the file will be set depending on the infer_method variable. For example, 
#' setting infer_method="ttest" will return a file names sc_inference_ttest.csv.
#' Default: TRUE.
#' @param save_weights Boolean to save optimized weights as a csv. 
#' Default: FALSE.
#'
#' @return Dataframe with synthetic control series, inference results and 
#' optimized weights for the given outcomes, either as a csv or as output 
#' variables.
#'
estimate_sc = function(outcomes, compute_ci = FALSE, precision, infer_method,
                       placebo_time = 0, save_series = TRUE, save_infer = TRUE, 
                       save_weights = FALSE) {
  
  # Attach required packages and functions
  library(lubridate)
  library(readr)
  library(tidyr)
  library(dplyr)
  library(stringr)
  library(eurostat)
  library(logger)
  library(scinference)
  source("01_functions/sc.R")
  
  # Raise errors
  if (!is.numeric(placebo_time)) {
    stop("placebo_time should be an integer between 0 and 70, both included.")
  }
  if (!(placebo_time %% 1 == 0) | (placebo_time < 0 | placebo_time > 70)) {
    stop("placebo_time should be an integer between 0 and 70, both included.")
  }
  
  # Determine whether placebo time is active
  is_placebo = if (placebo_time > 0) TRUE else FALSE
  
  if (is_placebo) {
    log_info(sprintf("*** Placebo test - time dimension (-%s months) ***", 
                     placebo_time))
  }

  # Raise errors
  if (compute_ci) {
    if ((!(is.numeric(precision))) | (!(precision > 0 & precision < 1))) {
      stop(
        sprintf("precision must be a number between 0 and 1. Got %s.", precision)
      )
    }
  }
  
  # Define treated and control units
  treated_units = c("ES", "PT")
  control_units = c("AT", "BE", "BG", "CZ", "DE", "DK", "EE", "EL", "FI", "HR",
                    "HU", "IE", "IT", "LT", "LU", "LV", "NL", "NO", "PL", "RO", 
                    "SE", "SI", "SK")
  
  # Define start, treatment and end dates
  start_date_hicp = as.Date("2013-06-01")
  treatment_date = as.Date("2022-06-01") %m-% months(placebo_time)
  end_date = as.Date("2023-12-01")
  
  log_info("Loading data")
  # Import day-ahead price data
  dap_df_raw = read_csv("02_data/day_ahead_price.csv", show_col_types = FALSE)
  # Import CPI at constant taxes
  if (file.exists("02_data/cpi_index.csv")) {
    hicp_df_raw = read_csv("02_data/cpi_index.csv", show_col_types = FALSE) 
  } else {
    hicp_df_raw = get_eurostat("prc_hicp_cind", time_format="date") 
    log_info("Saving CPI data to 02_data/cpi_index.csv")
    write_csv(hicp_df_raw, "02_data/cpi_index.csv")
  }
  
  # Raise errors
  hicp_df_raw = hicp_df_raw |>
    mutate(coicop = case_when(coicop == "TOT_X_NRG" ~ "xNRG",
                              coicop == "NNRG_IGD" ~ "IGDxNRG",
                              TRUE ~ coicop) )
  not_supported = NULL
  for (out in outcomes) {
    if (out != "DAP" & !(out %in% hicp_df_raw$coicop)) {
      not_supported = cbind(not_supported, out)
    }
  }
  if (!(is.null(not_supported))) {
    if (length(not_supported) == 1) {
      stop(
        sprintf("Outcome '%s' is not supported.\nSupported outcomes are: %s",
                paste(not_supported, collapse = ", "),
                paste(c("DAP", unique(hicp_df_raw$coicop)), collapse = ", ") )
      )
    } else {
      stop(
        sprintf("Outcomes %s are not supported.\nSupported outcomes are: %s",
                paste(not_supported, collapse = ", "),
                paste(c("DAP", unique(hicp_df_raw$coicop)), collapse = ", ") )
      )
    }
  }

  # Preprocess day-ahead auction data
  dap_df = dap_df_raw |>
    group_by(country) |>
    filter(!any(is.na(DAP))) |>
    ungroup() |>
    mutate(country_pool = case_when(country %in% c(treated_units, control_units) ~ TRUE,
                                    TRUE ~ FALSE),
           post_treatment = case_when(date >= treatment_date ~ TRUE,
                                      TRUE ~ FALSE) ) |>
    filter(country_pool == TRUE) |>
    select(date, country, DAP, post_treatment)
  # Preprocess CPI data
  hicp_df = hicp_df_raw |>
    mutate(
      country_pool = case_when(geo %in% c(treated_units, control_units) ~ TRUE,
                               TRUE ~ FALSE),
      vars = case_when(coicop %in% outcomes ~ TRUE,
                       TRUE ~ FALSE),
      post_treatment = case_when(TIME_PERIOD >= treatment_date ~ TRUE,
                                 TRUE ~ FALSE)
      ) |>
    filter(country_pool == TRUE & vars == TRUE & TIME_PERIOD >= start_date_hicp 
           & TIME_PERIOD <= end_date) |>
    select(date = TIME_PERIOD, country = geo, outcome = coicop, values, 
           post_treatment) |>
    pivot_wider(names_from = outcome, values_from = values) |>
    arrange(country, date)
  
  # Create empty data containers to store results
  series = NULL
  infer = NULL
  weights = NULL

  # One iteration for each treated unit
  for (tu in iteration_units) {
    log_info(sprintf("Treated unit: %s", tu))

    # One iteration for each outcome variable
    for (i in 1:length(outcomes)) {
      # Define outcome
      out = outcomes[i]
      
      #Rename dataframe 
      n_treated = if (tu == "ES") "PT" else "ES"
      if (out == "DAP") {
        sc_df = dap_df |>
          filter(country != n_treated)
      } else {
        sc_df = hicp_df |>
          filter(country != n_treated)
      }
      
      # Define length of pre-treatment period (T0)
      T0 = length(unique(sc_df$date[!(sc_df$post_treatment)]))
      log_info(sprintf("  Outcome: %s - T0: %s months", out, T0))
      # Define length of post-treatment period (T1)
      if (is_placebo) {
        T1 = 19
      } else {
        T1 = length(unique(sc_df$date[sc_df$post_treatment]))
      }
      # Define full length
      T01 = T0 + T1
      
      # Extract date range from data
      time_range = sc_df |>
        slice_head(n = T01) |>
        select(date) |>
        arrange(date) |>
        distinct(.keep_all = TRUE) |>
        as.matrix() |>
        unname()
      # Extract outcome for treated unit from data
      Y1 = sc_df |>
        filter(country == tu) |>
        arrange(date) |>
        slice_head(n = T01) |>
        select({{out}}) |>
        as.matrix() |>
        unname()
      # Extract outcome for control units from data
      Y0 = sc_df |>
        filter(country != tu) |>
        select(date, country, {{out}}) |>
        arrange(country, date) |>
        pivot_wider(names_from = country, values_from = {{out}}) |>
        select(-date) |>
        slice_head(n = T01) |>
        select_if(~ !any(is.na(.))) |>
        as.matrix()
      log_info(sprintf("  %s units in donor pool", dim(Y0)[2]))
      
      # Estimate synthetic controls
      y1 = Y1[1:T0, ]
      y0 = Y0[1:T0, ]
      estimate = sc(y1 = y1, y0 = y0, Y1 = Y1, Y0 = Y0, lsei_type = 2)
      diff = unname(estimate$u.hat)
      
      # Compute inference
      infer_result = scinference(Y1 = Y1, Y0 = Y0, T1 = T1, T0 = T0,
                                 inference_method = infer_method, alpha = .1, 
                                 ci = FALSE, theta0 = 0, 
                                 estimation_method = "sc", lsei_type = 2)
      # Store single inference results
      if (infer_method == "conformal") {
        single.infer = data.frame(pval = infer_result$p_val, T0 = T0, outcome = out,
                                  treated = tu,
                                  placebo = if (is_placebo) paste0("time_", placebo_time) else FALSE)
      } else if (infer_method == "ttest") {
        single.infer = data.frame(att = infer_result$att, se = infer_result$se,
                                  lb = infer_result$lb, ub = infer_result$ub, T0 = T0,
                                  outcome = out, treated = tu,
                                  placebo = if (is_placebo) paste0("time_", placebo_time)  else FALSE)
      }
      infer = rbind(infer, single.infer)
      
      # Approximate range of CI from difference between observed and synthetic series
      if (compute_ci == TRUE) {
        factor = mean(diff[1:T0])/mean(diff[T0+1:T1])
        decimal_plc = nchar(str_split(precision, "\\.")[[1]][2])
        min_gap = round(min(diff[T0+1:T1]), decimal_plc)
        max_gap = round(max(diff[T0+1:T1]), decimal_plc)
        gap_range = abs(max_gap - min_gap)
        grid = seq(from = min_gap - abs(min_gap * factor),
                   to = max_gap + abs(max_gap * factor),
                   by = precision)
      }
      # Estimate CI
      if (compute_ci) log_info("    Searching CI...")
      ci_found = FALSE
      while (!ci_found) {
        series_result = scinference(Y1 = Y1, Y0 = Y0, T1 = T1,  T0 = T0,
                                    inference_method = "conformal", alpha = .1, 
                                    ci = compute_ci, theta0 = 0, 
                                    estimation_method = "sc", 
                                    permutation_method = "iid", n_perm = 5000, 
                                    ci_grid = grid, lsei_type = 2)
        # Update grid or continue
        if (compute_ci) {
          if (min(grid) %in% series_result$lb & max(grid) %in% series_result$ub) {
            log_info("      Both bounds updated")
            grid = seq(from = round(min(grid) - gap_range*.33, decimal_plc),
                       to = round(max(grid) + gap_range*.33, decimal_plc),
                       by = precision)
          } else if (max(grid) %in% series_result$ub) {
            log_info("      Upper bound updated")
            grid = seq(from = min(grid),
                       to = round(max(grid) + gap_range*.33, decimal_plc),
                       by = precision)
          } else if (min(grid) %in% series_result$lb) {
            log_info("      Lower bound updated")
            grid = seq(from = round(min(grid) - gap_range*.33, decimal_plc),
                       to = max(grid), by = precision)
          } else {
            log_info("    CI found!")
            ci_found = TRUE
          }
        } else {
          ci_found = TRUE
        }
      }
      # Store single series results
      single.series = data.frame(date = time_range, obs = Y1, 
                                 synth = estimate$Y0.hat, diff = diff,
                                 upper_ci = if (compute_ci) c(rep(NA, T0), series_result$ub) else NA, 
                                 lower_ci = if (compute_ci) c(rep(NA, T0), series_result$lb) else NA, 
                                 T0 = T0, outcome = out, treated = tu,
                                 placebo = if (is_placebo) paste0("time_", placebo_time) else FALSE)
      series = rbind(series, single.series)
      
      # Store single optimized weights
      single.weights = data.frame(control_unit = colnames(Y0), 
                                  weight = estimate$w.hat, treated = tu, 
                                  outcome = out,
                                  placebo = if (is_placebo) paste0("time_", placebo_time) else FALSE)
      weights = bind_rows(weights, single.weights)
    }
  }

  # Return series as saved csv or as variable
  if (save_series) {
    if (compute_ci) {
      suffix = paste0("_", str_split(precision, "\\.")[[1]][2])
    } else {
      suffix = ""
    }
    if (is_placebo) {
      if (!dir.exists("04_placebo")) dir.create("04_placebo")
      series_fpath = sprintf("04_placebo/sc_series%s_placebo_time_%s.csv",
                                 suffix, placebo_time)
    } else {
      if (!dir.exists("03_results")) dir.create("03_results")
      series_fpath = sprintf("03_results/sc_series%s.csv", suffix)
    }
    log_info(sprintf("Saving series to %s", series_fpath))
    write_csv(series, series_fpath)
  }
  else {
    return(series)
  }
  
  # Return inference results as csv or as variable
  if (save_infer) {
    if (is_placebo) {
      if (!dir.exists("04_placebo")) dir.create("04_placebo")
      infer_fpath = sprintf("04_placebo/sc_inference_%s_placebo_time_%s.csv",
                            infer_method, placebo_time)
    } else {
      if (!dir.exists("03_results")) dir.create("03_results")
      infer_fpath = sprintf("03_results/sc_inference_%s.csv", infer_method)
    }
    log_info(sprintf("Saving inference to %s", infer_fpath))
    write_csv(infer, infer_fpath)
  } else {
    return(infer)
  }
  
  # Return optimized weights as csv or as variable
  if (save_weights) {
    if (is_placebo) {
      if (!dir.exists("04_placebo")) dir.create("04_placebo")
      weights_fpath = sprintf("04_placebo/sc_optimized_weights_placebo_time_%s.csv",
                              placebo_time)
    } else {
      if (!dir.exists("03_results")) dir.create("03_results")
      weights_fpath = "03_results/sc_optimized_weights.csv"
    }
    log_info(sprintf("Saving optimized weights to %s", weights_fpath))
    write_csv(weights, weights_fpath)
  } # else {
  #   return(weights)
  # }
  
}
