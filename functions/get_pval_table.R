#'
#' Inference table: Twelve Months After the Iberian Exception.
#'
#' @description This function replicates table A1 of Haro-Ruiz, M., Shcult C.,
#' and Wunder, C. (2023), which outlines the estimated ATEs of the Iberian 
#' exception mechanism on different price indicators for Spain and Portugal.
#'
#' @param dfs A list with dataframes returned by estimate_sc().
#'
#' @return A dataframe with average treatment effects across periods for
#' different outcomes and treated units.
#'
get_pval_table = function(dfs) {
  # Attach required packages
  require(tidyverse)
  #library(dplyr)
  #library(tidyr)

  # Raise errors
  expected_colnames = c("outcome", "treated", "period", "p_val")
  for (i in 1:length(dfs)) {
    missing_colnames = !(expected_colnames %in% colnames(dfs[[i]]))
    if (sum(missing_colnames) != 0) {
      stop(
        sprintf(
          "Variables %s must be columns in provided dataframes.",
          paste(
            expected_colnames[which(missing_colnames)],
            collapse = ", "
          )
        )
      )
    }
  }

  # Concatenate dataframes
  for (i in 1:length(dfs)) {
    if (i == 1) {
      pval_table = dfs[[i]]
    } else {
      pval_table = rbind(pval_table, dfs[[i]])
    }
  }
  # Format table
  pval_table = pval_table |>
    arrange(outcome, treated, period) |>
    select(outcome, treated, period, p_val) |>
    pivot_wider(
      names_from = treated,
      values_from = p_val,
      names_prefix = "pval_"
    )

  return(pval_table)
}
