#'
#' Estimate synthetic controls
#'
#' @description This function estimates a synthetic control unit for a
#' particular outcome variable for time based on a series of J control units
#' in the donor pool. It is based on the scinference package.
#'
#' @param y1 (T0 x 1) matrix of outcome variable for the treated unit in the
#' pre-treatment period.
#' @param y0 (T0 x J) matrix of outcome variable for the control units in the
#' pre-treatment period.
#' @param Y1 (T x 1) matrix of outcome variable for the treated unit in the
#' pre and post-treatment periods.
#' @param Y0 (T x J) matrix of outcome variable for the control units in the
#' pre and post-treatment periods.
#' @param lsei_type Integer indicating the minimization algorithm (see
#' limSolve::lsei for details).
#'
#' @return List with the following objects:
#' \itemize{
#'  \item u.hat - (T x 1) matrix of differences between observed and synthetic
#'  series
#'  \item w.hat - J vector of weights assigned to each unit in the donor pool
#'  \item Y0.hat - (T x 1) matrix of synthetic control unit
#' }
#' 
sc = function(y1, y0, Y1, Y0, lsei_type) {
  
  # Attach required packages
  suppressMessages(require(limSolve))
  
  # Define variables
  J = dim(y0)[2]
  e = matrix(1, 1, J)
  f = 1
  g = diag(x = 1, J, J)
  h = matrix(0, J, 1)
  # Estimate optimized weights for each control unit
  w.hat = lsei(A = y0, B = y1, E = e, F = f, G = g, H = h, type = lsei_type)$X
  # Estimate synthetic control unit
  Y0.hat = Y0 %*% w.hat
  # Compute differences between synthetic and observed series
  u.hat = Y1 - Y0.hat

  return(
    list(
      u.hat = u.hat,
      w.hat = w.hat,
      Y0.hat = Y0.hat
    )
  )
  
}
