#' delta_f
#'
#' @param data a dataframe output from back_estimate function
#' @param var the variable which will be normalized (one of poly, gam, linear, quantile)
#' @param cov_split a list of data table, each element being one coverslip (one technical replicate)
#'
#'
#' @return a vector of the normalized fluorescence values with the delta f / f
#'method, taking the predicted background as a local baseline
#' @export
#'
#' @examples
delta_f <- function(cov_split, var = c("poly", "gam", "linear", "quantile", "back")){

  if(var == "poly"){
    cov_split <- lapply(cov_split, function(x) x[, delta_f_f := poly_detrended / poly_fit, by = Cell_id])
  }

  if(var == "gam"){
    cov_split <- lapply(cov_split, function(x) x[, delta_f_f := gam_detrended / gam_fit, by = Cell_id])
  }

  if(var == "linear"){
    cov_split <- lapply(cov_split, function(x) x[, delta_f_f := linear_detrended / linear_fit, by = Cell_id])
  }

  if(var == "quantile"){
    cov_split <- lapply(cov_split, function(x) x[, delta_f_f := quantile_detrended / local_quantile, by = Cell_id])
  }

  if(var == "back"){
    cov_split <- lapply(cov_split, function(x) x[, delta_f_f := background_detrended / background, by = Cell_id])
  }

  return(cov_split)
}
