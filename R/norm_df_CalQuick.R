
#' norm_df
#'
#' @param data a data frame output from prepareData and back_estimate
#' @param cov_split A list of data frame with each element being the data for one coverslip (one technical replicate)
#' @param dim_list A list with each element being the dimension of a data frame in cov_split
#'
#' @return An updated data frame with four new columns : z_score (the values transformed in z_score)
#' and the smoothed version of the z_score ; delta_f_f (the values normalized by delta f /f method)
#' and its smoothed version
#' @export
#'
#'
#' @examples
norm_df <- function(data, var = c("raw", "poly", "gam", "linear", "quantile"), width, new = FALSE){

  print("inside norm")
  print(data$coverslip)
  print(typeof(data$coverslip))
  data$coverslip <- as.character(data$coverslip)

  cov_split <- split(data, data$coverslip)
  dim_list <- lapply(cov_split, function(x) dim(dplyr::filter(x, Cell_id == x$Cell_id[[1]]))[1])

  data_z <- z_score_travaux(data, var = var, cov_split = cov_split, dim_list = dim_list, new = new)
  print("z score computed")

  data_d <- delta_f(data_z, var = var)
  print("delta f / f computed")


   #smoothing the z score and the delta f variables
  smooth_df<- lapply(data_d, function(x) data.table::setDT(x)[, ':=' (smooth_z = gplots::wapply(time_frame,
                                        z_score, fun = mean, n = length(time_frame), width = width, method = "nobs")[[2]],
                                        smooth_delta = gplots::wapply(time_frame,
                                        delta_f_f, fun = mean, n = length(time_frame), width = width, method = "nobs")[[2]]),
                                        by = Cell_id])


  smooth_df <- do.call(rbind, smooth_df)

  #smooth_df <- do.call(rbind, data_d)

  return(smooth_df)
}
