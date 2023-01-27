#' DPA
#'
#'Implementation of the derivative passing accumulation method
#'
#' @param DPA_width
#' @param data
#'
#' @return
#' @export
#'
#' @examples
DPA <- function(data, DPA_width){

  #data <- data.table::setDT(data)[ , smooth_Diff := gplots::wapply(data$time_frame, data$first_derivative, fun = mean, n =length(data$time_frame), width = mean_width_diff, method = "nobs", drop.na = FALSE)[[2]]]

  print(data$Cell_id[[1]])
  pos <- data$first_derivative
  pos[pos < 0] <- 0
  neg <-  data$first_derivative
  neg[neg > 0] <- 0

  neg <- abs(neg)


  pos_lag <- dplyr::lag(pos, n = DPA_width, default = 0)
  neg_lead <- dplyr::lead(neg, n = DPA_width, default = 0)

  res_DPA <- pos_lag + neg_lead
  res_DPA <- res_DPA + dplyr::lag(res_DPA, n = 20, default = 0)
  #data$DPA <- accumulation

  return(res_DPA)
}
