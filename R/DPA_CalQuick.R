#' DPA
#'
#'Implementation of the derivative passing accumulation method
#'
#' @param DPA_width
#' @param mean_width
#' @param data
#'
#' @return
#' @export
#'
#' @examples
DPA <- function(data, DPA_width, mean_width_diff){

  data <- data.table::setDT(data)[ , smooth_Diff := gplots::wapply(data$time_frame, data$first_derivative, fun = mean, n =length(data$time_frame), width = mean_width_diff, method = "nobs", drop.na = FALSE)[[2]]]

  pos <- data$first_derivative
  pos[pos < 0] <- 0
  neg <-  data$first_derivative
  neg[neg > 0] <- 0

  neg <- abs(neg)


  pos_lag <- dplyr::lag(pos, n = DPA_width, default = 0)
  neg_lead <- dplyr::lead(neg, n = DPA_width, default = 0)

  accumulation <- pos_lag + neg_lead
  data$DPA <- accumulation


  return(data)
}
