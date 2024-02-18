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


  pos <- data$smooth_Diff
  pos[pos < 0] <- 0
  neg <-  data$smooth_Diff
  neg[neg > 0] <- 0

  neg <- abs(neg)


  pos_lag <- dplyr::lag(pos, n = DPA_width, default = 0)
  neg_lead <- dplyr::lead(neg, n = DPA_width, default = 0)

  res_DPA <- pos_lag + neg_lead
  res_DPA <- res_DPA + dplyr::lag(res_DPA, n = 20, default = 0)

  return(res_DPA)
}
