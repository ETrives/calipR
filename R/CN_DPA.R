#' CN_DPA
#'
#'Implementation of a constrained non negative variant of the derivative passing accumulation method
#'
#' @param data
#' @param CN_DPA_width
#'
#' @return
#' @export
#'
#' @examples
CN_DPA <- function(data, CN_DPA_width){

  pos <- data$smooth_Diff
  pos[pos < 0] <- 0

  res_CN_DPA <- lapply(seq(5, CN_DPA_width, by = 10), function(x) dplyr::lag(pos, n = x, default = 0))

  res <- rowSums(as.data.frame(res_CN_DPA))

  return(as.numeric(res))
}
