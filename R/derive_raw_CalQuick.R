#' derive_raw
#'
#' This function computes the first derivative on the raw detrended fluorescence values.
#'
#' @param x A dataframe output from back estimate function at least, not earlier in the process
#'
#' @return
#' @export
#'
#' @examples
derive_raw <- function(x){
  print(x$Cell_id[1])
  res <- doremi::calculate.gold(time = x$time_seconds, signal = x$Poly_detrended,
                        embedding = 3, n = 1)
  return(res)
}
