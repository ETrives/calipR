#' derive_smooth
#'
#'This function computes the first derivative on the standardized + smoothed fluorescence values
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
derive_smooth <- function(x){
  #print(x$Cell_id[1])
  res <- doremi::calculate.gold(time = x$time_seconds, signal = x$smooth_z,
                        embedding = 3, n = 1)
  return(res)
}
