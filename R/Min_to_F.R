#' Converts time in minutes in frames
#'
#' @param x
#' @param frame_rate
#'
#' @return
#' @export
#'
#' @examples
min_to_f <- function(x, frame_rate){

  if(is.integer(x) == FALSE) {
    y <- as.integer(x)
    y_min <- y*60

    y_final <- y_min + ((x %% 1) * 100)
  }

  else{
    y_final <- x*60

  }
  return(as.integer(y_final*frame_rate))
}
