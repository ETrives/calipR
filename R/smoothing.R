
#' smoothing
#'
#' @param data
#' @param band
#' @param delta_f
#' @param z_score
#' @param raw
#'
#' @return
#' @export
#'
#' @importFrom stats ksmooth
#' @examples
smoothing <- function(data, band = 4, delta_f = FALSE, z_score = FALSE, raw = FALSE){

  if(raw == TRUE){
    k <- ksmooth(data$time_frame, data$Mean_Grey, kernel = "normal", bandwidth= band)
    data$Ksmooth_raw <- k$y

  }

  if(delta_f == TRUE){
    k <- ksmooth(data$time_frame, data$delta_f_f, kernel = "normal", bandwidth= band)
    data$Ksmooth_delta <- k$y

  }

  if(z_score == TRUE){
    k <- ksmooth(data$time_frame, data$z_score, kernel = "normal", bandwidth= band)
    data$Ksmooth_z <- k$y

  }

  return(data)
}
