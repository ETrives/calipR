
#' inferiorValues
#'
#' @param peak
#' @param full_data
#'
#' @return
#' @export
#'
#' @examples
inferiorValues <- function(peak, full_data) {

  data <- full_data[full_data$Cell_id == peak$Cell_id & full_data$time_frame %between% list(peak$Max_peak_frame - 10,peak$Max_peak_frame + 10),]

  max_peak <- data[data$time_frame == peak$Max_peak_frame,]

  data <- full_data[full_data$Cell_id == peak$Cell_id,]
  before_window <- data[data$time_frame <= max_peak$time_frame,]

  before_window <- before_window[order(-time_frame)][1:40]

  after_window <- data[data$time_frame >= peak$Max_peak_frame,][1:40]

  before <- before_window$Mean_Grey < max_peak$Mean_Grey
  after <-  after_window$Mean_Grey < max_peak$Mean_Grey

  d <- as.data.frame(x = before)

  d$after <- after

  infVal <- length(d[d$before == TRUE & d$after == TRUE,]$before)

  return(infVal)
}
