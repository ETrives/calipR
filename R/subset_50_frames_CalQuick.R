
#' subset_window
#'
#'This function is defined to be used inside an lapply function. It subsets 50 frames
#'before a peak and stores it in a new data table. For each peak, there will be a corresponding
#'data table with only the 50 frames before the peak.
#'
#' @param dt_2 a data table containing the entire dataset (first ouput from resolve_mult_peaks function)
#' @param x a data table containing the informations regarding a given peak (second output from resolve_mult_peaks)
#'
#' @return a data table object subset with only 50 frames before a given peak
#' @export
#'
#' @importFrom data.table %between%
#'
#' @examples
subset_window <- function(dt_2, x, period = c("start", "end")) {

  if(period == "start") {
    dt <- dt_2[dt_2$Cell_id == x$Cell_id & dt_2$time_frame %between% list(x$start_window, x$Max_peak_frame)]
    print(dt$Cell_id[1])
  }

  if(period == "end") {
    dt <- dt_2[dt_2$Cell_id == x$Cell_id & dt_2$time_frame %between% list(x$Max_peak_frame, x$end_window)]
    print(dt$Cell_id[1])
  }

  return(dt)
}
