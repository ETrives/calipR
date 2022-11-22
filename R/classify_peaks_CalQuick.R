#' classify_peaks
#'
#' @param data
#' @param time_thresh
#' @param frame_rate
#'
#' @return
#' @export
#'
#' @examples
classify_peaks <- function(data, time_thresh, frame_rate){

  #data <- plyr::ddply(data, .(Cell_id), mutate, early_peak = Start_peak_stimulation == TRUE & True_peak == TRUE,
                #late_peak = Start_peak_rel_frame > min_to_f(time_thresh, frame_rate) & True_peak == TRUE,
                #standard_peak =  early_peak == FALSE & late_peak == FALSE & True_peak == TRUE)

  data <- data.table::setDT(data[[1]])

  data <- data[, early_peak := Start_peak_stimulation == TRUE & True_peak == TRUE, by = Cell_id]
  data <- data[, late_peak := Start_peak_rel_frame > min_to_f(time_thresh, frame_rate) & True_peak == TRUE, by = Cell_id]
  data <- data[, standard_peak :=  early_peak == FALSE & late_peak == FALSE & True_peak == TRUE, by = Cell_id]


  return(data)
}
