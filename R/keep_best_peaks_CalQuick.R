#' keep_best_peaks
#'
#' @param data the whole output from find_peaks function
#' @param range
#'
#' @return the same objects with an added column named True_Peaks which is TRUE if the peak is kept, FALSE if not
#' @export
#'
#' @examples
keep_best_peaks <- function(data){

  #data[[1]] <- data.table::setDT(data[[1]])[, True_peak := Max_peak_smooth_z == max(Max_peak_smooth_z), by = list(Cell_id, Max_peak_stimulus)]
  #data[[2]] <- data.table::setDT(data[[2]])[, True_peak := smooth_z == max(smooth_z), by = list(Cell_id, stimulus)]


  #peaks_data  <- data[[1]][data[[1]]$True_peak == TRUE,]
  #peaks_list <- split(peaks_data,cumsum(1:nrow(peaks_data) %in% seq(1:nrow(peaks_data))))

  peaks_list <- split(data[[1]],cumsum(1:nrow(data[[1]]) %in% seq(1:nrow(data[[1]]))))


  peaks_list <- lapply(peaks_list, function(x) x[, infValue :=  inferiorValues(x, data[[2]])])

  peaks_list <- lapply(peaks_list, function(x) x[, bimod_peak :=  bimodal_cell(x, data[[2]])])


  data_peaks <- do.call(rbind, peaks_list)

  model <- readRDS(system.file("model/model.rds", package = "CalQuick"))
  #readRDS("model.rds")

  data_peaks[, Prediction := predict(model, data_peaks, type="response")]

  data_final <- data_peaks[data_peaks$Prediction > 0.5,]

  return(list(data_final, data[[2]],data_peaks))
}
