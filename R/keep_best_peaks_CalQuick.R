#' keep_best_peaks
#'
#' @param data the whole output from find_peaks function
#' @param range
#'
#' @return the same objects with an added column named True_Peaks which is TRUE if the peak is kept, FALSE if not
#' @export
#'
#' @examples
keep_best_peaks <- function(data, range){

  data[[1]] <- data.table::setDT(data[[1]])[, True_peak := Max_peak_smooth_z == max(Max_peak_smooth_z), by = list(Cell_id, Max_peak_stimulus)]
  data[[2]] <- data.table::setDT(data[[2]])[, True_peak := smooth_z == max(smooth_z), by = list(Cell_id, stimulus)]

  print(data[[1]])

  print(data[[2]])

  peaks_data  <- data[[1]][data[[1]]$True_peak == TRUE,]
  peaks_list <- split(peaks_data,cumsum(1:nrow(peaks_data) %in% seq(1:nrow(peaks_data))))

  #peaks_list <- lapply(peaks_data, function(x) x[, triangleRatio := triangleRatio(full_data, x)])


  #peaks_list <- lapply(peaks_list, function(x)  x[, triangleRatio := triangleRatio(data[[2]], x)[[1]]])


  #peaks_list <- lapply(peaks_list, function(x) x[, trianglePoints :=  triangleRatio(data[[2]], x)[2]])

  #peaks_list <- lapply(peaks_list, function(x) x[, posArea :=  triangleRatio(data[[2]], x)[3]])

  #peaks_list <- lapply(peaks_list, function(x) x[, negArea :=  triangleRatio(data[[2]], x)[4]])

  peaks_list <- lapply(peaks_list, function(x) x[, infValue :=  inferiorValues(x, data[[2]])])

  peaks_list <- lapply(peaks_list, function(x) x[, bimod_peak :=  bimodal_cell(x, data[[2]])])

  #peaks_list <- lapply(peaks_list, function(x) x[, infValue_D :=  inferiorValues_lag(x, data[[2]])])


  #peaks_list <- lapply(peaks_list, function(x) x[, infValue_D :=  inferiorValues_lag(x, data[[2]])])

  #peaks_list <- lapply(peaks_list, function(x) x[, local_mean_diff := data[[2]][data[[2]]$Cell_id == x$Cell_id]$local_mean_diff[1]])


  data_peaks <- do.call(rbind, peaks_list)

  model <- readRDS("model.rds")

  data_peaks[, Prediction := predict(model, data_peaks, type="response")]

  data_final <- data_peaks[data_peaks$Prediction > 0.1,]
  #data_final <- data_peaks
  View(data)
  #true_peaks <- PNHRA(data[[1]], data[[2]], range)
  #true_peaks <- false_pos(data[[1]], data[[2]], range)


  #return(list(true_peaks, data[[2]]))

  return(list(data_final, data[[2]],data_peaks))
}
