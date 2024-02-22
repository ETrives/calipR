#' keep_best_peaks
#'
#'This function is optional in the workflow. It tries to identify false positives
#'to remove them. It is based on two parameters : (1) the number of value pairs
#'(-1 and +1 around the peak, -2 and +2 around the peak) that have fluorescence values
#'below the peak value (2) if the cell's fluorescence values follow a bimodal
#'distribution. Each of these parameters are used in a model to predict the
#'likelihood of a cell being a false responder. These parameters are abberant
#'when the background have not been estimated correctly (eg. when fluorescence trace
#'has a sigmoid shape (this case is badly handled by the DPA method and creates a lot
#'of false positives). Background estimation with pattern matching overcomes
#'this problem and deals much better with tricky shapes.
#'
#' @param data the whole output from find_peaks function
#' @param range
#'
#' @return the same objects with an added column named True_Peaks which is TRUE if the peak is kept, FALSE if not
#' @export
#'
#' @examples
keep_best_peaks <- function(data){


  peaks_list <- split(data[[1]],cumsum(1:nrow(data[[1]]) %in% seq(1:nrow(data[[1]]))))


  peaks_list <- lapply(peaks_list, function(x) x[, infValue :=  inferiorValues(x, data[[2]])])

  peaks_list <- lapply(peaks_list, function(x) x[, bimod_peak :=  bimodal_cell(x, data[[2]])])


  data_peaks <- do.call(rbind, peaks_list)

  model <- readRDS(system.file("model/model.rds", package = "calipR"))

  data_peaks[, Prediction := stats::predict(model, data_peaks, type="response")]
  data_final <- data_peaks[data_peaks$Prediction > 0.5,]


  return(list(data_final, data[[2]]))
}



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

#' bimodal_cell
#'
#' @param peak
#' @param full_data
#'
#' @return
#' @export
#'
#' @examples
bimodal_cell <- function(peak, full_data){

  data <- full_data[full_data$Cell_id == peak$Cell_id & full_data$time_frame %between% list(peak$Max_peak_frame - 30,peak$Max_peak_frame + 30),]
  bimod <- LaplacesDemon::is.bimodal(data$Mean_Grey)

  return(bimod)
}
