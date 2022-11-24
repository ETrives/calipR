#' clean_data
#'
#' This function removes cells that are moving, dying, not responding or
#' with mean fluorescence values that are deviating more than n sd from the
#' population's sd
#'
#' @param data a data.table output from prepareData function
#' @param threshold a number of sd above which : a cell's mean fluorescence is
#' considered an outlier, an ending median fluorescence is too high compared with
#' the minimum median fluorescence
#'
#' @param width the number of frames that will be taken to compute the
#' running average (this is used to smooth the data to then fit a linear model
#' and exclude cells with a fit (adjusted R2) above 80% )
#'
#' @return
#' @export
#'
#' @examples
  clean_data <- function(data, moving_threshold, outlier_threshold ,mean_width, DPA_width, mean_width_diff, mean_width_second){

  ncells_before <- length(unique(data$Cell_id))

  print(paste("Number of cells before cleaning", ncells_before, sep = ": " ))

  # Splitting the data frame by cell_id
  cell_split <- split(data, data$Cell_id)



  # Computing a local mean for each data.table
  cell_split <- lapply(cell_split, function(x) data.table::setDT(x)[, local_mean := gplots::wapply(x$time_frame, x$Mean_Grey, fun = mean, n=length(x$time_frame), width = mean_width, method = "nobs")[[2]]])


  # Computing first derivative
  cell_split <- lapply(cell_split, function(x) x[, first_derivative := doremi::calculate.gold(time = x$time_seconds, signal = x$local_mean,
                                                                                              embedding = 2, n = 1)$dsignal[,2]])



  cell_split <- lapply(cell_split, function(x) DPA(x, DPA_width, mean_width_diff))

  cell_split <- lapply(cell_split, function(x) x[, DPA := replace(DPA, is.na(DPA), quantile_speed(DPA, probs = .5))])

  #cell_split <- lapply(cell_split, function(x) x$first_derivative[which(is.na(x$first_derivative))] <- x$local_mean[which(is.na(x$first_derivative))])

  #test_clean_1Hz$first_derivative[which(is.na(test_clean_1Hz$first_derivative))] <- test_clean_1Hz$local_mean[which(is.na(test_clean_1Hz$first_derivative))]


  #cell_split <- lapply(cell_split, function(x) lapply(x, function(y) print(which(y == is.na(y)))))

  #x$DPA[x$DPA] <- (x$local_mean[x$DPA == 0] - 5)/2.533

  #y[which(x == max(x, na.rm=TRUE))[1],]

  print(cell_split[[1]])

  cell_split <- lapply(cell_split, function(x) data.table::setDT(x)[, smooth_DPA := gplots::wapply(x$time_frame, x$DPA, fun = mean, n=length(x$time_frame), width = 10, method = "nobs")[[2]]])


  cell_split <- lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := replace(Mean_Grey, smooth_DPA > quantile_speed(smooth_DPA, probs = .5)
                                                                                 |Mean_Grey < (mean(Mean_Grey) - 2*stats::sd(Mean_Grey)),
                                                                                 NaN)])



  cell_split <- lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := approxfun(which(!is.na(Mean_Grey_wo_peaks)), na.omit(Mean_Grey_wo_peaks))(seq_along(Mean_Grey_wo_peaks))])

  #cell_split <- lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := replace(Mean_Grey_wo_peaks, is.na(Mean_Grey_wo_peaks), quantile_speed(Mean_Grey_wo_peaks, probs = .5))])

  data <- do.call(rbind, cell_split)
  data$Mean_Grey_wo_peaks[which(is.na(data$Mean_Grey_wo_peaks))] <- data$local_mean[which(is.na(data$Mean_Grey_wo_peaks))]


  print(data)
  print("first DPA done")

  #cell_split <- lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := replace(Mean_Grey_wo_peaks, Mean_Grey_wo_peaks > quantile_speed(DPA, probs = .5, na.rm = T)
                                                                                # & smooth_DPA > quantile_speed(smooth_DPA, probs = .5) ,NaN)])

  #cell_split <- lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := approxfun(which(!is.na(Mean_Grey_wo_peaks)), na.omit(Mean_Grey_wo_peaks))(seq_along(Mean_Grey_wo_peaks))])

  #data <- do.call(rbind, cell_split)
  #data$Mean_Grey_wo_peaks[which(is.na(data$Mean_Grey_wo_peaks))] <- data$local_mean[which(is.na(data$Mean_Grey_wo_peaks))]



  cell_split <- split(data, data$Cell_id)


  cell_split <- lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := gplots::wapply(x$time_frame, x$Mean_Grey_wo_peaks,
                                                                                    fun = function(x) quantile_speed(x, probs = .1), n = length(x$time_frame), width = 30, method = "nobs")[[2]]])

  #cell_split <- lapply(cell_split, function(x) x[, quantile_detrended := Mean_Grey - local_quantile])

  data <- do.call(rbind, cell_split)
  data$Mean_Grey_wo_peaks[which(is.na(data$Mean_Grey_wo_peaks))] <- data$local_mean[which(is.na(data$Mean_Grey_wo_peaks))]

  cell_split <- split(data, data$Cell_id)

  data <- moving_cells(cell_split, threshold = moving_threshold)

  print(paste("Number of cells after cleaning", length(unique(data$Cell_id)), sep = ": " ))

  print(paste("Removed", ncells_before - length(unique(data$Cell_id)), "cells", sep = " " ))

  cell_split <- split(data, data$Cell_id)


  return(data)

}





#' moving_cells
#'
#' @param data
#' @param threshold An optional parameter defining over how many standard deviations (of the baseline)
#' a decrease in median during a stimulus will be considered a moving cell and thus discarded.
#'
#' @return a data frame with all moving cells removed
#' @export
#'
#' @examples
moving_cells <- function(data, threshold = 0.1){

  data <- lapply(data, function(x) data.table::setDT(x)[, Moving_cells
                                                        := sum(Mean_Grey == 0) / sum(Mean_Grey >= 0) > threshold])

  #print(data)
  data <- lapply(data, function(x) data.table::setDT(x)[, Anormal_variation := LaplacesDemon::is.bimodal(Mean_Grey)])

  #data <- lapply(data, function(x) x[Moving_cells == FALSE & Anormal_variation == FALSE,])

  data <- lapply(data, function(x) x[Moving_cells == FALSE,])

  data <- do.call(rbind, data)

  return(data)
}


