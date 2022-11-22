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


  cell_split <- lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := replace(Mean_Grey, DPA > quantile_speed(DPA, probs = .9) |
                                                                                                         Mean_Grey < (mean(Mean_Grey) - 3*stats::sd(Mean_Grey)),
                                                                                                       NaN)])


  cell_split <- lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := approxfun(which(!is.na(Mean_Grey_wo_peaks)), na.omit(Mean_Grey_wo_peaks))(seq_along(Mean_Grey_wo_peaks))])

  cell_split <- lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := replace(Mean_Grey_wo_peaks, is.na(Mean_Grey_wo_peaks), quantile_speed(Mean_Grey_wo_peaks, probs = .5))])

  cell_split <- lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := replace(Mean_Grey_wo_peaks, DPA > quantile_speed(DPA, probs = .8) &
                                                                                   Mean_Grey_wo_peaks > quantile_speed(Mean_Grey_wo_peaks, probs = .1)
                                                                                   , quantile_speed(Mean_Grey_wo_peaks, probs = .5))])


  data <- moving_cells(cell_split, threshold = moving_threshold)

  print(paste("Number of cells after cleaning", length(unique(data$Cell_id)), sep = ": " ))

  print(paste("Removed", ncells_before - length(unique(data$Cell_id)), "cells", sep = " " ))

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

  data <- lapply(data, function(x) data.table::setDT(x)[, Moving_cells := (sum(Mean_Grey == 0) / sum(Mean_Grey >= 0))  > threshold])

  data <- lapply(data, function(x) x[Moving_cells == FALSE,])

  data <- do.call(rbind, data)

  return(data)
}


