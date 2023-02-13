#' clean_data
#'
#' This function removes cells that are moving, dying, not responding or
#' with mean fluorescence values that are deviating more than n sd from the
#' population's sd
#'
#' @param moving_threshold
#' @param outlier_threshold
#' @param mean_width
#' @param DPA_width
#' @param CN_DPA_width
#' @param mean_width_diff
#' @param mean_width_second
#' @param data a data.table output from prepareData function
#'
#' @return
#' @export
#'
#' @examples
clean_data <- function(data, moving_threshold, outlier_threshold ,mean_width, DPA_width, CN_DPA_width, mean_width_diff, mean_width_second){

  ncells_before <- length(unique(data$Cell_id))

  print(paste("Number of cells before cleaning", ncells_before, sep = ": " ))

  # Splitting the data frame by cell_id
  cell_split <- split(data, data$Cell_id)


  # Removing Cells with too much Nas :

  #cell_split <- lapply(cell_split, function(x) if(length(do.call(rbind, lapply(is.na(x$Mean_Grey), function(x) if(x == TRUE) x))[,1])/length(x$Mean_Grey) < moving_threshold) {x} )

  cell_split <- lapply(cell_split, function(x) if((length(do.call(rbind, lapply(is.na(x$Mean_Grey), function(x) if(x == TRUE) x))[,1]) / length(x$Mean_Grey)) < moving_threshold) x)

  cell_split <- cell_split[cell_split != "NULL"]

  # Computing a local mean for each data.table
  cell_split <- lapply(cell_split, function(x) data.table::setDT(x)[, local_mean := gplots::wapply(x$time_frame, x$Mean_Grey, fun = mean, n=length(x$time_frame), width = mean_width, method = "nobs")[[2]]])



  #cell_split <- lapply(cell_split, function(x) x$Mean_Grey[x$time_frame == 1 & x$Mean_Grey == 0] <- x$local_mean[x$time_frame == 1 & x$Mean_Grey == 0])
  cell_split <- lapply(cell_split, function(x) x[, Mean_Grey := replace(Mean_Grey, time_frame == 1 & Mean_Grey == 0, local_mean[1])])


  # Computing first derivative
  cell_split <- lapply(cell_split, function(x) data.table::setDT(x)[, first_derivative := doremi::calculate.gold(time = x$time_seconds, signal = x$local_mean,
                                                                                                                 embedding = 2, n = 1)$dsignal[,2]])


  cell_split <- lapply(cell_split, function(x) x[ , smooth_Diff := gplots::wapply(x$time_frame, x$first_derivative, fun = mean, n =length(data$time_frame), width = mean_width_diff, method = "nobs", drop.na = FALSE)[[2]]])

  print(cell_split[[1]])

  cell_split <- lapply(cell_split, function(x) data.table::setDT(x)[, c("DPA", "CN_DPA") := list(DPA(x, DPA_width),  CN_DPA(x, CN_DPA_width))])


  print("DPA DOOONE")

  print("CN_DPA DOOONE")

  cell_split <- lapply(cell_split, function(x) x[, c("DPA", "CN_DPA") :=
                            list(replace(DPA, is.na(DPA),quantile_speed(DPA, probs = .5)),
                            replace(CN_DPA, is.na(CN_DPA), quantile_speed(CN_DPA, probs = .5)))])


  cell_split <- lapply(cell_split, function(x) data.table::setDT(x)[,
                      c("smooth_DPA", "smooth_CN_DPA") := list(gplots::wapply(x$time_frame, x$DPA, fun = mean, n=length(x$time_frame), width = 10, method = "nobs")[[2]],
                      gplots::wapply(x$time_frame, x$CN_DPA, fun = mean, n=length(x$time_frame), width = 10, method = "nobs")[[2]])])



  cell_split <- lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := replace(Mean_Grey, smooth_CN_DPA > quantile_speed(smooth_CN_DPA, probs = .7, na.rm = T)|
                                                                                   Mean_Grey < (mean(Mean_Grey) - 1.5*stats::sd(Mean_Grey)),
                                                                                 NaN)])

  print(cell_split[[1]])

  cell_split <- lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := replace(Mean_Grey_wo_peaks, time_frame == max(time_frame) & is.na(Mean_Grey_wo_peaks), local_mean[ max(time_frame)])])
  cell_split <- lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := replace(Mean_Grey_wo_peaks, time_frame == 1 & is.na(Mean_Grey_wo_peaks), local_mean[1])])

  print(cell_split[[1]])

  lapply(cell_split, function(x) if(length(is.na(x$Mean_Grey_wo_peaks)[is.na(x$Mean_Grey_wo_peaks)[TRUE]]) == length(x$Mean_Grey_wo_peaks)) {print(x)})

  #cell_split <- lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := replace(Mean_Grey, smooth_DPA > quantile_speed(smooth_DPA, probs = .5)
                                                                                # |Mean_Grey < (mean(Mean_Grey) - 2*stats::sd(Mean_Grey)),
                                                                                # NaN)])




  cell_split <- lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := approxfun(which(!is.na(Mean_Grey_wo_peaks)), na.omit(Mean_Grey_wo_peaks))(seq_along(Mean_Grey_wo_peaks))])

print("OKKKK")

  data <- do.call(rbind, cell_split)

  data$Mean_Grey_wo_peaks[which(is.na(data$Mean_Grey_wo_peaks))] <- data$local_mean[which(is.na(data$Mean_Grey_wo_peaks))]


  cell_split <- split(data, data$Cell_id)


  cell_split <- lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := gplots::wapply(x$time_frame, x$Mean_Grey_wo_peaks,
                                                                                        fun = function(x) quantile_speed(x, probs = .1), n = length(x$time_frame), width = 30, method = "nobs")[[2]]])

  data <- do.call(rbind, cell_split)
  data$Mean_Grey_wo_peaks[which(is.na(data$Mean_Grey_wo_peaks))] <- data$local_mean[which(is.na(data$Mean_Grey_wo_peaks))]

  cell_split <- split(data, data$Cell_id)

  data <- moving_cells_old(cell_split, threshold = moving_threshold)

  data <- do.call(rbind, cell_split)

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
moving_cells_old <- function(data, threshold = 0.1){

  data <- lapply(data, function(x) data.table::setDT(x)[, Moving_cells := sum(Mean_Grey == 0) / sum(Mean_Grey >= 0) > threshold])

  data <- lapply(data, function(x) data.table::setDT(x)[, Anormal_variation := LaplacesDemon::is.bimodal(Mean_Grey)])

  return(data)
}


