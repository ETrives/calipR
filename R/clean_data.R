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
#' @param mean_width_diff
#' @param data a data.table output from prepareData function
#'
#' @return
#' @export
#'
#' @examples
clean_data <- function(data, moving_threshold, outlier_threshold, mean_width, DPA_width, mean_width_diff){

  ncells_before <- length(unique(data$Cell_id))

  print(paste("Number of cells before cleaning", ncells_before, sep = ": " ))

  # Splitting the data frame by cell_id
  cell_split <- split(data, data$Cell_id)


  # Removing Cells with too much Nas :


  cell_split <- lapply(cell_split, function(x) if((length(do.call(rbind, lapply(is.na(x$Mean_Grey), function(x) if(x == TRUE) x))[,1]) / length(x$Mean_Grey)) < moving_threshold) x)

  cell_split <- cell_split[cell_split != "NULL"]

  # Computing a local mean for each data.table
  cell_split <- lapply(cell_split, function(x) data.table::setDT(x)[, local_mean :=
                          gplots::wapply(x$time_frame, x$Mean_Grey, fun = mean, n=length(x$time_frame), width = mean_width, method = "nobs")[[2]]])


  cell_split <- lapply(cell_split, function(x) data.table::setDT(x)[, Mean_Grey :=
                          replace(Mean_Grey, time_frame == 1 & Mean_Grey == 0, local_mean[1])])


  # Computing first derivative
  cell_split <- lapply(cell_split, function(x) data.table::setDT(x)[, first_derivative := doremi::calculate.gold(time = x$time_seconds, signal = x$local_mean,
                                                                                                                 embedding = 2, n = 1)$dsignal[,2]])


  cell_split <- lapply(cell_split, function(x) x[ , smooth_Diff := gplots::wapply(x$time_frame, x$first_derivative, fun = mean, n =length(data$time_frame), width = mean_width_diff, method = "nobs", drop.na = FALSE)[[2]]])


  cell_split <- lapply(cell_split, function(x) data.table::setDT(x)[, DPA := DPA(x, DPA_width)])


  cell_split <- lapply(cell_split, function(x) data.table::setDT(x)[, DPA := replace(DPA, is.na(DPA),quantile_speed(DPA, probs = .5))])


  cell_split <- lapply(cell_split, function(x) data.table::setDT(x)[, smooth_DPA := gplots::wapply(x$time_frame, x$DPA, fun = mean, n=length(x$time_frame), width = 10, method = "nobs")[[2]]])

  cell_split <- lapply(cell_split, function(x) if(moving_cells(x, moving_threshold)[["Moving_cells"]][1] == FALSE) {x} else{})

  cell_split <- cell_split[cell_split != "NULL"]


  print(paste("Number of cells after cleaning", length(cell_split), sep = ": " ))

  print(paste("Removed", ncells_before - length(cell_split), "cells", sep = " " ))


  data <- do.call(rbind, cell_split)

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
  data <- data.table::setDT(data)[, Moving_cells := sum(Mean_Grey == 0) / sum(Mean_Grey >= 0) > threshold]

  data <- data.table::setDT(data)[, Anormal_variation := LaplacesDemon::is.bimodal(Mean_Grey)]

  return(data)
}

