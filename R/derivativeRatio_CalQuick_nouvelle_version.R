#' derivativeRatio
#'
#' @param peaks_data
#' @param full_data
#' @param range
#'
#' @return
#' @export
#'
#' @examples
PNHRA <- function(peaks_data, full_data, range){


  peaks_data <- peaks_data[peaks_data$True_peak == TRUE,]

  peaks_list <- split(peaks_data,cumsum(1:nrow(peaks_data) %in% seq(1:nrow(peaks_data))))

  peaks_list <- lapply(seq(range,30, by = 5), function(x) peaks_data[, paste("window", x, sep="_") := derivativeRatio(full_data, peaks_list, x)])[[1]]


  #splitting each peak as an element of a list
  peaks_list <- split(peaks_data,cumsum(1:nrow(peaks_data) %in% seq(1:nrow(peaks_data))))


  peaks_list <- lapply(peaks_list, function(x) tidyr::gather(x,
                                                             key = "Window",
                                                             value = "Ratio", 7:length(x)))



  peaks_list <- lapply(peaks_list, function(x) data.table::setDT(x)[, Ratio := replace(Ratio, is.infinite(Ratio),NaN)])


  peaks_list <- lapply(peaks_list, function(x) x[, Median_Ratio := if(sum(is.na(Ratio) == TRUE) >= length(Ratio)/2)
  {NaN} else {quantile_speed(x$Ratio, probs = .5, na.rm = T)}])

  peaks_list <- lapply(peaks_list, function(x) x[, sd := stats::sd(x$Ratio)])

  peaks <- do.call(rbind, peaks_list)

  View(peaks)
  #peaks_final <- peaks[is.na(peaks$Median_Ratio) == FALSE & peaks$Median_Ratio >= 0.8 & peaks$Median_Ratio < 2.5, ]

  peaks_final <- peaks
  View(peaks_final)
  #peaks_final <- unique(peaks_final[,c(1:6)])

  return(list(peaks_final, peaks))


}

derivativeRatio <- function(full_data, peaks_list, range){


  print("inside derivativeRatio")

  start_window <- lapply(peaks_list, function(x) full_data[full_data$Cell_id == x$Cell_id & full_data$time_frame %between% list((x$Max_peak_frame - range), x$Max_peak_frame),] )

  start_pos <- lapply(start_window, function(x) length(x[x$smooth_Diff > 2,]$smooth_Diff))

  end_window <- lapply(peaks_list, function(x) full_data[full_data$Cell_id == x$Cell_id & full_data$time_frame %between% list(x$Max_peak_frame, (x$Max_peak_frame + range)),] )


  #window <- lapply(window, function(x) if(max(x$smooth_Diff, na.rm = TRUE) > 2 & min(x$smooth_Diff, na.rm = TRUE) < -2){x})


  end_neg <- lapply(end_window, function(x) length(x[x$smooth_Diff < -1,]$smooth_Diff))

  ratio <- unlist(purrr::map2(start_pos, end_neg, function(x,y) unlist(x)/unlist(y) ))


  #data <- do.call(rbind, window)

  return(as.vector(ratio))
  #return(data)
}



### Analyse de la PNHRA :


plotDR <- function(data, full_data, cells, by = c("position", "name")) {

  if(by == "position"){
    cell_vec <- unique(data$Cell_id)
    print(cell_vec)
    print(cell_vec[1])

    cell_vec <- cell_vec[cells]
    print(cell_vec)

    data <- data[data$Cell_id %in% cell_vec,]
  }


  if(by == "name"){
    data <- data[data$Cell_id %in% cells,]
  }



  p <- ggplot(data = data)+
    geom_point(aes(x=Cell_id, y = Ratio))+
    theme_classic()

  plots <-lapply(unique(data$Cell_id), function(x) simple_cell_plot(data = full_data, cell= x, var = "Mean_Grey", line = "gam"))


  return(list(plots, p ))
}


#' derivativeRatio
#'
#' @param peaks_data
#' @param full_data
#' @param range
#'
#' @return
#' @export
#'
#' @examples
false_pos <- function(peaks_data, full_data, range){


  peaks_data <- peaks_data[peaks_data$True_peak == TRUE,]

  peaks_list <- split(peaks_data,cumsum(1:nrow(peaks_data) %in% seq(1:nrow(peaks_data))))

  full_data_peaks <- lapply(peaks_list, function(x) full_data[full_data$Cell_id == x$Cell_id & full_data$stimulus == x$Max_peak_stimulus,] )


  responders <- unique(peaks_data$Cell_id)


  full_data_peaks <- lapply(full_data_peaks, function(x) x[ , derivative_peak := max(smooth_Diff, na.rm = TRUE)[1]])

  full_data_peaks <- lapply(full_data_peaks, function(x) x[, derivative_peak_frame:= x[x$smooth_Diff == derivative_peak,]$time_frame[1]])



  full_data_peaks <- lapply(full_data_peaks, function(x) x[, first_zero := x$time_frame == x[x$time_frame %between%
                                                                                               list(x$derivative_peak_frame, max(x$time_frame)) & x$smooth_Diff <= 0,][1,]$time_frame])


  data <- derivativeRatio(full_data_peaks, 20)

  return(data)


}
