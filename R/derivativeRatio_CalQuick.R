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

  print(peaks_list)
  print(full_data)
  full_data_peaks <- lapply(peaks_list, function(x) full_data[full_data$Cell_id == x$Cell_id & full_data$time_frame %between% list((x$Max_peak_frame - 50), (x$Max_peak_frame + 50)),] )

  lapply(full_data_peaks, function(x) if(x$Cell_id[1] == "cbau" |x$Cell_id[1] == "cazy"|x$Cell_id[1] == "casi"|x$Cell_id[1] == "cakb"
                                         |x$Cell_id[1] == "caly" |x$Cell_id[1] == "canu"|x$Cell_id[1] == "caoa"|x$Cell_id[1] == "caxe"
                                         |x$Cell_id[1] == "caxi") {View(x)})

  responders <- unique(peaks_data$Cell_id)


  full_data_peaks <- lapply(full_data_peaks, function(x) x[ , derivative_peak := max(smooth_Diff, na.rm = TRUE)[1]])

  full_data_peaks <- lapply(full_data_peaks, function(x) x[, derivative_peak_frame:= x[x$smooth_Diff == derivative_peak,]$time_frame[1]])


  full_data_peaks <- lapply(full_data_peaks, function(x) x[, first_zero := x$time_frame == x[x$time_frame %between%
                                                                    list(x$derivative_peak_frame, max(x$time_frame)) & x$smooth_Diff <= 0,][1,]$time_frame])


  lapply(full_data_peaks, function(x) if(x$Cell_id[1] == "cbau" |x$Cell_id[1] == "cazy"|x$Cell_id[1] == "casi"|x$Cell_id[1] == "cakb"
                                         |x$Cell_id[1] == "caly" |x$Cell_id[1] == "canu"|x$Cell_id[1] == "caoa"|x$Cell_id[1] == "caxe"
                                         |x$Cell_id[1] == "caxi") {View(x)})

  peaks_list <- lapply(seq(range,20, by = 5), function(x) peaks_data[, paste("window", x, sep="_") := derivativeRatio(full_data_peaks, x)])[[1]]


  #splitting each peak as an element of a list
  peaks_list <- split(peaks_data,cumsum(1:nrow(peaks_data) %in% seq(1:nrow(peaks_data))))


  peaks_list <- lapply(peaks_list, function(x) tidyr::gather(x,
                            key = "Window",
                            value = "Ratio", 7:length(x)))



  peaks_list <- lapply(peaks_list, function(x) data.table::setDT(x)[, Ratio := replace(Ratio, is.infinite(Ratio),NaN)])


  peaks_list <- lapply(peaks_list, function(x) x[, Median_Ratio := if(sum(is.na(Ratio) == TRUE) == length(Ratio))
              {NaN} else {quantile_speed(x$Ratio, probs = .5, na.rm = T)}])


  peaks <- do.call(rbind, peaks_list)

  View(peaks)
  #View(peaks[peaks$Median_Ratio > 1.5 | peaks$Median_Ratio < 0.7 | is.na(peaks$Median_Ratio) == TRUE,])

  peaks_final <- peaks[is.na(peaks$Median_Ratio) == FALSE, ]

  peaks_final <- peaks[min(Ratio) > 0.2, , by = ]


  peaks_final <- unique(peaks_final[,c(1:6)])

  #peaks_full <- peaks

  #View(peaks)
  #View(peaks_full)

return(list(peaks_final, peaks))


}

derivativeRatio <- function(full_data_peaks, range){


  # Defining the range in which to find before and after the peak to find the
  # start/end

  print("inside derivativeRatio")
  full_data_peaks <- lapply(full_data_peaks, function(x) x[,start_window := x$time_frame - as.integer(range)])
  full_data_peaks <- lapply(full_data_peaks, function(x) x[,end_window := x$time_frame + as.integer(range)])


  window <- lapply(full_data_peaks, function(x) x[x$time_frame %between% list(x[x$first_zero==TRUE,]$start_window, x[x$first_zero==TRUE,]$end_window),])

  #print(window)
  start_pos <- lapply(window, function(x) length(x[x$smooth_Diff > 2,]$smooth_Diff))

  #window <- lapply(window, function(x) if(max(x$smooth_Diff, na.rm = TRUE) > 2 & min(x$smooth_Diff, na.rm = TRUE) < -2){x})


  end_neg <- lapply(window, function(x) length(x[x$smooth_Diff < -2,]$smooth_Diff))

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
