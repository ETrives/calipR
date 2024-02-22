
#' find_borders
#'
#' This function finds the start and the end of each peak that has been found
#' from the previous step
#'
#'
#' @param data
#' @param range
#'
#' @return
#' @export
#'
#' @examples
find_borders <- function(data, range){


True_peaks <- data[[1]]
dt_2 <- data[[2]]


# Defining the range in which to find before and after the peak to find the
# start/end

True_peaks$start_window <- unlist(lapply(True_peaks$Max_peak_frame, function(x) x - as.integer(range)))
True_peaks$end_window <- unlist(lapply(True_peaks$Max_peak_frame, function(x) x + as.integer(range)))

# Splitting the first data table into a list of data table with each element
# containing the informations regarding one peak

find_1 <- split(True_peaks,cumsum(1:nrow(True_peaks) %in% seq(1:nrow(True_peaks))))

# Find start of peak
start_area_list <- find_start(find_1, dt_2, range)
peak_start <- do.call(rbind, start_area_list)
peak_start$Cell_id == start_area_list$Cell_id


# Find end of peak
end_area_list <- find_end(find_1, dt_2, range)
peak_end <- do.call(rbind, end_area_list)


True_peaks$Start_peak_frame <- peak_start$time_frame
True_peaks$Start_peak_derivative <- peak_start$smooth_Diff

True_peaks$Start_peak_stimulus <- peak_start$stimulus
True_peaks$Start_peak_rel_frame <- peak_start$Time_frame_stim
True_peaks$Start_peak_stimulation <- peak_start$Stimulation

True_peaks$End_peak_frame <- peak_end$time_frame
True_peaks$End_peak_derivative <- peak_end$smooth_Diff

True_peaks$End_peak_frame <- peak_end$time_frame

True_peaks$End_peak_rel_frame <- peak_end$Time_frame_stim

True_peaks$group <- peak_start$group


peaks <- split(True_peaks,cumsum(1:nrow(True_peaks) %in% seq(1:nrow(True_peaks))))

isnot.na <- Negate(is.na)

peaks <- lapply(peaks, function(x) if(isnot.na(x$End_peak_frame)) {x})

True_peaks <- do.call(rbind, peaks)

return(list(True_peaks, peak_start, peak_end))


}


#' find_start
#'
#' This function finds the start of each peak in the dataset. It is based on
#' the trapezoid method as described in :
#' https://www.researchgate.net/profile/Roberto-Oliveira-7/publication/51637823_New_approach_for_T-wave_end_detection_on_electrocardiogram_Performance_in_noisy_conditions/links/004635204e355e4bed000000/New-approach-for-T-wave-end-detection-on-electrocardiogram-Performance-in-noisy-conditions.pdf?_sg%5B0%5D=e63wPE5M2qk78DidbANkTHjPauBikqoopZck3Hrj9eqqA7w5KrtUB6sNLfqSW9SW7ZYgaWkL9pb9kBdKmhL0mg.3bSqx9fAuEE_4YmwlOnGgFtq-7uSsouXgfLVmhw0abvkwJGoWvnjWJI6TSHCNR_tl-MXl4b4xIQOsHC3nsQtTQ&_sg%5B1%5D=xG1pCg_18Q1boMc0hcDjNAb7_J1kIvrvE1BJ9Wzfq0xiVLYXGhpICPqjvK2tv0LLi7FtXzbwchBqH9y2zpLFyWRMC-SqMuU7UT2NTqwL7MwE.3bSqx9fAuEE_4YmwlOnGgFtq-7uSsouXgfLVmhw0abvkwJGoWvnjWJI6TSHCNR_tl-MXl4b4xIQOsHC3nsQtTQ&_sg%5B2%5D=okeeSpIP3PH3D6113upLcK4TWZ5hLfbDzb-p2-Cf-VnfnszQIU3SXbz9kQ-v188m9_jCJq74Vrw1SuI.R-cwcRofo6lYn_UKG_b_vpgEaw3REI89B7mUiQWIOH_cTGwL6nHAXMwO5ZTDD73QR1ToSWp9OmiTl8W4LBPdHA&_iepl=
#'
#'
#' @param peaks A list of dataframe with each element being the informations of a peak
#' @param full_data
#' @param range the time window after the maxima of the peak in wich to look for the end
#'
#' @return A list of data table objects with each element being the informations regarding the start of one peak
#' @export
#'
#' @examples
find_start <- function(peaks, full_data, range){

  print("inside find_start")
  df_list_start <- lapply(peaks, subset_window, dt_2 = full_data, period = "start")


  df_list_start <- lapply(df_list_start, function(x) x[order(-x$time_frame),])


  df_list_start_sub <- lapply(df_list_start, function(x) x[1:10,])


  XmYm_start <- lapply(df_list_start_sub, function(x) x[x$first_derivative == max(x$first_derivative, na.rm = TRUE)[1],])



  XrYr_start <- lapply(df_list_start, function(x) x[x$time_frame == min(x$time_frame)[1],])


  start_area_list <- purrr::pmap(list(df_list_start, XmYm_start, XrYr_start),
                                 function(x,y,z) unlist(lapply(x$time_frame, function(a)
                                   0.5*((y$DPA - x[x$time_frame == a,]$DPA)* (2*y$time_frame  - (max(x$time_frame) - x[x$time_frame == a,]$time_frame)
                                                                              - z$time_frame)))))


  df_list_start <- purrr::map2(start_area_list, df_list_start, function(x,y) y[which(x == max(x, na.rm=TRUE))[1],])


  return(df_list_start)
}


#' find_end
#'
#' This function finds the end of each peak in the dataset. It is based on
#' the trapezoid method as described in :
#' https://www.researchgate.net/profile/Roberto-Oliveira-7/publication/51637823_New_approach_for_T-wave_end_detection_on_electrocardiogram_Performance_in_noisy_conditions/links/004635204e355e4bed000000/New-approach-for-T-wave-end-detection-on-electrocardiogram-Performance-in-noisy-conditions.pdf?_sg%5B0%5D=e63wPE5M2qk78DidbANkTHjPauBikqoopZck3Hrj9eqqA7w5KrtUB6sNLfqSW9SW7ZYgaWkL9pb9kBdKmhL0mg.3bSqx9fAuEE_4YmwlOnGgFtq-7uSsouXgfLVmhw0abvkwJGoWvnjWJI6TSHCNR_tl-MXl4b4xIQOsHC3nsQtTQ&_sg%5B1%5D=xG1pCg_18Q1boMc0hcDjNAb7_J1kIvrvE1BJ9Wzfq0xiVLYXGhpICPqjvK2tv0LLi7FtXzbwchBqH9y2zpLFyWRMC-SqMuU7UT2NTqwL7MwE.3bSqx9fAuEE_4YmwlOnGgFtq-7uSsouXgfLVmhw0abvkwJGoWvnjWJI6TSHCNR_tl-MXl4b4xIQOsHC3nsQtTQ&_sg%5B2%5D=okeeSpIP3PH3D6113upLcK4TWZ5hLfbDzb-p2-Cf-VnfnszQIU3SXbz9kQ-v188m9_jCJq74Vrw1SuI.R-cwcRofo6lYn_UKG_b_vpgEaw3REI89B7mUiQWIOH_cTGwL6nHAXMwO5ZTDD73QR1ToSWp9OmiTl8W4LBPdHA&_iepl=
#'
#'
#' @param peaks A list of dataframe with each element being the informations of a peak
#' @param full_data
#' @param range the time window after the maxima of the peak in wich to look for the end
#'
#' @return A list of data table objects with each element being the informations regarding the end of one peak
#' @export
#'
#' @examples
find_end <- function(peaks, full_data, range){

  df_list_end <- lapply(peaks, subset_window, dt_2 = full_data, period = "end")

  df_list_end_sub <- lapply(df_list_end, function(x) x[1:20,])


  XmYm_end <- lapply(df_list_end_sub, function(x) x[x$first_derivative == min(x$first_derivative, na.rm = TRUE),])

  #XrYr_end <- lapply(df_list_end, function(x) x[x$time_frame == max(x$time_frame, na.rm = TRUE),])

  XrYr_end <- lapply(df_list_end, function(x) x[x$local_mean == min(x$local_mean, na.rm = TRUE),])


  #lapply(XmYm_end, function(x) if(x$Cell_id[1] == "cahr") {print(x)})

  #lapply(XrYr_end, function(x) if(x$Cell_id[1] == "cahr") {print(x)})

  # Computing the area of the trapezoid for each i :

  end_area_list <- purrr::pmap(list(df_list_end, XmYm_end, XrYr_end),
                               function(x,y,z) unlist(lapply(x$time_frame, function(a) 0.5*((y$deconvolved_trace -
                                                                                               x[x$time_frame == a,]$deconvolved_trace)*((2*z$time_frame) - x[x$time_frame == a,]$time_frame) - (y$time_frame)))))


  df_list_end <- purrr::map2(end_area_list, df_list_end, function(x,y) y[which(x == max(x, na.rm=TRUE))[1],])

  print("find_end = done")
  return(df_list_end)
}



#' subset_window
#'
#'This function is defined to be used inside an lapply function. It subsets 50 frames
#'before a peak and stores it in a new data table. For each peak, there will be a corresponding
#'data table with only the 50 frames before the peak.
#'
#' @param dt_2 a data table containing the entire dataset (first ouput from resolve_mult_peaks function)
#' @param x a data table containing the informations regarding a given peak (second output from resolve_mult_peaks)
#'
#' @return a data table object subset with only 50 frames before a given peak
#' @export
#'
#' @importFrom data.table %between%
#'
#' @examples
subset_window <- function(dt_2, x, period = c("start", "end")) {

  if(period == "start") {

    dt <- dt_2[dt_2$Cell_id == x$Cell_id & dt_2$time_frame %between% list(x$start_window, x$Max_peak_frame),]

  }

  if(period == "end") {

    dt <- dt_2[dt_2$Cell_id == x$Cell_id & dt_2$time_frame %between% list(x$Max_peak_frame, x$end_window),]

  }

  return(dt)
}

