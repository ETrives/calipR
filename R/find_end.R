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

