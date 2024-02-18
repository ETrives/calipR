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

print("subset = ok")

#df_list_start <- lapply(peaks, function(x) full_data[full_data$Cell_id == x$Cell_id])

df_list_start <- lapply(df_list_start, function(x) x[order(-x$time_frame),])

print("order = ok")

df_list_start_sub <- lapply(df_list_start, function(x) x[1:10,])

print("sub 10 lines = ok")

XmYm_start <- lapply(df_list_start_sub, function(x) x[x$first_derivative == max(x$first_derivative, na.rm = TRUE)[1],])



XrYr_start <- lapply(df_list_start, function(x) x[x$time_frame == min(x$time_frame)[1],])

#lapply(XmYm_start, function(x) if(x$Cell_id[1] == "cahr") {print(x)})

#lapply(XrYr_start, function(x) if(x$Cell_id[1] == "cahr") {print(x)})

start_area_list <- purrr::pmap(list(df_list_start, XmYm_start, XrYr_start),
                               function(x,y,z) unlist(lapply(x$time_frame, function(a)
                                 0.5*((y$DPA - x[x$time_frame == a,]$DPA)* (2*y$time_frame  - (max(x$time_frame) - x[x$time_frame == a,]$time_frame)
                                  - z$time_frame)))))


df_list_start <- purrr::map2(start_area_list, df_list_start, function(x,y) y[which(x == max(x, na.rm=TRUE))[1],])

print("find_start = done")


return(df_list_start)
}
