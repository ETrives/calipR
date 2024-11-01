### Working on peak extraction for AUC and other parameters :

#' peakExtractR
#'
#' @param peaks_data a data table with each line corresponding to a spike found
#' during the deconvolution. This is the first element of the list returned by
#' the deconvolve() function
#' @param norm_data a data table output from the norm_df() function
#' @param peak_frame an integer corresponding to the number of frame you want
#' to screen after each spike to find the peak maximum default to 10 (tested in 1 and 0.25Hz data)
#' @param threshold a numeric indicating the z score threshold a peak maximum must
#' exceed to be kept default to 3 (i.e. 3 standard deviations)
#' @param delta_threshold a numeric indicating the delta f/f threshold a peak maximum must
#' exceed to be kept default to 0
#' @param var the variable used for deconvolution (default to "background_detrended").
#' If you don't use the pattern matching option for noise estimation and use DPA method instead,
#' this parameter needs to be changed to "gam_detrended"
#'
#' @return a list with 3 elements : a data table with peaks data (with peak start and end estimated),
#' the norm data and a logical indicating if peaks have been found.
#' @export
#'
#' @examples
peakExtractR <- function(peaks_data, norm_data, peak_frame = 10, threshold = 3,
                         delta_threshold = 0, var = "background_detrended"){

  'isnotna' <- Negate('is.na')
  stim_list <- unique(norm_data$stimulus)

  if(length(peaks_data$Cell_id) != 0) {



  # For each spike, get it and the 21 lines that follow it then find the max peak
  # and only keep that line

  # Using an even peak_frame value is important
  peak_frame <- 2 * round(peak_frame/2)



  dt <- subset_spike_frames(norm_data,peaks_data, peak_frame = peak_frame/2)


  dt[, Max_by_20 := max(smooth_z, na.rm = TRUE)[[1]], by = .(blocs)]

  #Derivative criteria in calipR v1.0.1:
  dt[, peak_start_new := .SD[smooth_Diff_detrended > 0][1,]$time_frame, by = .(blocs)]
  dt[, peak_start_new := .SD[1,]$time_frame, by = .(blocs)]


  dt <- dt[Max_by_20 ==  smooth_z]
  dt <- dt[, duplicate := duplicated(blocs)][duplicate == FALSE]


  peaks_data$Max_peak_frame <- dt$time_frame
  peaks_data$max_peak_smooth_z <- dt$smooth_z
  peaks_data$max_peak_smooth_delta <- dt$delta_f_f
  peaks_data$peak_start <- dt$peak_start_new

  # now same but to find peak end :

  dt <- subset_spike_frames(norm_data,peaks_data, peak_frame = 5*peak_frame)

  dt[, Max_by_20 := max(smooth_z, na.rm = TRUE)[[1]], by = .(blocs)]

  dt[, smooth_Diff := ifelse(is.na(smooth_Diff_detrended), first_derivative_detrended,smooth_Diff_detrended), by = .(blocs)]

  dt[, peak_end := .SD[smooth_z <= 0 & smooth_Diff_detrended <= 0,][1,]$time_frame,  by = .(Cell_id,blocs)]

  dt[, peak_end := ifelse(is.na(peak_end),
                                .SD[time_frame > .SD[smooth_z == Max_by_20]$time_frame][smooth_z == min(smooth_z, na.rm = TRUE)]$time_frame, peak_end),  by = .(Cell_id,blocs)]

  dt <- dt[, duplicate := duplicated(blocs)][duplicate == FALSE]

  peaks_data$peak_end <- dt$peak_end

}

# Remove NAs

peaks_data <- peaks_data[isnotna(max_peak_smooth_z) & max_peak_smooth_z >= threshold & max_peak_smooth_delta >= delta_threshold]


if(dim(peaks_data)[1] != 0) {

  peaks_data <- dplyr::rename(peaks_data,  "spike_stimulus" = "stimulus", "spike_frame" = "time_frame", "spike_stimulation" = "Stimulation",
                              "spike_smooth_z" = "smooth_z", "spike_first_derivative" = "first_derivative", "spike_smooth_Diff" = "smooth_Diff_detrended")


  peaks_data <- unique(peaks_data[,c("Cell_id", "spike_frame", "spike_stimulus",
                                     "spike_smooth_z", "Mean_Grey",
                                     "Max_peak_frame", "max_peak_smooth_z",
                                     "spike_smooth_Diff",
                                     "Time_frame_stim", "Prev_stim", "peak_end","peak_start" )])



  ### Removing spikes detected during a descending phase :
  peaks_data <- data.table::setDT(peaks_data)[,lead_spike_smooth_Diff_1 := dplyr::lead(spike_smooth_Diff, 1, default = 0), by = Cell_id]
  peaks_data[,lead_spike_smooth_Diff_2 := dplyr::lead(spike_smooth_Diff, 2, default = 0), by = Cell_id]
  peaks_data[,lead_spike_smooth_Diff_3 := dplyr::lead(spike_smooth_Diff, 3, default = 0), by = Cell_id]

  peaks_data <- peaks_data[spike_smooth_Diff > 0 |lead_spike_smooth_Diff_3 > 0 |
                             lead_spike_smooth_Diff_2 > 0 |lead_spike_smooth_Diff_1 > 0 ]

  peaks_data[,diff := c(0,diff(spike_frame)), by = Cell_id]
  peaks_data[,labels := cumsum(diff > 10) , by = Cell_id]

  # Method to assign the first stimulus to a spike group :
  peaks_data[, new_stimulus :=  spike_stimulus[1],by = .(Cell_id,labels)]


  peaks_data[, new_new_stimulus := ifelse(Time_frame_stim[1] < 3, Prev_stim, new_stimulus), by = .(Cell_id, labels)]
  peaks_data[, new_new_stimulus := stim_list[new_new_stimulus]]
  peaks_data[, new_stimulus :=  new_new_stimulus]


  peaks <- TRUE

  peaks_data_i <- peaks_data

  # Identifying overlapping peaks to merge them :
  peaks_data[, peaks_start := ifelse(is.na(peak_start), min(spike_frame,na.rm = TRUE)[[1]],peak_start), by =.(Cell_id, labels)]
  peaks_data[, peaks_end_new := Mode(peak_end)[[1]], by =.(Cell_id, labels)]

  sub_peaks_data <- unique(peaks_data[, c("Cell_id","labels","peaks_start","peaks_end_new")])

  sub_peaks_data <- sub_peaks_data[, .SD[peaks_start == min(peaks_start, na.rm =TRUE)], by = .(Cell_id,labels)]

  sub_peaks_data[, lead_peaks_start := dplyr::lead(peaks_start, default = 0), by = Cell_id]

  sub_peaks_data[, peaks_diff := .SD$lead_peaks_start - .SD$peaks_end_new, by = Cell_id]

  sub_peaks_data[, merge_peaks := ifelse(.SD$lead_peaks_start != 0 & peaks_diff <= 0, TRUE,FALSE), by = Cell_id]

  sub_peaks_data[, lead_peaks_end := dplyr::lead(peaks_end_new, default = 0), by = Cell_id]

  sub_peaks_data[, peak_end_final := ifelse(merge_peaks == TRUE, lead_peaks_end, peaks_end_new), by = .(Cell_id,labels)]


  setkey(sub_peaks_data, Cell_id, labels,peaks_start)
  setkey(peaks_data, Cell_id, labels,peaks_start)

  peaks_data_bis <- peaks_data[sub_peaks_data]
  peaks_data <- peaks_data[, .SD[peaks_start == min(peaks_start, na.rm =TRUE)], by = .(Cell_id,labels)]

  peaks_data[Cell_id %in% unique(peaks_data_bis$Cell_id)]$peaks_end_new <- peaks_data_bis$peak_end_final


  peaks_data <- peaks_data[peaks_start < peaks_end_new]


  peaks_data[,peaks_start := min(.SD$peaks_start, na.rm = TRUE) , by = .(Cell_id,peaks_end_new)]
  peaks_data[,peaks_end_new := max(.SD$peaks_end_new, na.rm = TRUE) , by = .(Cell_id,peaks_start)]


}


else{

  peaks <- FALSE
}
return(list(peaks_data, norm_data, peaks))

}



peakExtraction <- function(peak_extracted_data, thresh = 3, var, peak_frame) {



  peak_extracted_data <- peakExtractR(peak_extracted_data[[1]], peak_extracted_data[[2]],
                                      threshold = thresh, var = var, peak_frame = peak_frame)

  if(length(peak_extracted_data[[1]]$Cell_id >= 1)){
  peak_extracted_data[[1]][, peaks_start_bis := peaks_start]
  peak_extracted_data[[1]][, peaks_end_new_bis := peaks_end_new]
  peak_extracted_data[[1]] <- peak_extracted_data[[1]][, .SD[1,], by = .(Cell_id,peaks_start_bis,peaks_end_new_bis)]
  res <- extractPeakVal(peak_extracted_data[[2]],peak_extracted_data[[1]])

  res[, peak_start := min(time_frame, na.rm=TRUE)[[1]], by =.(peak_id)]
  res[, peak_end := max(time_frame, na.rm=TRUE)[[1]], by =.(peak_id)]
  res[, peak_max := max(Mean_Grey, na.rm=TRUE)[[1]], by =.(peak_id)]
  res[, peak_max_frame := .SD[Mean_Grey == peak_max]$time_frame[[1]], by =.(peak_id)]
  res <- res[peak_start < peak_end]
  res[, auc := flux::auc(time_frame, smooth_z), by = peak_id]
  res <- res[, .SD[1,], by = .(Cell_id,peak_id)]
  peak_extracted_data[[1]] <- res
  }

  else{

  }
  return(peak_extracted_data)

}

extractPeakVal <- function(full_data,peaks_data){

  'isnotna' <- Negate('is.na')

  # Retrieve indices of lines where a spike occurred in dt1
  setkey(full_data, Cell_id, time_frame)
  setkey(peaks_data, Cell_id, spike_frame)
  match_indices <- full_data[peaks_data, which = TRUE]

  # create a border n lines later (for each spike)
  end <- match_indices + (peaks_data$peaks_end_new - peaks_data$peaks_start)
  full_data <- full_data[, id := seq(1,length(full_data$Cell_id))]

  # Extract the line + the n lines following each spike
  res <- full_data[.(id = unlist(Map(':', match_indices, end))), on = .(id)]
  res[, lead_frame := dplyr::lead(time_frame), by = Cell_id]
  res[, Diff := lead_frame - time_frame, by = Cell_id]
  res[, peak_id := rleid(Diff)]
  res[, peak_id_bis := rleid(Diff)]
  res[, peak_id := ifelse(.N == 1, .SD$peak_id -1,.SD$peak_id), by = .(peak_id_bis)]

  return(res)
}

