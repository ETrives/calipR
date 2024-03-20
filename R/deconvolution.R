#' deconvolve
#'
#' Deconvolution of the denoised fluorescence trace based on the FastL0SpikeInference package.
#' To date, this step is used for two things : identifiying the start of calcium events and efficiently identifying
#' multiple peaks during a given stimulus. Found spikes are not used as is to infer spiking activity but will be in the future.
#'
#' @param norm_data
#' @param gam
#' @param lambda
#' @param constraint
#' @param estimate_calcium
#' @param var
#' @param ESP
#' @param threshold
#'
#' @return
#' @export
#'
#' @examples
deconvolve <- function(norm_data, gam = 0.95, lambda = 1, constraint = T, estimate_calcium = T, var = "gam_detrended", ESP = 0.0001,
                       threshold = 3, delta_threshold = 0, peak_frame = 10) {

  'isnotna' <- Negate('is.na')
  cells <- unique(norm_data$Cell_id)
  stim_list <- unique(norm_data$stimulus)
  norm_data <- norm_data[, Prev_stim := ifelse(stimulus != stim_list[1], stim_list[grep(stimulus[[1]], stim_list) -1], stim_list[1]), by = .(Cell_id, stimulus) ]

  norm_data <- norm_data[, Prev_stim := stim_list[Prev_stim]]

  cell_split <- split(norm_data, norm_data$Cell_id)

  cell_split <- lapply(cell_split, function(x) x[, lag_stim := dplyr::lead(stimulus, 5, default = NA)])

  cell_split <- lapply(cell_split, function(x) add_peak_info(x, gam = gam, lambda = lambda,constraint, estimate_calcium, var, ESP))

  peaks_data <- lapply(cell_split, function(x) if(length(x$peak_frames)[[1]] != 0) {x[x$time_frame %in% x$peak_frames[[1]]]} )

  cell_split <- lapply(cell_split, function(x) x[, stimulus := lag_stim])

  data <- do.call(rbind, cell_split)
  peaks_data <- do.call(rbind, peaks_data)


  if(length(peaks_data$Cell_id) != 0) {


    # For each spike, get it and the 21 lines that follow it then find the max peak
    # and only keep that line
    dt <- subset_spike_frames(norm_data,peaks_data, peak_frame)


    dt <- dt[, Max_by_20 := max(smooth_z, na.rm = TRUE)[[1]], by = .(blocs)]

    #Derivative and smooth_z criteria:
    dt <- dt[, peak_start_new := .SD[smooth_z > 0 & smooth_Diff > 0,][1,]$time_frame, by = .(blocs)]

    # valley in the 10 first frames :
    #dt <- dt[, peak_start_new := .SD[ smooth_z == min(.SD[c(1:10),]$smooth_z,na.rm = TRUE)]$time_frame[[1]], by = .(blocs)]


    #dt <- dt[, peak_start_new := ifelse(is.na(peak_start_new), min()), by = .(blocs)]

    dt <- dt[Max_by_20 ==  smooth_z]

    print("dt peak start")
    print(dt)

    dt <- dt[, duplicate := duplicated(blocs)][duplicate == FALSE]


    peaks_data$Max_peak_frame <- dt$time_frame
    peaks_data$max_peak_smooth_z <- dt$smooth_z
    peaks_data$max_peak_smooth_delta <- dt$delta_f_f
    peaks_data$peak_start <- dt$peak_start_new


    # now same but to find peak end :

    dt <- subset_spike_frames(norm_data,peaks_data, peak_frame = 100)
    print("dt_end")
    print(dt)

    dt[, Max_by_20 := max(smooth_z, na.rm = TRUE)[[1]], by = .(blocs)]

    # Chercher le pic après le maximum
    #dt <- dt[, peak_end := .SD[smooth_z <= 0 & time_frame > .SD[smooth_z == Max_by_20]$time_frame][1,]$time_frame,  by = .(blocs)]

    # Critère de dérivée négative à la place :

    dt[, smooth_Diff := ifelse(is.na(smooth_Diff), first_derivative,smooth_Diff), by = .(blocs)]

    dt <- dt[, peak_end := .SD[smooth_z <= 0 & smooth_Diff <= 0,][1,]$time_frame,  by = .(blocs)]




    dt <- dt[, peak_end := ifelse(is.na(peak_end),
                            .SD[time_frame > .SD[smooth_z == Max_by_20]$time_frame][smooth_z == min(smooth_z, na.rm = TRUE)]$time_frame, peak_end),  by = .(blocs)]

    dt <- dt[, duplicate := duplicated(blocs)][duplicate == FALSE]

    print("dt peak end")
    print(dt)

    peaks_data$peak_end <- dt$peak_end

  }

  # Remove NAs

  peaks_data <- peaks_data[isnotna(max_peak_smooth_z) & max_peak_smooth_z >= threshold & max_peak_smooth_delta >= delta_threshold]



  if(dim(peaks_data)[1] != 0) {

    peaks_data <- dplyr::rename(peaks_data,  "spike_stimulus" = "stimulus", "spike_frame" = "time_frame", "spike_stimulation" = "Stimulation",
                                "spike_smooth_z" = "smooth_z", "spike_first_derivative" = "first_derivative", "spike_smooth_Diff" = "smooth_Diff")


    peaks_data <- unique(peaks_data[,c("Cell_id", "spike_frame", "spike_stimulus",
                                       "spike_smooth_z", "Mean_Grey", get("var"),
                                       "Max_peak_frame", "max_peak_smooth_z",
                                       "spike_smooth_Diff", "lag_stim",
                                       "Time_frame_stim", "Prev_stim", "peak_end","peak_start" )])



  peaks_data <- data.table::setDT(peaks_data)[,lead_spike_smooth_Diff_1 := dplyr::lead(spike_smooth_Diff, 1, default = 0), by = Cell_id]
  peaks_data <- peaks_data[,lead_spike_smooth_Diff_2 := dplyr::lead(spike_smooth_Diff, 2, default = 0), by = Cell_id]
  peaks_data <- peaks_data[,lead_spike_smooth_Diff_3 := dplyr::lead(spike_smooth_Diff, 3, default = 0), by = Cell_id]


  peaks_data <- peaks_data[spike_smooth_Diff > 0 |lead_spike_smooth_Diff_3 > 0 |
                    lead_spike_smooth_Diff_2 > 0 |lead_spike_smooth_Diff_1 > 0 ]


  peaks_data <- peaks_data[,diff := c(0,diff(spike_frame)), by = Cell_id]


  peaks_data <- peaks_data[,labels := cumsum(diff > 10) , by = Cell_id]


  # Method to assign the first stimulus to a spike group :
  peaks_data[, new_stimulus :=  spike_stimulus[1],by = .(Cell_id,labels)]


  peaks_data[, new_new_stimulus := ifelse(Time_frame_stim[1] < 3, Prev_stim, new_stimulus), by = .(Cell_id, labels)]
  peaks_data <- peaks_data[, new_new_stimulus := stim_list[new_new_stimulus]]
  peaks_data <- peaks_data[, new_stimulus :=  new_new_stimulus]


  peaks <- TRUE


  # Identifying overlapping peaks to merge them :
  peaks_data[, peaks_start := ifelse(is.na(peak_start), min(spike_frame,na.rm = TRUE)[[1]],peak_start), by =.(Cell_id, labels)]
  peaks_data[, peaks_end_new := Mode(peak_end)[[1]], by =.(Cell_id, labels)]

  sub_peaks_data <- unique(peaks_data[, c("Cell_id","labels","peaks_start","peaks_end_new")])

  sub_peaks_data[, lead_peaks_start := dplyr::lead(peaks_start, default = 0), by = Cell_id]

  sub_peaks_data[, peaks_diff := .SD$lead_peaks_start - .SD$peaks_end_new, by = Cell_id]

  sub_peaks_data[, merge_peaks := ifelse(.SD$lead_peaks_start != 0 & peaks_diff <= 0, TRUE,FALSE), by = Cell_id]

  sub_peaks_data[, lead_peaks_end := dplyr::lead(peaks_end_new, default = 0), by = Cell_id]

  sub_peaks_data[, peak_end_final := ifelse(merge_peaks == TRUE, lead_peaks_end, peaks_end_new), by = .(Cell_id,labels)]

  #sub_peaks_data <- sub_peaks_data[merge_peaks == TRUE]

  print("sub_peaks_data")
  View(sub_peaks_data)

  #rep_len <- peaks_data[Cell_id %in% unique(sub_peaks_data$Cell_id), .N, by = Cell_id]

  setkey(sub_peaks_data, Cell_id, labels,peaks_start)
  setkey(peaks_data, Cell_id, labels,peaks_start)

  print("peaks_data")
  View(peaks_data)

  peaks_data_bis <- peaks_data[sub_peaks_data]

  print("peaks_data")
  View(peaks_data_bis)

  peaks_data[Cell_id %in% unique(peaks_data_bis$Cell_id)]$peaks_end_new <- peaks_data_bis$peak_end_final



  peaks_data <- peaks_data[peaks_start < peaks_end_new]

 # peaks_data[, peaks_borders := .(.(.(min(spike_frame,na.rm = TRUE)[[1]],Mode(peak_end)[[1]]))), by =.(Cell_id, labels)]

 # peaks_data[,peaks_start := ifelse(length(unique(peaks_end_new)) == 1 ]

  peaks_data[,peaks_start := min(.SD$peaks_start, na.rm = TRUE) , by = .(Cell_id,peaks_end_new)]
  peaks_data[,peaks_end_new := max(.SD$peaks_end_new, na.rm = TRUE) , by = .(Cell_id,peaks_start)]

  print("peaks_data_after")
  View(peaks_data)
  }


  else{

  peaks <- FALSE
  }

  return(list(peaks_data, data, peaks))
}


#' subset_spike_frames
#'
#' @param dt1
#' @param dt2
#'
#' @return
#' @export
#'
#' @examples
subset_spike_frames <- function(dt1,dt2, peak_frame = 10){

  # Retrieve indices of lines where a spike occurred in dt1
  setkey(dt1, Cell_id, time_frame)
  setkey(dt2, Cell_id, time_frame)

  match_indices <- dt1[dt2, which = TRUE]

  # create a border n lines later (for each spike)
  end <- match_indices + peak_frame

  dt1 <- dt1[, id := seq(1,length(dt1$Cell_id))]


  # Extract the line + the n lines following each spike
  res <- dt1[.(id = unlist(Map(':', match_indices, end))), on = .(id)]


  # Add a grouping variable "blocs" to then compute the max on each of these parts
  res <- res[, blocs := rep(1:(length(res$Cell_id)/(peak_frame+1)), each = peak_frame+1)]



  return(res)
}


#' add_peak_info
#'
#' @param x
#' @param gam
#' @param lambda
#' @param constraint
#' @param estimate_calcium
#' @param var
#' @param ESP
#'
#' @return
#' @export
#'
#' @examples
add_peak_info <- function(x, gam, lambda,constraint, estimate_calcium, var = var, ESP){

  peak_data <- FastLZeroSpikeInference::estimate_spikes(x[[var]], gam = gam,
                                                        lambda = lambda,constraint, estimate_calcium, ESP)



  x <- x[, c("deconvolved_trace", "peak_frames") :=
           list(peak_data$estimated_calcium, list(peak_data$spikes))]

  return(x)

}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


