#' deconvolve
#'
#' Deconvolution of the denoised fluorescence trace based on the FastL0SpikeInference package.
#' To date, this step is used for two things : identifiying the start of calcium events and efficiently identifying
#' multiple peaks during a given stimulus. Found spikes are not used as is to infer spiking activity but will be in the future.
#'
#' @param norm_data a data table output from norm_df() function
#'
#' @param gam a double between 0 and 1 modulating the modeled peak decay (i.e.
#' a gam of 0.99 models much longer decay than a gam of 0.5) default to 0.95.
#'
#' @param lambda an integer controlling the sensitivity of spike detection (i.e.
#' a lambda of 1 will find much more spikes for a given event than a lambda of 1000).
#' In v.1.0.0-alpha, deconvolution is only used to find calcium events, not spikes.
#' So increasing this number to the point where you only find one (or few) spike(s) per calcium event
#' can largely increase the processing speed and will not affect the results. You can interactively optimize
#' this parameter in the parameter optimization tab in the GUI.
#'
#' @param var variable used for deconvolution. Use the denoised trace for good
#' performances ("gam_detrended" if using DPA noise estimation or "background_detrended"
#' if using pattern matching noise estimation)
#'
#'
#' @return
#' @export
#'
#' @examples
deconvolve <- function(norm_data, gam = 0.95, lambda = 1, var = "gam_detrended",
                      peak_frame = 10) {

  'isnotna' <- Negate('is.na')
  cells <- unique(norm_data$Cell_id)
  stim_list <- unique(norm_data$stimulus)
  norm_data <- norm_data[, Prev_stim := ifelse(stimulus != stim_list[1], stim_list[grep(stimulus[[1]], stim_list) -1], stim_list[1]), by = .(Cell_id, stimulus) ]

  norm_data <- norm_data[, Prev_stim := stim_list[Prev_stim]]

  # compute the first derivative on the detrended and normalized trace

  first_d_fct <- function(x,y) doremi::calculate.gold(time = x, signal = y,
                                                      embedding = 2, n = 1)$dsignal[,2]

  local_mean_diff_fct <- function(x,y,z) gplots::wapply(x, y, fun = mean, n=length(z), width = 10, method = "nobs", drop.na = FALSE)[[2]]

  norm_data[, first_derivative_detrended := norm_data[, .(first_derivative = first_d_fct(get("time_seconds"), get("z_score"))), by = Cell_id]$first_derivative]
  norm_data[, smooth_Diff_detrended := norm_data[, .(smooth_Diff = local_mean_diff_fct(get("time_frame"), get("first_derivative_detrended"), get("time_frame"))), by = Cell_id]$smooth_Diff]

  cell_split <- split(norm_data, norm_data$Cell_id)

  #cell_split <- lapply(cell_split, function(x) x[, lag_stim := dplyr::lead(stimulus, 5, default = NA)])

  cell_split <- lapply(cell_split, function(x) add_peak_info(x, gam = gam, lambda = lambda, var))

  peaks_data <- lapply(cell_split, function(x) if(length(x$peak_frames)[[1]] != 0) {x[x$time_frame %in% x$peak_frames[[1]]]} )

  #cell_split <- lapply(cell_split, function(x) x[, stimulus := lag_stim])

  data <- do.call(rbind, cell_split)
  peaks_data <- do.call(rbind, peaks_data)


  return(list(peaks_data, data))
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

  print(peak_frame)
  print("peak_frame_2")

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
#' @param var

#'
#' @return
#' @export
#'
#' @examples
add_peak_info <- function(x, gam, lambda, var = var){

  peak_data <- FastLZeroSpikeInference::estimate_spikes(x[[var]], gam = gam,
                                                        lambda = lambda,constraint = T,
                                                        estimate_calcium = T)



  x <- x[, c("deconvolved_trace", "peak_frames") :=
           list(peak_data$estimated_calcium, list(peak_data$spikes))]

  return(x)

}

#' mode
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


