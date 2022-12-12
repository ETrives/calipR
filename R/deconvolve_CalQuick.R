

#' deconvolve
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
deconvolve <- function(norm_data, gam = 0.95, lambda = 1, constraint = T, estimate_calcium = T, var = "smooth_delta", ESP = 0.0001,
                       threshold = 3) {

  cells <- unique(norm_data$Cell_id)

  cell_split <- split(norm_data, norm_data$Cell_id)
  cell_split <- lapply(cell_split, function(x) add_peak_info(x, gam = gam, lambda = lambda,constraint, estimate_calcium, var, ESP))
  peaks_data <- lapply(cell_split, function(x) if(length(x$peak_frames)[[1]] != 0) {x[x$time_frame %in% x$peak_frames[[1]]]} )
  peaks_data <- lapply(peaks_data, function(x) if(dim(x)[[1]] != 0) {x[, frame_window := x$time_frame +20]} )

  data <- do.call(rbind, cell_split)
  peaks_data <- do.call(rbind, peaks_data)
  peaks_data <- split(peaks_data,cumsum(1:nrow(peaks_data) %in% seq(1:nrow(peaks_data))))


  peaks_data <- lapply(peaks_data, function(x) data[data$smooth_z == max(data[data$Cell_id == x$Cell_id & data$time_frame %between% list(x$time_frame, x$frame_window)]$smooth_z)[[1]]])

  peaks_data <- lapply(peaks_data, function(x) if(x$smooth_z >= threshold) {x} )

  peaks_data <- do.call(rbind, peaks_data)

  peaks_data <- dplyr::rename(peaks_data,  "Max_peak_stimulus" = "stimulus", "Max_peak_frame" = "time_frame", "Max_peak_stimulation" = "Stimulation",
                              "Max_peak_smooth_z" = "smooth_z", "Max_peak_first_derivative" = "first_derivative" )


  return(list(peaks_data, data))

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

  lambda <- as.numeric(lambda)
  gam <- as.numeric(gam)
  peak_data <- FastLZeroSpikeInference::estimate_spikes(x[[var]], gam = gam,
                                                        lambda = lambda*max(x[[var]]),constraint, estimate_calcium, ESP)


  x <- x[, c("deconvolved_trace", "peak_frames") :=
           list(peak_data$estimated_calcium, list(peak_data$spikes))]

  return(x)

}
