

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
deconvolve <- function(norm_data, gam = 0.95, lambda = 1, constraint = T, estimate_calcium = T, var = "gam_detrended", ESP = 0.0001,
                       threshold = 3) {

  cells <- unique(norm_data$Cell_id)

  cell_split <- split(norm_data, norm_data$Cell_id)

  print("first_split = ok")
  cell_split <- lapply(cell_split, function(x) add_peak_info(x, gam = gam, lambda = lambda,constraint, estimate_calcium, var, ESP))

  print(cell_split)
  print("add peak info = OK ")
  peaks_data <- lapply(cell_split, function(x) if(length(x$peak_frames)[[1]] != 0) {x[x$time_frame %in% x$peak_frames[[1]]]} )

  print("discreted peaks ")


  data <- do.call(rbind, cell_split)
  peaks_data <- do.call(rbind, peaks_data)
  print(peaks_data)

  peaks_data <- split(peaks_data,cumsum(1:nrow(peaks_data) %in% seq(1:nrow(peaks_data))))

  print(peaks_data)
  peaks_data <- lapply(peaks_data, function(x) if(x$smooth_z[[1]] >= threshold) {x} )

  peaks_data <- do.call(rbind, peaks_data)

  #peaks_data <- split(peaks_data,cumsum(1:nrow(peaks_data) %in% seq(1:nrow(peaks_data))))

  print("split ok")

  print("threshold ok" )
  # peaks_data <- do.call(rbind, peaks_data)

    if(is.null(peaks_data) == FALSE) {

      peaks_data <- dplyr::rename(peaks_data,  "spike_stimulus" = "stimulus", "spike_frame" = "time_frame", "spike_stimulation" = "Stimulation",
                                  "spike_smooth_z" = "smooth_z", "spike_first_derivative" = "first_derivative" )


      peaks_data <- unique(peaks_data[,c("Cell_id", "spike_frame", "spike_stimulus", "spike_smooth_z", "Mean_Grey", "gam_detrended")])
    }


print(peaks_data)
print("peaks_data")

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

  max <- max(x[[var]])

  if(max <= 0){
    max <- lambda
  }

  peak_data <- FastLZeroSpikeInference::estimate_spikes(x[[var]], gam = gam,
                                                        lambda = max*lambda,constraint, estimate_calcium, ESP)


  x <- x[, c("deconvolved_trace", "peak_frames") :=
           list(peak_data$estimated_calcium, list(peak_data$spikes))]

  return(x)

}
