# Vectorisation des subsets dans deconvolve :

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

  cell_split <- lapply(cell_split, function(x) add_peak_info(x, gam = gam, lambda = lambda,constraint, estimate_calcium, var, ESP))

  peaks_data <- lapply(cell_split, function(x) if(length(x$peak_frames)[[1]] != 0) {x[x$time_frame %in% x$peak_frames[[1]]]} )

  data <- do.call(rbind, cell_split)
  peaks_data <- do.call(rbind, peaks_data)


  if(length(peaks_data$Cell_id) != 0) {


    # For each spike, get it and the 21 lines that follow it then find the max peak
    # and only keep that line
    dt <- subset_spike_frames(norm_data,peaks_data)
    dt <- dt[, Max_by_20 := max(smooth_z, na.rm = TRUE), by = blocs]
    dt <- dt[Max_by_20 ==  smooth_z]

    peaks_data$Max_peak_frame <- dt$time_frame
    peaks_data$max_peak_smooth_z <- dt$smooth_z

  }

  # Remove NAs and spikes with a max peak below threshold
  peaks_data <- peaks_data[!is.na(max_peak_smooth_z) & max_peak_smooth_z >= threshold]


  if(is.null(peaks_data) == FALSE) {

    peaks_data <- dplyr::rename(peaks_data,  "spike_stimulus" = "stimulus", "spike_frame" = "time_frame", "spike_stimulation" = "Stimulation",
                                "spike_smooth_z" = "smooth_z", "spike_first_derivative" = "first_derivative" )


    peaks_data <- unique(peaks_data[,c("Cell_id", "spike_frame", "spike_stimulus",
                                       "spike_smooth_z", "Mean_Grey", "gam_detrended", "Max_peak_frame", "max_peak_smooth_z" )])
  }


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
subset_spike_frames <- function(dt1,dt2){

  # Retrieve indices of lines where a spike occurred in dt1
  setkey(dt1, Cell_id, time_frame)
  setkey(dt2, Cell_id, time_frame)

  match_indices <- dt1[dt2, which = TRUE]

  # create a border 20 lines later (for each spike)
  end <- match_indices + 20L

  match <- match[, id := seq(1,length(match$Cell_id))]
  dt1 <- dt1[, id := seq(1,length(dt1$Cell_id))]

  # Extract the line + the 20 following each spike
  res <- dt1[.(id = unlist(Map(':', match_indices, end))), on = "id"]

  # Add a grouping variable "blocs" to then compute the max on each of these parts
  res <- res[, blocs := rep(1:(length(res$Cell_id)/21), each = 21)]

  return(res)
}

