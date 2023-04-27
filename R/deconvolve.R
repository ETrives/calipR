

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


  print("add peak info = OK ")
  peaks_data <- lapply(cell_split, function(x) if(length(x$peak_frames)[[1]] != 0) {x[x$time_frame %in% x$peak_frames[[1]]]} )

  print("discreted peaks ")

  print("discreted peaks ")
  peaks_data <- lapply(peaks_data, function(x) if(dim(x)[[1]] != 0) {x[, frame_window := x$time_frame +20]} )

  print("frame window created ")


  data <- do.call(rbind, cell_split)
  peaks_data <- do.call(rbind, peaks_data)
  print(peaks_data)

  print(length(peaks_data$Cell_id))

  if(length(peaks_data$Cell_id) != 0) {

    peaks_data <- split(peaks_data,cumsum(1:nrow(peaks_data) %in% seq(1:nrow(peaks_data))))
print( "yo")




    print( "ya")
    smooth_z <- unlist(lapply(peaks_data, function(x) data[data$smooth_z == max(data[data$Cell_id == x$Cell_id & data$time_frame %between%
                                   list(x$time_frame, x$frame_window)]$smooth_z, na.rm = TRUE)[[1]],][1,]$smooth_z))
    print( "yo")

    max_frame <- unlist(lapply(peaks_data, function(x) data[data$smooth_z == max(data[data$Cell_id == x$Cell_id & data$time_frame %between%
                                                                                       list(x$time_frame, x$frame_window)]$smooth_z, na.rm = TRUE)[[1]],][1,]$time_frame))

    print( "yu")

    print(length(smooth_z))
    print(length(max_frame))

    peaks_data <- do.call(rbind, peaks_data)
    print(length(peaks_data$Cell_id))

    peaks_data$max_peak_smooth_z <- smooth_z
    peaks_data$Max_peak_frame <- max_frame

#print(peaks_data)
    print("DONNEE")

    print("subset ok" )
    #print(peaks_data)
  }

  View(peaks_data)
  print(peaks_data)

  peaks_data <- split(peaks_data,cumsum(1:nrow(peaks_data) %in% seq(1:nrow(peaks_data))))

  print(threshold)
  lapply(peaks_data, function(x) if(is.na(x$max_peak_smooth_z[[1]])) {print(x)} else{})

  print("youu")
  peaks_data <- lapply(peaks_data, function(x) if(is.na(x$max_peak_smooth_z[[1]])) {} else{x})

  print("youuD")

  peaks_data <- do.call(rbind, peaks_data)
  peaks_data <- split(peaks_data,cumsum(1:nrow(peaks_data) %in% seq(1:nrow(peaks_data))))


  #lapply(peaks_data, function(x) print(x$max_peak_smooth_z[[1]] >= threshold))

  peaks_data <- lapply(peaks_data, function(x) if(x$max_peak_smooth_z[[1]] >= threshold) {x} else{})

  peaks_data <- do.call(rbind, peaks_data)

 # print(peaks_data)
  #peaks_data <- split(peaks_data,cumsum(1:nrow(peaks_data) %in% seq(1:nrow(peaks_data))))

  print("split ok")

  print("threshold ok" )
  # peaks_data <- do.call(rbind, peaks_data)


    if(is.null(peaks_data) == FALSE) {

      peaks_data <- dplyr::rename(peaks_data,  "spike_stimulus" = "stimulus", "spike_frame" = "time_frame", "spike_stimulation" = "Stimulation",
                                  "spike_smooth_z" = "smooth_z", "spike_first_derivative" = "first_derivative" )


      peaks_data <- unique(peaks_data[,c("Cell_id", "spike_frame", "spike_stimulus", "spike_smooth_z", "Mean_Grey", "gam_detrended", "Max_peak_frame")])
    }


#print(peaks_data)

print("peaks_data")


print(data)
print(peaks_data)
print("hou")
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
