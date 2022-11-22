

#' zero_crossing_index
#'
#' @param data
#' @param max
#'
#' @return
#' @export
#'
#' @examples
zero_crossing_index <- function(data, max){


  d <- data[data$first_derivative < 0,]


  # Computing the delta between the Ksmooth Z at the peak and the Ksmooth Z for each time point
  d$delta <- max - abs(d$smooth_z)

  # Computing a inferior threshold the delta needs to cross (here set to 20% of the max delta)
  borne_inf <- max(d$delta) - (max(d$delta)/100)*50

  # Filtering the values crossing the threshold
  rise <- d[abs(d$delta) >= borne_inf,]

  if(dim(rise)[1] < 1) {
    print("delta doesn't cross the inferior threshold")
    print(data)
    rise <- data[1,]
    rise$smooth_z <- NA
  }
  # choosing the value that is the closest from the peak
  time <- max(rise$time_frame) + 1

  rise <- data[data$time_frame == time,]

  if(dim(rise)[1] < 1) {
    print("time_frame + 1 not found in the data")
    print(data)
    print(rise$time_frame)
    rise <- data[1,]
    rise$smooth_z <- NA
  }

  return(rise)
}
