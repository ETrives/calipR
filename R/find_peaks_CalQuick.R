#' find_peaks
#'
#' @param data
#' @param smooth
#' @param raw
#' @param threshold
#' @param group
#'
#' @return
#' @export
#' @importFrom data.table :=
#' @examples
find_peaks <- function(data, smooth = FALSE, raw = FALSE, threshold, group = FALSE){

  data <- data.table::setDT(data)
  threshold <- as.integer(threshold)

  data <- data[, Response := smooth_z >= threshold, by = list(Cell_id, stimulus) ]

  # Detecting responding cells
  sum_response <- data[, Sum_resp := sum(Response), by = Cell_id]

  # Filtering responding cells
  Responding_cells <- sum_response[Sum_resp != 0]

  # créer une colonne qui contient l'info de la dérivée de la valeur précédende :

  Responding_cells <- Responding_cells[, c("lag_first_derivative", "lag_smooth",
                                           "lead_smooth") := list(dplyr::lag(first_derivative),dplyr::lag(smooth_z),dplyr::lead(smooth_z)), by = list(Cell_id)]


  Responding_cells <- Responding_cells[, Max_Peak := Response == TRUE &
                                         lag_smooth < smooth_z & lead_smooth < smooth_z]

  df <- Responding_cells[Max_Peak == TRUE]

  df <- dplyr::rename(df,  "Max_peak_stimulus" = "stimulus", "Max_peak_frame" = "time_frame", "Max_peak_stimulation" = "Stimulation",
               "Max_peak_smooth_z" = "smooth_z", "Max_peak_first_derivative" = "first_derivative" )

  if(group == TRUE){
    df <- subset(df, select = c(Cell_id, Max_peak_stimulus, group, Max_peak_frame,
                                 Max_peak_smooth_z, Max_peak_first_derivative ))
  }

  if(group == FALSE){
    df <- subset(df, select = c(Cell_id, Max_peak_stimulus, Max_peak_frame, Max_peak_smooth_z,
                                Max_peak_first_derivative ))
  }

  return(list(df, Responding_cells))
}
