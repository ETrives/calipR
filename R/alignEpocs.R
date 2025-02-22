
#' alignEpocs
#'
#' @param behavior_data A data table containing behavioral annotations timestamped
#'                      with the annotateVideo function
#' @param fiber_data A data table containing calcium data, isosbestic data,
#'                   mouse ID and the time (in seconds) as generated with the
#'                   extract_tdt_data function or extract_all_tdt_data function
#' @param mouse_id A character indicating the mouse ID.
#'
#' This function can be use with a single video annotation file (one mouse) and
#' a dataset containing all fiber photometry data (all mice). Or with a single
#' video annotation file and a fiber photometry data containing only this mouse.
#' To process all video annotations and all fiber photometry data, use this function
#' iteratively with apply or a for loop.
#'
#' @return A joint data table with behavioral data aligned to fiber photometry data
#' @export
#'
#' @examples
alignEpocs <- function(behavior_data, fiber_data, mouse_id){

  fiber_data[, TIME_SECONDS_END := TIME_SECONDS + 0.0000001]

  setkey(behavior_data, onset, offset)  # Clé pour la jointure rapide
  setkey(fiber_data, TIME_SECONDS, TIME_SECONDS_END)

  # data.table function which aligns the behaviors in fiber, based on the time interval
  # specified in the behavior datatable.

  result <- foverlaps(fiber_data[ID == mouse_id], behavior_data,
                      by.x = c("TIME_SECONDS", "TIME_SECONDS_END"),
                      by.y = c("onset", "offset"),
                      nomatch = NA)

  colnames(result)[1] <- "VIDEO_FRAME"
  colnames(result)[7] <- "FIBER_FRAME"

  return(result)
}
