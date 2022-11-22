
#' filter_baseline_response
#'
#' @param data
#' @param threshold
#'
#' @return a filtered dataframe with cells responding at baseline removed
#' @export
#'
#' @examples
filter_baseline_response <- function(data, threshold){

  data <- data.table::setDT(data)[, Base_Response := smooth_z >= threshold & stimulus == "1.Baseline"]

  sum_response <-  plyr::ddply(data, .(Cell_id), summarise, Base_Response = sum(Base_Response))

  sum_filter <- sum_response[sum_response$Base_Response == 0,]

  data_filt <- dplyr::filter(data, Cell_id %in% sum_filter$Cell_id)
  return(data_filt)
}
