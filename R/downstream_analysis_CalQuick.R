
#' downstream_analysis
#'
#' @param data
#' @param moving_thresh
#' @param outlier_thresh
#' @param mean_width
#' @param DPA_width
#' @param mean_width_diff
#' @param method
#' @param norm_var
#' @param norm_width
#' @param lambda
#' @param gam
#' @param constraint
#' @param threshold
#' @param deconvolve_var
#' @param borders_range
#' @param time_thresh
#' @param frame_rate
#' @param compare_groups
#'
#' @return
#' @export
#'
#' @examples
downstream_analysis <- function(data, moving_thresh = 0.1, outlier_thresh = 2, mean_width = 10, DPA_width = 10,
                                mean_width_diff = 10, method = "gam", norm_var = "gam",
                                norm_width = 10, lambda = 100,
                                gam = 0.97, constraint = T, threshold = 3,
                                deconvolve_var = "gam_detrended", borders_range = 50,
                                time_thresh = 1, frame_rate, compare_groups = FALSE) {

  clean <- clean_data(data, moving_thresh, outlier_thresh, mean_width, DPA_width, mean_width_diff)
  print("cleaning = OK")
  back <- backEstimate(clean, method = method)
  print("back = OK")

  norm <- norm_df(back, var = norm_var, width = norm_width)
  print("norm = OK")

  deconvolved <- deconvolve(norm, lambda = lambda, gam = gam, constraint = constraint,
                            threshold = threshold, var = deconvolve_var)
  best <- keep_best_peaks(deconvolved)
  borders <- find_borders(best, range = borders_range)


return(list(borders[[1]], norm))
}
