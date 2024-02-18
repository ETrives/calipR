
#' backEstimate
#'
#'Function to estimate the background (the low frequency changes) of each ROI
#'with 2 available methods : a running average or a generalized additive model (GAM).
#'These variables are computed on the Mean Gray fluorescence values in which probable peaks have been removed
#'within the previous function(clean_data).
#'
#' @param data a data frame output from PrepareData function
#' @param method One method among the 4 possible options : "linear", "polynomial", "gam", "quantile"
#'
#' @return a data table object with 5 new columns : a local mean, the first derivative, the DPA, 
#' the mean grey values with peak values replaced and the fitted values with the chosen method (the background estimation)
#' @export
#'
#' @examples
#'
backEstimate <- function(data, smooth = 50, method = c("smooth","gam")){


  cell_split <- split(data, data$Cell_id)

  if(method == "smooth"){
  cell_split  <- lapply(cell_split , function(x) x[, gam_fit := gplots::wapply(x$time_frame, x$Mean_Grey_wo_peaks, fun = mean, n=length(x$time_frame), width = smooth, method = "nobs")[[2]]])
  cell_split <- lapply(cell_split, function(x) x[, gam_detrended := Mean_Grey - gam_fit])

  }

  if(method == "gam"){


    cell_split <- lapply(cell_split, function(x) x[, gam_fit := mgcv::gam(Mean_Grey_wo_peaks ~ s(time_frame, bs = "cr", k = 11), data = x, gamma = 2)[[3]]])
    cell_split <- lapply(cell_split, function(x) x[, gam_detrended := Mean_Grey - gam_fit])
  }

  data <- do.call(rbind, cell_split)

  return(data)
}
