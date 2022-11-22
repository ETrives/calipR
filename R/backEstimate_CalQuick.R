
#' backEstimate
#'
#'Function to estimate the background (the low frequency changes) of each ROI
#'with 4 available methods : simple linear regression, polynomial regression,
#'generalized additive model (GAM) or 8th percentile.
#'
#' @param data a data frame output from PrepareData function
#' @param method One method among the 4 possible options : "linear", "polynomial", "gam", "quantile"
#'
#' @return a data table object with 5 new columns : a local mean, the first derivative, the DPA, the mean grey values with peak values replaced by 10th percentile and the fitted values with the chosen method (the background estimation)
#' @export
#'
#' @examples
#'
backEstimate <- function(data, mean_width = 50, method = c("linear", "poly", "gam", "quantile")){

  # Splitting the data frame by cell_id
  cell_split <- split(data, data$Cell_id)

  # Converting to each elements to data.table objects
  cell_split <- lapply(cell_split, function(x) data.table::setDT(x))

  # Computing a local mean for each data.table
  #cell_split <- lapply(cell_split, function(x) x[, local_mean := gplots::wapply(x$time_frame, x$Mean_Grey, fun = mean, n=length(x$time_frame), width = width, method = "nobs")[[2]]])

  # Computing first derivative
  #cell_split <- lapply(cell_split, function(x) x[, first_derivative := doremi::calculate.gold(time = x$time_seconds, signal = x$local_mean,
                                                                                             # embedding = 3, n = 1)$dsignal[,2]])

  # computing DPA
  #cell_split <- lapply(cell_split, function(x) DPA(x, w = 3))


  # Replacing peak values by 10th percentile
  #cell_split <- lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := replace(Mean_Grey, DPA > stats::quantile(DPA, probs = .6, na.rm = TRUE),
     # stats::quantile(Mean_Grey, probs = .1, na.rm = TRUE))])

  # Estimating the background with one of the following methods : linear, polynomial, gam, quantile

  if(method == "linear" ){

    f_linear <- stats::as.formula(paste("Mean_Grey_wo_peaks", "time_frame", sep = " ~ "))
    cell_split <- lapply(cell_split, function(x) x[, linear_fit := stats::lm(formula = f_linear, .SD)$fitted.values, by = Cell_id])
    cell_split <- lapply(cell_split, function(x) x[, linear_detrended := Mean_Grey - linear_fit])

  }


  if(method == "poly" ){

    f_poly <- stats::as.formula(paste("Mean_Grey_wo_peaks", "poly(time_frame,5)", sep = " ~ "))

    cell_split <- lapply(cell_split, function(x) x[, poly_fit := stats::lm(formula = f_poly, .SD)$fitted.values])

    cell_split <- lapply(cell_split, function(x) x[, poly_detrended := Mean_Grey - poly_fit])

  }



  if(method == "gam"){


    cell_split <- lapply(cell_split, function(x) x[, gam_fit := mgcv::gam(Mean_Grey_wo_peaks ~ s(time_frame, bs = "cr", k = 11), data = x, gamma = 2)[[3]]])
    cell_split <- lapply(cell_split, function(x) x[, gam_detrended := Mean_Grey - gam_fit])
  }


  if(method == "quantile"){

    cell_split <- lapply(cell_split, function(x) x[, local_quantile := gplots::wapply(x$time_frame, x$Mean_Grey_wo_peaks,
                                                  fun = function(x) quantile_speed(x, probs = .1), n = length(x$time_frame), width = 30, method = "nobs")[[2]]])

    cell_split <- lapply(cell_split, function(x) x[, quantile_detrended := Mean_Grey - local_quantile])

    data <- do.call(rbind, cell_split)
    data$quantile_detrended[which(is.na(data$quantile_detrended))] <- data$local_mean[which(is.na(data$quantile_detrended))]
    cell_split <- split(data, data$Cell_id)
  }

return(do.call(rbind, cell_split))
}
