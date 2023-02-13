
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
backEstimate <- function(data, smooth = 50, method = c("smooth","gam")){

  #noise_model <- readRDS(system.file("model/noise_model.rds", package = "calipR"))

  #data <- data %>%
   # dplyr::bind_cols(
    #  stats::predict(noise_model, data),
     # stats::predict(noise_model, data, type = "prob" )
    #)


  cell_split <- split(data,data$Cell_id)

  #print(cell_split)
  #cell_split <- lapply(cell_split, function(x) data.table::setDT(x)[, signal :=
#                            ifelse(x$.pred_signal > 0.5 | x$smooth_DPA >
#                                     mean(x$smooth_DPA)*0.8 | x$smooth_Diff < 1 , TRUE, FALSE)])

 # cell_split <- lapply(cell_split, function(x) x[, noise :=
  #                                                 ifelse(x$.pred_noise > 0.7 | x$smooth_DPA <
   #                                                         0.5*mean(x$smooth_DPA)|
    #                                                        x$Mean_Grey <
     #                                                       mean(x$Mean_Grey),TRUE, FALSE)])

  #print("noise = ok")
  #cell_split <- lapply(cell_split, function(x) x[, noise := replace(noise, time_frame == max(time_frame), TRUE)])

  #cell_split <-  lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := replace(Mean_Grey, signal == TRUE & noise == FALSE,NaN)])

  #cell_split <- lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := approxfun(which(!is.na(Mean_Grey_wo_peaks)), na.omit(Mean_Grey_wo_peaks))(seq_along(Mean_Grey_wo_peaks))])

  #data <- do.call(rbind, cell_split)
  #data$Mean_Grey_wo_peaks[which(is.na(data$Mean_Grey_wo_peaks))] <- data$local_mean[which(is.na(data$Mean_Grey_wo_peaks))]

  print("mean_grey wo peaks ok")

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

print("ok")

  return(data)
}
