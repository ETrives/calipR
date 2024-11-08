#' clean_data
#'
#' This function removes cells that are moving, dying, not responding or
#' with mean fluorescence values that are deviating more than n sd from the
#' population's sd
#'
#' @param moving_threshold
#' @param outlier_threshold
#' @param mean_width
#' @param DPA_width
#' @param CN_DPA_width
#' @param mean_width_diff
#' @param data a data.table output from prepareData function
#'
#' @return
#' @export
#'
#' @examples
clean_data <- function(data, moving_threshold, outlier_threshold, w, method = "DPA") {

  ncells_before <- length(unique(data$Cell_id))

  print(paste("Number of cells before cleaning", ncells_before, sep = ": " ))

  # Removing Cells with too much Nas :



  na_sum <- data.table::setDT(data)[, .(NA_sum = sum(is.na(get("Mean_Grey")))), by = Cell_id ]


  frame_sum <- data[, .(frame_sum = length(get("Mean_Grey"))), by = Cell_id ]


  na_ratio <- na_sum$NA_sum / frame_sum$frame_sum

  data$na_ratio <- unlist(purrr::map2(na_ratio, frame_sum$frame_sum, function(x,y)
                   rep(x,times = y)))


  # Removing cells that moved too much (fluorescence down to zero)

  zero_sum <- data[, .(zero_sum = sum((get("Mean_Grey") == 0))), by = Cell_id ]
  zero_ratio <- zero_sum$zero_sum / frame_sum$frame_sum


  data$zero_ratio <- unlist(purrr::map2(zero_ratio, frame_sum$frame_sum, function(x,y)
    rep(x,times = y)))
  data <- data[zero_ratio < moving_threshold & na_ratio < moving_threshold]


  # Computing a local mean for each data.table

  local_mean_fct <- function(x,y,z) gplots::wapply(x, y, fun = mean, n=length(z), width = w, method = "nobs")[[2]]

  data <- data[, local_mean := data[, .(local_mean = local_mean_fct(get("time_frame"), get("Mean_Grey"), get("time_frame"))), by = Cell_id]$local_mean]

  #data <- data[, Mean_Grey := data[, .(Mean_Grey = replace(get("Mean_Grey"), get("time_frame") == 1
                                             #                & get("Mean_Grey") == 0, get("local_mean")[1])), by = Cell_id]$Mean_Grey]





    first_d_fct <- function(x,y) doremi::calculate.gold(time = x, signal = y,
                                                        embedding = 2, n = 1)$dsignal[,2]

    data <- data[, first_derivative := data[, .(first_derivative = first_d_fct(get("time_seconds"), get("local_mean"))), by = Cell_id]$first_derivative]


    local_mean_diff_fct <- function(x,y,z) gplots::wapply(x, y, fun = mean, n=length(z), width = w, method = "nobs", drop.na = FALSE)[[2]]

    data <- data[, smooth_Diff := data[, .(smooth_Diff = local_mean_diff_fct(get("time_frame"), get("first_derivative"), get("time_frame"))), by = Cell_id]$smooth_Diff]

    if(method == "DPA") {

  cell_split <- split(data, data$Cell_id)
  cell_split <- lapply(cell_split, function(x) data.table::setDT(x)[, c("DPA", "CN_DPA") := list(DPA(x, w),  CN_DPA(x, w))])


  cell_split <- lapply(cell_split, function(x) x[, c("DPA", "CN_DPA") :=
                            list(replace(DPA, is.na(DPA),quantile_speed(DPA, probs = .5)),
                            replace(CN_DPA, is.na(CN_DPA), quantile_speed(CN_DPA, probs = .5)))])


  cell_split <- lapply(cell_split, function(x) data.table::setDT(x)[,
                      c("smooth_DPA", "smooth_CN_DPA") := list(gplots::wapply(x$time_frame, x$DPA, fun = mean, n=length(x$time_frame), width = 10, method = "nobs")[[2]],
                      gplots::wapply(x$time_frame, x$CN_DPA, fun = mean, n=length(x$time_frame), width = 10, method = "nobs")[[2]])])



  cell_split <- lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := replace(Mean_Grey, smooth_Diff > 1 |
                                                                                   smooth_DPA > quantile_speed(smooth_DPA, probs = .7, na.rm = T)|
                                                                                   smooth_CN_DPA > quantile_speed(smooth_CN_DPA, probs = .7, na.rm = T)|
                                                                                   Mean_Grey < (mean(Mean_Grey) - 1.5*stats::sd(Mean_Grey)),
                                                                                 NaN)])

  cell_split <- lapply(cell_split, function(x) x[, signal := ifelse(is.na(Mean_Grey_wo_peaks), 1, NA)])

  cell_split <- lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := replace(Mean_Grey_wo_peaks, time_frame == max(time_frame) & is.na(Mean_Grey_wo_peaks), local_mean[ max(time_frame)])])
  cell_split <- lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := replace(Mean_Grey_wo_peaks, time_frame == 1 & is.na(Mean_Grey_wo_peaks), local_mean[1])])



  cell_split <- lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := approxfun(which(!is.na(Mean_Grey_wo_peaks)), na.omit(Mean_Grey_wo_peaks))(seq_along(Mean_Grey_wo_peaks))])

  data <- do.call(rbind, cell_split)

  data$Mean_Grey_wo_peaks[which(is.na(data$Mean_Grey_wo_peaks))] <- data$local_mean[which(is.na(data$Mean_Grey_wo_peaks))]


  cell_split <- split(data, data$Cell_id)


  cell_split <- lapply(cell_split, function(x) x[, Mean_Grey_wo_peaks := gplots::wapply(x$time_frame, x$Mean_Grey_wo_peaks,
                                                                                        fun = function(x) quantile_speed(x, probs = .1), n = length(x$time_frame), width = 30, method = "nobs")[[2]]])

  data <- do.call(rbind, cell_split)
  data$Mean_Grey_wo_peaks[which(is.na(data$Mean_Grey_wo_peaks))] <- data$local_mean[which(is.na(data$Mean_Grey_wo_peaks))]

  cell_split <- split(data, data$Cell_id)

  data <- lapply(cell_split, function(x) data.table::setDT(x)[, Anormal_variation := LaplacesDemon::is.bimodal(Mean_Grey)])

  data <- do.call(rbind, cell_split)

  }

  print(paste("Number of cells after cleaning", length(unique(data$Cell_id)), sep = ": " ))

  print(paste("Removed", ncells_before - length(unique(data$Cell_id)), "cells", sep = " " ))


  return(data)

}



#' DPA
#'
#'Implementation of the derivative passing accumulation method
#'
#' @param DPA_width
#' @param data
#'
#' @return
#' @export
#'
#' @examples
DPA <- function(data, DPA_width){


  pos <- data$smooth_Diff
  pos[pos < 0] <- 0
  neg <-  data$smooth_Diff
  neg[neg > 0] <- 0

  neg <- abs(neg)

  pos_lag <- dplyr::lag(pos, n = DPA_width, default = 0)

  neg_lead <- dplyr::lead(neg, n = DPA_width, default = 0)


  res_DPA <- pos_lag + neg_lead
  res_DPA <- res_DPA + dplyr::lag(res_DPA, n = 20, default = 0)

  return(res_DPA)
}


#' CN_DPA
#'
#'Implementation of a constrained non negative variant of the derivative passing accumulation method.
#'The persistence of the "potential signal" can be adapted by lagging the initial positive vector
#'
#' @param data
#' @param CN_DPA_width
#'
#' @return
#' @export
#'
#' @examples
CN_DPA <- function(data, CN_DPA_width){

  pos <- data$smooth_Diff
  pos[pos < 0] <- 0

  res_CN_DPA <- lapply(seq(5, CN_DPA_width, by = 10), function(x) dplyr::lag(pos, n = x, default = 0))

  res <- rowSums(as.data.frame(res_CN_DPA))

  return(as.numeric(res))
}


# Quick Quantile function coming from here : https://gist.github.com/sikli/f1775feb9736073cefee97ec81f6b193
#' quantile_speed
#'
#' @param x
#' @param probs
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
quantile_speed <- function(x, probs = c(0.1, 0.9), na.rm = F) {

  if (na.rm) x <- x[!is.na(x)]

  n <- length(x)
  index <- 1 + (n - 1) * probs

  lo    <- floor(index)
  hi    <- ceiling(index)

  x  <- sort(x, partial = unique(c(lo, hi)))
  qs <- x[lo]

  i     <- 1:length(probs)
  h     <- index - lo
  qs    <- (1 - h) * qs + h * x[hi]
  qs

}



#' smoothing
#'
#' @param data
#' @param band
#' @param delta_f
#' @param z_score
#' @param raw
#'
#' @return
#' @export
#'
#' @importFrom stats ksmooth
#' @examples
smoothing <- function(data, band = 4, delta_f = FALSE, z_score = FALSE, raw = FALSE){

  if(raw == TRUE){
    k <- ksmooth(data$time_frame, data$Mean_Grey, kernel = "normal", bandwidth= band)
    data$Ksmooth_raw <- k$y

  }

  if(delta_f == TRUE){
    k <- ksmooth(data$time_frame, data$delta_f_f, kernel = "normal", bandwidth= band)
    data$Ksmooth_delta <- k$y

  }

  if(z_score == TRUE){
    k <- ksmooth(data$time_frame, data$z_score, kernel = "normal", bandwidth= band)
    data$Ksmooth_z <- k$y

  }

  return(data)
}


