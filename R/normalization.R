
#' norm_df
#'
#' A wrapper which executes z score noralization, deltaf/f normalization
#' and computes a smoothed version of it
#'
#' @param data a data frame output from prepareData and back_estimate
#' @param cov_split A list of data frame with each element being the data for one coverslip (one technical replicate)
#' @param dim_list A list with each element being the dimension of a data frame in cov_split
#'
#' @return An updated data frame with four new columns : z_score (the values transformed in z_score)
#' and the smoothed version of the z_score ; delta_f_f (the values normalized by delta f /f method)
#' and its smoothed version
#' @export
#'
#'
#' @examples
norm_df <- function(data, var = c("raw", "poly", "gam", "linear", "quantile","back"),
                    width, reference = c("baseline","estimate")){


  data$coverslip <- as.character(data$coverslip)

  cov_split <- split(data, data$coverslip)
  dim_list <- lapply(cov_split, function(x) dim(dplyr::filter(x, Cell_id == x$Cell_id[[1]]))[1])

  data_z <- z_score(data, var = var, cov_split = cov_split, dim_list = dim_list,
                    reference = reference)
  print("z score computed")

  data_d <- delta_f(data_z, var = var)
  print("delta f / f computed")


  #smoothing the z score and the delta f variables
  smooth_df<- lapply(data_d, function(x) data.table::setDT(x)[, ':=' (smooth_z = gplots::wapply(time_frame,
                                                                                                z_score, fun = mean, n = length(time_frame), width = width, method = "nobs", drop.na = FALSE)[[2]],
                                                                      smooth_delta = gplots::wapply(time_frame,
                                                                                                    delta_f_f, fun = mean, n = length(time_frame), width = width, method = "nobs", drop.na = FALSE)[[2]]),
                                                              by = Cell_id])


  smooth_df <- do.call(rbind, smooth_df)

  return(smooth_df)
}


#' delta_f
#'
#' Normalizes the denoised fluorescence trace with the delta f/f method taking the predicted background as a local baseline
#'
#' @param data a dataframe output from back_estimate function
#' @param var the variable which will be normalized (one of poly, gam, linear, quantile)
#' @param cov_split a list of data table, each element being one coverslip (one technical replicate)
#'
#'
#' @return the cov_split list within which, each datatable has a new column : the normalized fluorescence values
#' @export
#'
#' @examples
delta_f <- function(cov_split, var = c("poly", "gam", "linear", "quantile", "back")){

  if(var == "poly"){
    cov_split <- lapply(cov_split, function(x) x[, delta_f_f := poly_detrended / poly_fit, by = Cell_id])
  }

  if(var == "gam"){
    cov_split <- lapply(cov_split, function(x) x[, delta_f_f := gam_detrended / gam_fit, by = Cell_id])
  }

  if(var == "linear"){
    cov_split <- lapply(cov_split, function(x) x[, delta_f_f := linear_detrended / linear_fit, by = Cell_id])
  }

  if(var == "quantile"){
    cov_split <- lapply(cov_split, function(x) x[, delta_f_f := quantile_detrended / local_quantile, by = Cell_id])
  }

  if(var == "back"){
    cov_split <- lapply(cov_split, function(x) x[, delta_f_f := background_detrended / background, by = Cell_id])
  }

  return(cov_split)
}



#' z_score
#'
#' @param data The output of prepareData function or a data frame with similar elements
#' @param raw An optional logical parameter. If TRUE, The z score is computed on raw data
#' @param detrended An optional logical parameter. If TRUE, The z score is computed on detrended data
#' @param poly_std An optional logical parameter. If TRUE, The z score is computed on data normalized through polynomial fit
#' @param cov_split A list of data frame with each element being the data for one coverslip (one technical replicate)
#' @param dim_list
#'
#' @return the normalized values (z score) as a vector of floating numbers in the same order of the original data frame
#' @export
#'
#'
#'
#'
#' @examples
z_score <- function(data, var = c("raw", "poly", "gam", "linear", "quantile", "back"),
                    cov_split, dim_list, reference = c("baseline","estimate") ) {

  print("computing_z_score")

  . <- NULL
  mean_baseline <- NULL
  sd_baseline <- NULL
  Mean_Grey <- NULL
  Cell_id <- NULL
  stimulus <- NULL
  mean_baseline <- NULL
  sd_baseline <- NULL
  poly_detrended <- NULL
  gam_detrended <- NULL
  quantile_detrended <- NULL

  # Storing the number of cells for each coverslip to replicate the dim this number of times
  ncells_list <- vector(mode = "list", length = length(cov_split))

  ncells_list <- purrr::map(cov_split, function(x) length(table(x$Cell_id)))


  dim_list_final <- list()

  dim_list_final <- unlist(purrr::map2(dim_list, ncells_list, rep))



  dt <- data.table::setDT(data)



  if(var == "raw"){
    mean <- dt[, .(mean = mean(Mean_Grey)), .(Cell_id, stimulus)][stimulus == "1.Baseline"]
    mean_base_list <- split(mean, mean$Cell_id)

    # Creating a vector of mean baseline for each coverslip, that has the same number of lines as each coverslip
    #mean_vec_list <- vector(mode = "list", length = length(mean_baseline$mean)*sum(unlist(dim_list)))

    dt <- dt[, mean_baseline := unlist(purrr::map2(mean_base_list, dim_list_final, function(x,y) rep(x$mean[[1]], times = y)))]

    sd <- dt[, .(sd = stats::sd(Mean_Grey)), .(Cell_id, stimulus)][stimulus == "1.Baseline"]
    sd_base_list <- split(sd, sd$Cell_id)

    dt <- dt[, sd_baseline := unlist(purrr::map2(sd_base_list, dim_list_final, function(x,y) rep(x$sd[[1]], times = y)))]

    z_score <- dt[, z_score := (Mean_Grey - mean_baseline) / sd_baseline, by = list(Cell_id, stimulus)]
  }


  if(var == "poly"){
    mean <- dt[, .(mean = mean(poly_detrended)), .(Cell_id, stimulus)][stimulus == "1.Baseline"]
    mean_base_list <- split(mean, mean$Cell_id)

    # Creating a vector of mean baseline for each coverslip, that has the same number of lines as each coverslip

    dt <- dt[, mean_baseline := unlist(purrr::map2(mean_base_list, dim_list_final, function(x,y) rep(x$mean[[1]], times = y)))]

    sd <- dt[, .(sd = stats::sd(poly_detrended)), .(Cell_id, stimulus)][stimulus == "1.Baseline"]
    sd_base_list <- split(sd, sd$Cell_id)

    dt <- dt[, sd_baseline := unlist(purrr::map2(sd_base_list, dim_list_final, function(x,y) rep(x$sd[[1]], times = y)))]

    z_score <- dt[, z_score := (poly_detrended - mean_baseline) / sd_baseline, by = list(Cell_id, stimulus)]

  }

  if(var == "gam"){
    mean <- dt[, .(mean = mean(gam_detrended)), .(Cell_id, stimulus)][stimulus == "1.Baseline"]
    mean_base_list <- split(mean, mean$Cell_id)


    # Creating a vector of mean baseline for each coverslip, that has the same number of lines as each coverslip

    dt <- dt[, mean_baseline := unlist(purrr::map2(mean_base_list, dim_list_final, function(x,y) rep(x$mean[[1]], times = y)))]

    sd <- dt[, .(sd = stats::sd(gam_detrended)), .(Cell_id, stimulus)][stimulus == "1.Baseline"]
    sd_base_list <- split(sd, sd$Cell_id)

    dt <- dt[, sd_baseline := unlist(purrr::map2(sd_base_list, dim_list_final, function(x,y) rep(x$sd[[1]], times = y)))]

    z_score <- dt[, z_score := (gam_detrended - mean_baseline) / sd_baseline, by = list(Cell_id, stimulus)]

  }


  if(var == "quantile"){
    mean <- dt[, .(mean = mean(quantile_detrended)), .(Cell_id, stimulus)][stimulus == "1.Baseline"]
    mean_base_list <- split(mean, mean$Cell_id)

    # Creating a vector of mean baseline for each coverslip, that has the same number of lines as each coverslip

    dt <- dt[, mean_baseline := unlist(purrr::map2(mean_base_list, dim_list_final, function(x,y) rep(x$mean[[1]], times = y)))]

    sd <- dt[, .(sd = stats::sd(quantile_detrended)), .(Cell_id, stimulus)][stimulus == "1.Baseline"]
    sd_base_list <- split(sd, sd$Cell_id)

    dt <- dt[, sd_baseline := unlist(purrr::map2(sd_base_list, dim_list_final, function(x,y) rep(x$sd[[1]], times = y)))]

    z_score <- dt[, z_score := (quantile_detrended - mean_baseline) / sd_baseline, by = list(Cell_id, stimulus)]

  }


  if(var == "back" & reference == "estimate"){

    dt[, mean_baseline := mean(.SD[is.na(signal) & stimulus == "1.Baseline"]$background_detrended,na.rm = TRUE), .(Cell_id)]
    dt[, mean_baseline := mean(mean_baseline,na.rm = TRUE), .(Cell_id)]

    dt[, sd_baseline := sd(.SD[is.na(signal) & stimulus == "1.Baseline"]$background_detrended,na.rm = TRUE), .(Cell_id)]
    dt[, sd_baseline := mean(sd_baseline,na.rm = TRUE), .(Cell_id)]

    z_score <- dt[, z_score := (background_detrended - mean_baseline) / sd_baseline, by = Cell_id]

  }

  if(var == "back" & reference == "baseline"){

    #mean <- dt[is.na(signal), .(mean = mean(background_detrended)), .(Cell_id)]
    # mean_base_list <- split(mean, mean$Cell_id)

    dt[, mean_baseline := mean(.SD[stimulus == "1.Baseline"]$background_detrended,na.rm = TRUE), .(Cell_id)]
    dt[, mean_baseline := mean(mean_baseline,na.rm = TRUE), .(Cell_id)]

    dt[, sd_baseline := sd(.SD[stimulus == "1.Baseline"]$background_detrended,na.rm = TRUE), .(Cell_id)]
    dt[, sd_baseline := mean(sd_baseline,na.rm = TRUE), .(Cell_id)]

    # Creating a vector of mean baseline for each coverslip, that has the same number of lines as each coverslip

    z_score <- dt[, z_score := (background_detrended - mean_baseline) / sd_baseline, by = Cell_id]

  }


  return(split(z_score, z_score$coverslip))
}


