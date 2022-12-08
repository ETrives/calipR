
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
z_score <- function(data, var = c("raw", "poly", "gam", "linear", "quantile"), cov_split, dim_list ) {

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
    #mean_vec_list <- vector(mode = "list", length = length(mean_baseline$mean)*sum(unlist(dim_list)))

    dt <- dt[, mean_baseline := unlist(purrr::map2(mean_base_list, dim_list_final, function(x,y) rep(x$mean[[1]], times = y)))]

    sd <- dt[, .(sd = stats::sd(Mean_Grey)), .(Cell_id, stimulus)][stimulus == "1.Baseline"]
    sd_base_list <- split(sd, sd$Cell_id)

    dt <- dt[, sd_baseline := unlist(purrr::map2(sd_base_list, dim_list_final, function(x,y) rep(x$sd[[1]], times = y)))]

    z_score <- dt[, z_score := (poly_detrended - mean_baseline) / sd_baseline, by = list(Cell_id, stimulus)]

  }

  if(var == "gam"){
    mean <- dt[, .(mean = mean(gam_detrended)), .(Cell_id, stimulus)][stimulus == "1.Baseline"]
    mean_base_list <- split(mean, mean$Cell_id)


    # Creating a vector of mean baseline for each coverslip, that has the same number of lines as each coverslip
    #mean_vec_list <- vector(mode = "list", length = length(mean_baseline$mean)*sum(unlist(dim_list)))

    dt <- dt[, mean_baseline := unlist(purrr::map2(mean_base_list, dim_list_final, function(x,y) rep(x$mean[[1]], times = y)))]

    sd <- dt[, .(sd = stats::sd(Mean_Grey)), .(Cell_id, stimulus)][stimulus == "1.Baseline"]
    sd_base_list <- split(sd, sd$Cell_id)

    dt <- dt[, sd_baseline := unlist(purrr::map2(sd_base_list, dim_list_final, function(x,y) rep(x$sd[[1]], times = y)))]

    z_score <- dt[, z_score := (gam_detrended - mean_baseline) / sd_baseline, by = list(Cell_id, stimulus)]

  }


  if(var == "quantile"){
    mean <- dt[, .(mean = mean(quantile_detrended)), .(Cell_id, stimulus)][stimulus == "1.Baseline"]
    mean_base_list <- split(mean, mean$Cell_id)

    # Creating a vector of mean baseline for each coverslip, that has the same number of lines as each coverslip
    #mean_vec_list <- vector(mode = "list", length = length(mean_baseline$mean)*sum(unlist(dim_list)))

    dt <- dt[, mean_baseline := unlist(purrr::map2(mean_base_list, dim_list_final, function(x,y) rep(x$mean[[1]], times = y)))]

    sd <- dt[, .(sd = stats::sd(Mean_Grey)), .(Cell_id, stimulus)][stimulus == "1.Baseline"]
    sd_base_list <- split(sd, sd$Cell_id)

    dt <- dt[, sd_baseline := unlist(purrr::map2(sd_base_list, dim_list_final, function(x,y) rep(x$sd[[1]], times = y)))]

    z_score <- dt[, z_score := (quantile_detrended - mean_baseline) / sd_baseline, by = list(Cell_id, stimulus)]

  }

  return(split(z_score, z_score$coverslip))
}
