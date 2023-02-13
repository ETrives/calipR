
# Nouvelle méthode de normalisation

# Idée 1 :

# récupérer Mean grey wo peaks et faire l'écart type sur la baseline de ça



# Idée 2 :
# sectionner mean grey wo peaks en x morceaux, calculer l'écart type sur chaque
# puis prendre la médiane


### Mais attention, il faut d'abord que que Mean_grey wo peaks soit detrended

# donc ;

# Mean_grey_wo_peaks - gam_fit

# je viens de faire la première idée. ça augmente pas mal le z score.
# problème : ça risque de détecter plus de faux positifs.
# si ça reste comme ça, il faut dans tous les cas mettre un seuil de sd plus
# élevé (> 5)


z_score_travaux <- function(data, var = c("raw", "poly", "gam", "linear", "quantile"), cov_split, dim_list, new = FALSE) {

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


  if(var == "poly" ){
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

  if(var == "gam" & new == FALSE){
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


  if(var == "gam" & new == TRUE){

    dt <- dt[, wo_peaks_detrended := Mean_Grey_wo_peaks - gam_fit]
    mean <- dt[, .(mean = mean(wo_peaks_detrended)), .(Cell_id, stimulus)][stimulus == "1.Baseline"]
    mean_base_list <- split(mean, mean$Cell_id)


    # Creating a vector of mean baseline for each coverslip, that has the same number of lines as each coverslip
    #mean_vec_list <- vector(mode = "list", length = length(mean_baseline$mean)*sum(unlist(dim_list)))

    dt <- dt[, mean_baseline := unlist(purrr::map2(mean_base_list, dim_list_final, function(x,y) rep(x$mean[[1]], times = y)))]

    sd <- dt[, .(sd = stats::sd(wo_peaks_detrended)), .(Cell_id, stimulus)][stimulus == "1.Baseline"]
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
