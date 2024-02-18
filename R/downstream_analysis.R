
#' downstream_analysis
#'
#' A wrapper function used in the graphical user interface to process the already prepared data.
#' Can be used as a one line function to run the complete workflow on the output from prepareData or prepareata_track.
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
#' @param compare_groups
#' @param CN_DPA_width
#' @param false_pos
#' @param one_cell
#' @param simulation
#' @param pattern_matching
#' @param posBank
#' @param negBank
#' @param windows
#' @param steps
#'
#' @return
#' @export
#'
#' @examples
downstream_analysis <- function(data, moving_thresh = 0.1, outlier_thresh = 2, mean_width = 20, DPA_width = 10, CN_DPA_width = 20,
                                mean_width_diff = 10, method = "gam", norm_var = "gam",
                                norm_width = 10, lambda = 100,
                                gam = 0.97, constraint = T, z_thresh = 3, delta_thresh = 0,
                                deconvolve_var = "gam_detrended", borders_range = 50,
                                time_thresh = 1, compare_groups = FALSE, false_pos = c(TRUE, FALSE), one_cell = FALSE, simulation = FALSE,
                                pattern_matching = FALSE, posBank = list(),
                                negBank = list(), windows = c(30,70,100), steps = c(5,10,50)) {

  lambda <- as.numeric(lambda)

  gam <- as.numeric(gam)




  if(one_cell == FALSE){
  shiny::withProgress(message = "Analyzing Full Dataset", value = 0, detail = "Cleaning Data", {

  if(pattern_matching == TRUE){
  clean <-clean_data(data, moving_thresh, outlier_thresh, mean_width,
                              CN_DPA_width, DPA_width, mean_width_diff, method = "back")

  shiny::incProgress(1/5, detail = "Estimating Background")


  back <- patDetectR(clean, windows, steps, new_len = 30, posBank,
                                       negBank, Var = "Mean_Grey")

  back <- backEstimatR(clean, back)



  shiny::incProgress(1/5, detail = "Normalizing Data")

  norm <- norm_df(back, var = "back", width = norm_width)

  shiny::incProgress(1/5, detail = "Performing Deconvolution")

  deconvolved <- deconvolve(norm, lambda = lambda, gam = gam, constraint = constraint,
                            threshold = z_thresh, delta_threshold = delta_thresh, var = "background_detrended")


  }


  if(pattern_matching == FALSE){
  clean <- clean_data(data, moving_thresh, outlier_thresh, mean_width,
                              CN_DPA_width, DPA_width, mean_width_diff)

  shiny::incProgress(1/5, detail = "Estimating Background")

  back <- calipR::backEstimate(clean, method = method)

  shiny::incProgress(1/5, detail = "Normalizing Data")

  norm <- calipR::norm_df(back, var = norm_var, width = norm_width)

  shiny::incProgress(1/5, detail = "Performing Deconvolution")

  deconvolved <- deconvolve(norm, lambda = lambda, gam = gam, constraint = constraint,
                            threshold = z_thresh, delta_threshold = delta_thresh, var = deconvolve_var)
  }




  if(length(deconvolved[[1]]$Cell_id) != 0){

  if(false_pos == TRUE){
  shiny::incProgress(1/5, detail = "Removing Estimated False Positives")

  deconvolved <- keep_best_peaks(deconvolved)

  }

    shiny::incProgress(1/5, detail = "Computing Statistics")

 
    res <- Analyze_Responses(deconvolved[[1]], clean, compare_groups = compare_groups,
                             one_cell = one_cell, simulation = simulation)
}




  })
  }

  if(one_cell == TRUE){

    shiny::withProgress(message = "Testing New Parameters", value = 0, detail = "Performing Deconvolution", {


      if(pattern_matching == TRUE){

        clean <- clean_data(data, moving_thresh, outlier_thresh, mean_width,
                            CN_DPA_width, DPA_width, mean_width_diff, method = "back")

        back <- patDetectR(clean, windows, steps, new_len = 30, posBank,
                           negBank, Var = "Mean_Grey")

        back <- backEstimatR(clean, back)

        norm <- norm_df(back, var = "back", width = norm_width)

        deconvolved <- deconvolve(norm, lambda = lambda,gam = gam,
                                  constraint = constraint,threshold = z_thresh,
                                  delta_threshold = delta_thresh,
                                  var = "background_detrended")


      }



      if(pattern_matching == FALSE){


        clean <- clean_data(data, moving_thresh, outlier_thresh, mean_width,
                            CN_DPA_width, DPA_width, mean_width_diff)

        back <- backEstimate(clean, method = method)

        norm <- norm_df(back, var = norm_var, width = norm_width)

        deconvolved <- deconvolve(norm, lambda = lambda, gam = gam, constraint = constraint,
                                  threshold = z_thresh, delta_threshold = delta_thresh,
                                  var = deconvolve_var)

      }



    if(length(deconvolved[[1]]$Cell_id) != 0){

      if(false_pos == TRUE){
        deconvolved <- keep_best_peaks(deconvolved)

  
      }

 
    }
    res <- "NO RES"
    norm <- data
    })
  }


return(list(deconvolved[[1]], deconvolved[[2]], res))
}


