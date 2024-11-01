
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
#' @param threshold
#' @param deconvolve_var
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
downstream_analysis <- function(data, rate, moving_thresh = 0.1, outlier_thresh = 2,
                               method = "back", norm_var = "back", reference = c("baseline","estimate"),
                                lambda = 100,
                                gam = 0.97, z_thresh = 3, delta_thresh = 0,
                                deconvolve_var = "background_detrended", compare_groups = FALSE, false_pos = c(TRUE, FALSE), one_cell = FALSE, simulation = FALSE,
                                pattern_matching = FALSE, posBank = list(),
                                negBank = list(), windows = NULL) {

  lambda <- as.numeric(lambda)

  gam <- as.numeric(gam)

  roll_width <- 10*rate

if(method == "back"){
  max_peak_bank <- lapply(posBank, function(x) max(x,na.rm=TRUE))
  idx_max <- lapply(seq(1,length(posBank)), function(x) which(posBank[[x]] == max_peak_bank[[x]]))

  peak_frame <- round(mean(unlist(idx_max), na.rm=TRUE))

}
  else{
    peak_frame <- 40
  }

  if(one_cell == FALSE){
  shiny::withProgress(message = "Analyzing Full Dataset", value = 0, detail = "Cleaning Data", {

  if(pattern_matching == TRUE){
  clean <-clean_data(data, moving_thresh, outlier_thresh, w = roll_width, method = "back")

  shiny::incProgress(1/5, detail = "Estimating Background")


  back <- patDetectR(clean, posBank, negBank, Var = "Mean_Grey")

  back <- backEstimatR(clean, back, roll_width*3)



  shiny::incProgress(1/5, detail = "Normalizing Data")

  norm <- norm_df(back, var = "back", width = roll_width, reference = reference)

  shiny::incProgress(1/5, detail = "Performing Deconvolution")

  deconvolved <- deconvolve(norm, lambda = lambda, gam = gam, var = "background_detrended")


  deconvolved <- peakExtraction(deconvolved, thresh = z_thresh, var = deconvolve_var,
                                peak_frame = peak_frame)


  }


  if(pattern_matching == FALSE){
  clean <- clean_data(data, moving_thresh, outlier_thresh, w = roll_width)

  shiny::incProgress(1/5, detail = "Estimating Background")

  back <- backEstimate(clean, method = method)

  shiny::incProgress(1/5, detail = "Normalizing Data")

  norm <- norm_df(back, var = norm_var, width = roll_width, reference = reference)

  shiny::incProgress(1/5, detail = "Performing Deconvolution")

  deconvolved <- deconvolve(norm, lambda = lambda, gam = gam, var = deconvolve_var)

  #deconvolved <- peakExtractR(deconvolved[[1]], deconvolved[[2]],
    #                          threshold = z_thresh, delta_threshold = delta_thresh,
    #                          var = deconvolve_var)

  deconvolved <- peakExtraction(deconvolved, thresh = z_thresh, var = deconvolve_var,
                                peak_frame = peak_frame)

  }


  if(length(deconvolved[[1]]$Cell_id) != 0){

    shiny::incProgress(1/5, detail = "Computing Statistics")


    res <- Analyze_Responses(deconvolved[[1]], clean, compare_groups = compare_groups,
                             one_cell = one_cell, simulation = simulation)
}




  })
  }

  if(one_cell == TRUE){

    shiny::withProgress(message = "Testing New Parameters", value = 0, detail = "Performing Deconvolution", {


      if(pattern_matching == TRUE){

        clean <- clean_data(data, moving_thresh, outlier_thresh, w = roll_width,
                            method = "back")

        back <- patDetectR(clean, posBank,
                           negBank, Var = "Mean_Grey")

        back <- backEstimatR(clean, back, w = roll_width*3)

        norm <- norm_df(back, var = "back", width = roll_width, reference = reference)

        deconvolved <- deconvolve(norm, lambda = lambda,gam = gam,
                                  var = "background_detrended")

        deconvolved <- peakExtraction(deconvolved, thresh = z_thresh, var = deconvolve_var,
                                      peak_frame = peak_frame)


      }



      if(pattern_matching == FALSE){


        clean <- clean_data(data, moving_thresh, outlier_thresh, w = roll_width)

        back <- backEstimate(clean, method = method)

        norm <- norm_df(back, var = norm_var, width = roll_width, reference = reference)

        deconvolved <- deconvolve(norm, lambda = lambda, gam = gam,
                                  var = deconvolve_var)



        deconvolved <- peakExtraction(deconvolved, thresh = z_thresh, var = deconvolve_var,
                                      peak_frame = peak_frame)


      }



    if(length(deconvolved[[1]]$Cell_id) != 0){


    }
    res <- "NO RES"
    norm <- data
    })
  }


return(list(deconvolved[[1]], deconvolved[[2]], res))
}


