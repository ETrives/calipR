
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
downstream_analysis <- function(data, moving_thresh = 0.1, outlier_thresh = 2, mean_width = 20, DPA_width = 10, CN_DPA_width = 20,
                                mean_width_diff = 10, method = "gam", norm_var = "gam",
                                norm_width = 10, lambda = 100,
                                gam = 0.97, constraint = T, threshold = 3,
                                deconvolve_var = "gam_detrended", borders_range = 50,
                                time_thresh = 1, compare_groups = FALSE, false_pos = c(TRUE, FALSE), one_cell = FALSE, simulation = FALSE) {

  lambda <- as.numeric(lambda)
  print(lambda)
  gam <- as.numeric(gam)
  print(gam)
  threshold <- as.numeric(threshold)
  print(threshold)
  borders_range <- as.integer(borders_range)
  print(borders_range)

  if(one_cell == FALSE){
  shiny::withProgress(message = "Analyzing Full Dataset", value = 0, detail = "Cleaning Data", {

  clean <- calipR::clean_data(data, moving_thresh, outlier_thresh, mean_width, CN_DPA_width, DPA_width, mean_width_diff)


    print("cleaning = OK")

  shiny::incProgress(1/5, detail = "Estimating Background")

  back <- calipR::backEstimate(clean, method = method)
  print("back = OK")

  shiny::incProgress(1/5, detail = "Normalizing Data")

  norm <- calipR::norm_df(back, var = norm_var, width = norm_width)
  print("norm = OK")

  print(norm)

  shiny::incProgress(1/5, detail = "Performing Deconvolution")

  deconvolved <- deconvolve(norm, lambda = lambda, gam = gam, constraint = constraint,
                            threshold = threshold, var = deconvolve_var)

  print("deconvolved = ok")
  print(deconvolved[[1]]$Cell_id)

  print(deconvolved[[1]])


  if(length(deconvolved[[1]]$Cell_id) != 0){

  if(false_pos == TRUE){
  shiny::incProgress(1/5, detail = "Removing Estimated False Positives")

  deconvolved <- keep_best_peaks(deconvolved)

  print("best = ok")
}
}



  shiny::incProgress(1/5, detail = "Computing Statistics")

  print(deconvolved[[1]])
  res <- Analyze_Responses(deconvolved[[1]], clean, compare_groups = compare_groups,
                                   one_cell = FALSE, simulation = simulation)
  })
  }

  if(one_cell == TRUE){

    shiny::withProgress(message = "Testing New Parameters", value = 0, detail = "Performing Deconvolution", {

    deconvolved <- deconvolve(data, lambda = lambda, gam = gam, constraint = constraint,
                              threshold = threshold, var = deconvolve_var)

    print("deconvolved :")
    print(deconvolved)
    print("deconvolved = ok")
    print(deconvolved[[1]]$Cell_id)



    if(length(deconvolved[[1]]$Cell_id) != 0){

      if(false_pos == TRUE){
        deconvolved <- keep_best_peaks(deconvolved)

        print(deconvolved)


        #else{
          #shiny::incProgress(1/2, detail = "Finding Peaks Borders")

          #borders <- calipR::find_borders(best, range = borders_range)
        #}
      }

      #if(false_pos == FALSE){
        #shiny::incProgress(1/2, detail = "Finding Peaks Borders")

        #borders <- calipR::find_borders(deconvolved, range = borders_range)
      #}

    }
    res <- "NO RES"
    norm <- data
    })
  }

print("finished Downstream analysis")
print(deconvolved)


print("YEAH")
return(list(deconvolved[[1]], deconvolved[[2]], res))
}
