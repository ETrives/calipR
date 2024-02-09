
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
                                gam = 0.97, constraint = T, threshold = 3,
                                deconvolve_var = "gam_detrended", borders_range = 50,
                                time_thresh = 1, compare_groups = FALSE, false_pos = c(TRUE, FALSE), one_cell = FALSE, simulation = FALSE,
                                pattern_matching = FALSE, posBank = list(),
                                negBank = list(), windows = c(30,70,100), steps = c(5,10,50)) {

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

  if(pattern_matching == TRUE){
  clean <-clean_data(data, moving_thresh, outlier_thresh, mean_width,
                              CN_DPA_width, DPA_width, mean_width_diff, method = "back")

  shiny::incProgress(1/5, detail = "Estimating Background")


  back <- patDetectR(clean, windows, steps, new_len = 30, posBank,
                                       negBank, Var = "Mean_Grey")
print("patdec ok" )
  back <- backEstimatR(clean, back)

  print("back ok" )

  shiny::incProgress(1/5, detail = "Normalizing Data")

  norm <- norm_df(back, var = "back", width = norm_width)

  print("norm ok" )

  shiny::incProgress(1/5, detail = "Performing Deconvolution")

  deconvolved <- deconvolve(norm, lambda = lambda, gam = gam, constraint = constraint,
                            threshold = threshold,var = "background_detrended")

  print("deconvolve ok" )


  }


  if(pattern_matching == FALSE){
  clean <- clean_data(data, moving_thresh, outlier_thresh, mean_width,
                              CN_DPA_width, DPA_width, mean_width_diff)
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
  }






  print("deconvolved = ok")
  print(deconvolved[[1]]$Cell_id)

  print(deconvolved[[1]])


  if(length(deconvolved[[1]]$Cell_id) != 0){

  if(false_pos == TRUE){
  shiny::incProgress(1/5, detail = "Removing Estimated False Positives")

  deconvolved <- keep_best_peaks(deconvolved)

  print("best = ok")
  }

    shiny::incProgress(1/5, detail = "Computing Statistics")

    print(deconvolved[[1]])
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
                                  constraint = constraint,threshold = threshold,
                                  var = "background_detrended")


      }



      if(pattern_matching == FALSE){


        clean <- clean_data(data, moving_thresh, outlier_thresh, mean_width,
                            CN_DPA_width, DPA_width, mean_width_diff)

        back <- backEstimate(clean, method = method)
        print("back = OK")


        norm <- norm_df(back, var = norm_var, width = norm_width)
        print("norm = OK")

        print(norm)

        deconvolved <- deconvolve(norm, lambda = lambda, gam = gam, constraint = constraint,
                                  threshold = threshold, var = deconvolve_var)

      }


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


print("YEAH new one !")
return(list(deconvolved[[1]], deconvolved[[2]], res))
}


##### Version simplifiée semble être bien intégrée mais faudra vérifier en faisant
### différents tests avant de remplacer l'autre fonction.

downstream_analysis_travaux <- function(data, moving_thresh = 0.1, outlier_thresh = 2, mean_width = 20, DPA_width = 10, CN_DPA_width = 20,
                                mean_width_diff = 10, method = "gam", norm_var = "gam",
                                norm_width = 10, lambda = 100,
                                gam = 0.97, constraint = T, threshold = 3,
                                deconvolve_var = "gam_detrended", borders_range = 50,
                                time_thresh = 1, compare_groups = FALSE, false_pos = c(TRUE, FALSE), one_cell = FALSE, simulation = FALSE,
                                pattern_matching = FALSE, posBank = list(),
                                negBank = list(), windows = c(30,70,100), steps = c(5,10,50)) {

  lambda <- as.numeric(lambda)
  print(lambda)
  gam <- as.numeric(gam)
  print(gam)
  threshold <- as.numeric(threshold)
  print(threshold)
  borders_range <- as.integer(borders_range)
  print(borders_range)



  shiny::withProgress(message = "Analyzing Full Dataset", value = 0, detail = "Cleaning Data", {

    if(pattern_matching == TRUE){
      clean <-clean_data(data, moving_thresh, outlier_thresh, mean_width,
                         CN_DPA_width, DPA_width, mean_width_diff, method = "back")


      cell1 <- clean$Cell_id[[1]]
      plot(clean[Cell_id == cell1]$Mean_Grey, type = "l")


      shiny::incProgress(1/5, detail = "Estimating Background")

      back <- patDetectR(clean, windows, steps, new_len = 30, posBank,
                         negBank, Var = "Mean_Grey")
      print("patdec ok" )
      back <- backEstimatR(clean, back)

      print("back ok" )

      shiny::incProgress(1/5, detail = "Normalizing Data")

      norm <- norm_df(back, var = "back", width = norm_width)

      print("norm ok" )

      shiny::incProgress(1/5, detail = "Performing Deconvolution")

      deconvolved <- deconvolve(norm, var = "background_detrended")

      print("deconvolve ok" )

      print("cell1")

      print(cell1)


    }


    if(pattern_matching == FALSE){
      clean <- clean_data(data, moving_thresh, outlier_thresh, mean_width,
                          CN_DPA_width, DPA_width, mean_width_diff)

      cell1 <- clean$Cell_id[[1]]
      plot(clean[Cell_id == cell1]$Mean_Grey, type = "l")

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

      print("cell1")

      print(cell1)
    }



    if(length(deconvolved[[1]]$Cell_id) != 0){

      if(false_pos == TRUE){
        shiny::incProgress(1/5, detail = "Removing Estimated False Positives")

        deconvolved <- keep_best_peaks(deconvolved)

        print("best = ok")
      }

      shiny::incProgress(1/5, detail = "Computing Statistics")

      print(deconvolved[[1]])

      if(one_cell == FALSE){
        res <- Analyze_Responses(deconvolved[[1]], clean, compare_groups = compare_groups,
                                 one_cell = one_cell, simulation = simulation)

      }

      if(one_cell == TRUE){
        res <- Analyze_Responses(deconvolved[[1]], clean, compare_groups = compare_groups,
                                 one_cell = one_cell, simulation = simulation)

      }
    }





    else{
      res <- "NO RES"
      norm <- data
    }


  })

  print("New_New one ! ")

  return(list(deconvolved[[1]], deconvolved[[2]], res))
}





