

#' Count_responders
#'
#' @param data the first data frame output from find_rise or keep_best_peaks. It must be a data frame
#' with each row describing one response
#' @param df_tidy the full data frame after cleaning (the output from clean_data)
#'
#' @return the number of cells that responded at least once and the proportion of responses it corresponds to
#' @export
#'
#' @examples
Count_responders <- function(data, df_tidy){

  n_responders <- length(unique(data$Cell_id))
  prop_resp <- n_responders / length(unique(df_tidy$Cell_id))

  return(list("N_responders" = n_responders,"Prop_responders" = prop_resp))
}




#' Count_responders_stim
#'
#' @param data the first data frame output from find_rise
#' @param df_tidy the full but cleaned data frame (output from clean_data())
#'
#' @return a table summarizing the number of peaks for each stimulus (will be used in compare_props function)
#' and a data frame gathering the number/proportion of cells that responded to each stimulus
#' @export
#'
#' @examples
Count_responders_stim <- function(data, df_tidy){

  #data$Start_peak_stimulus <- str_replace_all(data$Start_peak_stimulus, "[12345.]", "")

  n_peaks_stim <- table(data$Cell_id, data$Start_peak_stimulus)


  counts <- lapply(colnames(n_peaks_stim), function(x) stim_response(x,n_peaks_stim))


  responses <- lapply(seq_along(1:length(counts[[1]])), function(x) lapply(counts, function(y) y[[x]]))


  df_final <- data.frame(Stimulus = colnames(n_peaks_stim))
  df_final$Resp <- unlist(responses[[2]])
  df_final$Non_Resp <- unlist(responses[[3]])
  df_final$Prop <- unlist(responses[[4]])

  return(list(n_peaks_stim, "n_responders_stim" = df_final))
}


stim_response <- function(x, n_peaks_stim){

  response <- sum(n_peaks_stim[,x] != 0)
  non_response <- sum(n_peaks_stim[,x] == 0)
  resp_prop <- sum(n_peaks_stim[,x] != 0) / (sum(n_peaks_stim[,x] != 0) + sum(n_peaks_stim[,x] == 0))

  return(list(x, response, non_response, resp_prop))

}




#' Compare_props
#'
#' @param data the full output from Count_responders_stim()
#'
#' @return a list with 2 elements. The first is the general summary of the cochran's q test
#' the second is the results of the pairwise comparisons between stimuli with McNemar test with correction for multiple comparisons
#' @export
#'
#' @examples
Compare_props <- function(data){

  for(i in colnames(data[[1]])){

    data[[1]][,i] <- unlist(lapply(data[[1]][,i], Two_to_one))
  }

  data <- data.frame(data[[1]])

  res_tot <- rstatix::cochran_qtest(data, Freq~Var2|Var1)
  res_post_hoc <- rstatix::pairwise_mcnemar_test(data, Freq ~ Var2|Var1)

  return(list(res_tot, res_post_hoc))
}
