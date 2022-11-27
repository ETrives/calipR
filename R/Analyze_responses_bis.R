

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

  tot_cells <- length(unique(df_tidy$Cell_id))
  n_responders <- length(unique(data$Cell_id))
  prop_resp <- n_responders / tot_cells

  return(list("N_responders" = n_responders,"Prop_responders" = prop_resp, "N_cells" = tot_cells))
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
Count_responders_stim <- function(data, df_tidy, n_cells){

  #data$Start_peak_stimulus <- str_replace_all(data$Start_peak_stimulus, "[12345.]", "")

  n_peaks_stim <- table(data$Cell_id, data$Start_peak_stimulus)


  counts <- lapply(colnames(n_peaks_stim), function(x) stim_response(x,n_peaks_stim, n_cells))


  responses <- lapply(seq_along(1:length(counts[[1]])), function(x) lapply(counts, function(y) y[[x]]))


  df_final <- data.frame(Stimulus = colnames(n_peaks_stim))
  df_final$Resp <- unlist(responses[[2]])
  df_final$Non_Resp <- unlist(responses[[3]])
  df_final$Proportion_of_responders <- unlist(responses[[4]])
  df_final$Proportion_of_total_cells <- unlist(responses[[5]])

  return(list(n_peaks_stim, "n_responders_stim" = df_final))
}





stim_response <- function(x, n_peaks_stim, n_cells){

  response <- sum(n_peaks_stim[,x] != 0)
  non_response <- sum(n_peaks_stim[,x] == 0)
  resp_prop <- response / (response + non_response)
  resp_tot_prop <- response / n_cells

  return(list(x, response, non_response, resp_prop, resp_tot_prop))
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

  res_tot <- rstatix::cochran_qtest(data, Response~stimulus|Cell_id)
  res_post_hoc <- rstatix::pairwise_mcnemar_test(data, Response~stimulus|Cell_id)

  return(list(res_tot, res_post_hoc))
}
