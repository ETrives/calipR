#' Analyze_Responses
#'
#' @param data
#' @param df_clean
#' @param compare_groups
#'
#' @return
#' @export
#'
#' @examples
Analyze_Responses <- function(data, df_clean, compare_groups = FALSE){

  data_count <- Count_responders(data, df_clean)


  if(compare_groups == FALSE){
    data_count_stim <- Count_responders_stim(data, df_clean)

  }

  if(compare_groups == TRUE) {
    data_count_stim <- Response_by_stim_and_groups(data, df_clean)
  }


  #peaks <- Peaks_summary(data, df_clean)

  #data_sum <- Peak_sum(peaks)

  #peak_sum <- data_sum$Peak_sum

  #total_peaks <- sum(unlist(peak_sum))

  #props_stim <- Peak_prop(peaks, data_sum)


  between_stim <- Compare_props(data_count_stim)

  #return(list(unlist(data_count), data_count_stim[[2]], "n_peaks" = total_peaks, props_stim, between_stim[[1]], between_stim[[2]]))

  return(list(data_count, data_count_stim[[2]], between_stim[[1]], between_stim[[2]]))
}



#' Peak_prop
#'
#' @param data_count
#' @param data_sum
#'
#' @return
#' @export
#'
#' @examples
Peak_prop <- function(data_count, data_sum){


  df <- filter(data_count, Peak_class == "standard")
  props <- data.frame(1:dim(df)[1])
  prop_vec <- list()
  stim_vec <- list()
  index = 1

  for(i in data_sum$Peak_class){
    df <- filter(data_count, Peak_class == i)

    prop_vec[[i]] <- df$n / data_sum$Peak_sum[[index]]
    stim_vec <- df$Stimulus

    index = index +1


  }

  props <- cbind(props, stim_vec, prop_vec)
  return(props[,-1])
}




#' Compare_props
#'
#' @param data a dataframe gathering the number and proportion of cells that responded to each stimulus (output from count_responders_stim())
#'
#' @return
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





#' Peak_sum
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
Peak_sum <- function(data) {

  n_data <- plyr::ddply(data, .(Peak_class), summarise, Peak_sum = sum(n))

  n_data$Class_prop <- n_data$Peak_sum / sum(n_data$Peak_sum)

  return(n_data)
}



#' Peaks_summary
#'
#' @param data
#' @param df_tidy
#'
#' @return
#' @export
#'
#' @examples
Peaks_summary <- function(data, df_tidy){

  #data$Start_peak_stimulus <- str_replace_all(data$Start_peak_stimulus, "[12345.]", "")

  res_std <- data.frame(table(data$Start_peak_stimulus, data$standard_peak  ))



  res_std <- dplyr::filter(res_std, Var2 == TRUE)


  res_early <- data.frame(table(data$Start_peak_stimulus, data$early_peak  ))


  res_early <- dplyr::filter(res_early, Var2 == TRUE)
  res_late <- data.frame(table(data$Start_peak_stimulus, data$late_peak  ))
  res_late <- dplyr::filter(res_late, Var2 == TRUE)

  res_final <- rbind(res_std, res_early, res_late)
  peak_class <- c("standard", "early", "late")
  res_final$Peak_class <- rep(peak_class, each = dim(res_std)[[1]])
  res_final <- res_final[,-2]
  names(res_final) <- c("Stimulus", "n", "Peak_class")

  return("N_peaks_per_class_and_stim"= res_final)
}


#' Two_to_one
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
Two_to_one <- function(x){

  if(x >= 1){
    y <- 1
  }

  else {
    y <- 0

  }

  return(y)
}
