
#' stim_var
#'
#' @param data the original dataset
#' @param stimuli a character vector with the names of the stimuli used
#' @param each an integer vector with the number of repetitions for each stimulus
#' @return Returns a dataframe with a new variable called stimulus and prints
#' the output of a control to verify that the stimuli are repeated the right
#' number of times
#'
#' @export
#'
#'
#'
#' @examples


stim_var <- function(data, stimuli, each, frame_rate, coverslip_id){


  frame_list <- list()
  time <- purrr::map(each, function(x) as.numeric(x))


  # Converting minutes to frames
  frame_list <- lapply(time, min_to_f, frame_rate)

  frame_list <- append(frame_list, dim(data)[1])

  rep_each <- list()
  count <- 0

  for(i in frame_list){
    x <- i - count
    count <- count + x
    rep_each <- append(rep_each, x)
  }


  rep_each <- rep_each[-1]
  index = 1
  stimuli_full <- list()


  for (i in stimuli){
    stim <- rep(i, rep_each[index])
    stimuli_full <- append(stimuli_full, stim)
    index = index + 1
  }

  data$stimulus <- unlist(stimuli_full)

  data$coverslip <- rep(coverslip_id, dim(data)[1])

  #print("This is for you to verify that each stimulus
  #in the new dataframe has been repeated the right number of times:")
  #for (i in stimuli){
    #print(dim(dplyr::filter(data, stimulus == i))[1])
  #}



  return(data)


}
