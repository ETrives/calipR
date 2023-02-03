library(tidyr)

#' cell_sort
#'
#' @param df
#' @param pat
#' @param duration_in_seconds
#' @param frame_rate
#' @param id
#' @param multiple
#' @param compare_groups
#' @param groups
#'
#' @return
#' @export
#' @importFrom data.table :=

#' @examples
cell_sort <- function(df,pat,  duration_in_seconds, frame_rate, id,
                      multiple = TRUE, compare_groups = TRUE, groups, marker, marker_thresh){


  stimuli_full <- df$stimulus
  coverslip_full <- df$coverslip


  df <- dplyr::select(df, dplyr::matches(pat))
  dim <- dim(df)


  data_fus <- tidyr::gather(df,
                     key = "Cell_id",
                     value = "Mean_Grey", 1:dim[2])


  data_fus <- data_fus[,c("Cell_id", "Mean_Grey")]

  data_fus$Mean_Grey <- unlist(lapply(data_fus$Mean_Grey, function(x) if(is.character(x)) {str_replace_all( x, ",",".")} else{x}))
  data_fus$Mean_Grey <- unlist(lapply(data_fus$Mean_Grey, function(x) if(is.character(x)) {as.numeric(x)} else {x}))


  ### Adding the marker column
  if(is.null(marker)){}
  else{
  long_marker <- tidyr::gather(marker,
                               key = "Cell_id",
                               value = "Mean_Grey", 1:dim[2])
  print("long_marker")
  print(long_marker)
  print(dim)
  data_fus$marker <- rep(long_marker$Mean_Grey, each = dim[1])
  data_fus$marker_positive <-  data_fus$marker > marker_thresh
  print(data_fus)
}


  #Creating the variable stimulus and adding it to the data
  stimuli_final <- unlist(rep(stimuli_full,dim[2]))
  coverslip_final <- unlist(rep(coverslip_full,dim[2]))

  data_fus$stimulus <- stimuli_final
  data_fus$coverslip <- coverslip_final


  if(compare_groups == TRUE){
    data_fus$group <- rep(groups, dim(data_fus)[1])
  }

  data_fus$Cell_id <- rep(unlist(createId(data_fus, id)), each = dim[1])

  #creating a variable with time frame
  frame_vec <- rep(seq(from = 1, to = dim[1]),times = dim[2])
  data_fus$time_frame <- frame_vec

  # Adding a variable with the actual time in seconds
  time_sec <- rep(seq(from = 1/ frame_rate, to = dim[1]/frame_rate, by = 1/frame_rate ),times = dim[2])
  data_fus$time_seconds <- time_sec

  duration <- duration_in_seconds * frame_rate

  # Adding a variable tracking the frame with each stimulus being the zero reference :

  data_fus <- data.table::setDT(data_fus)[, Time_frame_stim := seq(c(1:length(stimulus)))]
  data_fus <- data_fus[, Stimulation := Time_frame_stim <= duration]

  return(data_fus)

}
