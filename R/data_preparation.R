###### Preparation of manual cell detection input (wide format) #######
#' prepareData
#'
#' This function is a wrapper which reads the raw fluorescence csv files (wide format),
#' transform it to long format and adds several meta in column (Cell_id, time, stimuli,
#' molecular marker, group, coverslip).
#'
#'
#' @param folder_name Path to the folder containing the fluorescence data and the meta.csv file
#' @param frame_rate Your Acquisition rate in Hertz (eg. 0.25 if it is 0.25 Hz, 100 if it is 100 Hz)
#' @param duration_in_seconds The time your stimulus takes to be in contact to your cells. Peaks found during this period will be attributed to the previous stimulus.
#'
#' @return
#' @export



#' @examples
prepareData <- function(folder_name, frame_rate, duration_in_seconds = 30,
                        marker_thresh, unit) {

  # Get the file names and store the content in a list of df :
  myFiles <- list.files(folder_name, pattern = "\\.csv", recursive = T, full.names = T)
  len_before <- length(myFiles)

  # Removing the meta_data file :

  myFiles <- myFiles[!stringr::str_detect(myFiles,pattern="meta") & !stringr::str_detect(myFiles,pattern="marker")]

  len_after <- length(myFiles)
  meta <- list.files(folder_name, pattern = "meta", recursive = T, full.names = T)


  df_list <- vector(mode = "list", length = length(myFiles))

  # Reading all the data files

  df_list <- lapply(myFiles, function(x) data.table::fread(x, skip = 1, header = FALSE))
  df_list <- lapply(df_list, function(x) x[,2:length(x)])
  df_list <- lapply(df_list, function(x) data.table::setnames(x, paste0(rep("Mean", length(x)), seq(1: length(x)))))

  # Checking if marker files have been added

  if(len_before - len_after > 1){
    marker <- list.files(folder_name, pattern = "marker", recursive = T, full.names = T)

    # Reading the files
    marker_list <- lapply(marker, function(x) data.table::fread(x, skip = 1, header = FALSE))
    marker_list <- lapply(marker_list, function(x) x[,2:length(x)])
    marker_list <- lapply(marker_list, function(x) data.table::setnames(x, paste0(rep("Mean", length(x)), seq(1: length(x)))))
  }

  else{
    marker_list <- NULL
  }


 # Get the coverslip number and add a Letter to it :

  index_cov <-length(stringr::str_split(myFiles[[1]], "/")[[1]])

  coverslip_id <- lapply(myFiles, function(x) as.integer(stringr::str_replace_all(stringr::str_split(x, "/")[[1]][index_cov], "[.csv.]", "")))

  letter_list <- LETTERS[seq(from = 1, to = ceiling(length(myFiles)/9))]
  cov_num <- rep(seq(from = 1, to = 9), times = length(letter_list))

  letter_list <- rep(letter_list, each = 9)

  if(length(letter_list[which(is.na(letter_list))]) != 0){

  na_cov <- length(letter_list[which(is.na(letter_list))])
  second_letter_list <- LETTERS[seq(from = 1, to = length(na_cov))]
  third_letter_list <- LETTERS[seq(from = 1, to = length(na_cov))]
  dbl <- paste0(second_letter_list, third_letter_list)
  letter_list[which(is.na(letter_list))] <- dbl
  }

  coverslip_id <- purrr::map2(letter_list, cov_num, function(x,y) paste(x,y,sep =""))

  # Fetching the stimuli informations :

  meta_df <- data.table::fread(meta)
  stimuli <- unique(meta_df$stimuli)
  stimuli <- paste(seq(1,length(stimuli)),stimuli, sep = ".")

  # now the time informations :

  each <- meta_df$timing
  each <- split(each, ceiling(seq_along(each)/length(stimuli)))

  # Get the pattern to find in the colnames for the cell_sort function :

  pattern <- substr(colnames(df_list[[1]])[[1]], 1,4)


  ### Fetching group information

  index_gr <-length(stringr::str_split(myFiles[[1]], "/")[[1]])-2

  group_list <- lapply(myFiles, function(x) stringr::str_split(x, "/")[[1]][index_gr])


    for(i in 1:length(df_list)){

      df_list[[i]] <- tidy_df(df_list[[i]],stimuli, each[[i]], pattern, duration_in_seconds,
                              frame_rate, coverslip_id = coverslip_id[[i]], id = i, multiple = TRUE,
                      group_list[[i]], marker_list[[i]], marker_thresh, unit)

    }


  df <- do.call(rbind, df_list)

  df$marker_positive <- as.factor(df$marker_positive)
  df$group <- as.factor(df$group)
  df$stimulus <- as.factor(df$stimulus)
  df$coverslip <- as.factor(df$coverslip)


  return(df)
}



#' tidy_df
#'
#' @param data the original raw dataset
#' @param stimuli a character vector with the names of the stimuli used
#' @param each an integer vector with the number of repetitions for each stimulus
#' @param pattern a character pattern in the colnames of the cells to be used
#' to gather the cells in one column
#'
#' @return A fully tidied dataset ready for filtering step
#' @export
#'
#' @examples

tidy_df <- function(data, stimuli, each, pattern, duration_in_seconds,
                    frame_rate, coverslip_id, id, multiple = FALSE, groups,
                    marker, marker_thresh, unit) {

  df_stim <- stim_var(data, stimuli, each, frame_rate, coverslip_id, unit)
  df_final <- cell_sort(df_stim, pattern, duration_in_seconds, frame_rate, id = id,
                        multiple = multiple, groups, marker, marker_thresh)


  return(df_final)
}


#' stim_var
#'
#'Takes the wide format data, the meta.csv file and the coverslip names. For each
#'data table (each file), it creates a stimulus variable which duplicates the name
#'of each stimulus the adequate number of times (depending on the time during which
#'each stimulus was applied), which is specified in the meta file.The coverslip
#'id is also replicated to match the number of line in each data table.
#'
#' @param data the original dataset
#' @param stimuli a character vector with the names of the stimuli used
#' @param each an integer vector with the number of repetitions for each stimulus
#' @return Returns a dataframe with a new variable called stimulus and coverslip
#'
#'
#' @export
#'
#'
#'
#' @examples


stim_var <- function(data, stimuli, each, frame_rate, coverslip_id, unit){

  frame_list <- list()
  time <- purrr::map(each, function(x) as.numeric(x))

  # Converting minutes to frames
  frame_list <- lapply(time, function(x) time_to_f(x,frame_rate, unit = unit))

  frame_list <- append(frame_list, dim(data)[1])

  rep_each <- list()
  count <- 0

  for(i in frame_list){
    x <- i - count
    count <- count + x
    rep_each <- append(rep_each, x)
  }

  rep_each <- rep_each[-1]

  stimuli_full <- data.table("stimuli" = stimuli,"n" = rep_each)[,new := list(rep(stimuli,n))]$new[[1]]

  data$stimulus <- stimuli_full

  data$coverslip <- rep(coverslip_id, dim(data)[1])


  return(data)


}



#' cell_sort
#'
#' Transforms the raw datatable in long format, creates a unique Identifier for each cell
#' and adds coverslip, timing, grouping and molecular marker informations.
#'
#' @param df
#' @param pat
#' @param duration_in_seconds
#' @param frame_rate
#' @param id
#' @param multiple
#' @param compare_groups
#' @param groups
#' @param marker
#' @param marker_thresh
#'
#' @return A long format data table with 8 columns : Mean_Grey, Cell_id, stimulus, coverslip, time_frame,
#' time_seconds, time_frame_stim, marker, marker_positive

#' @export
#' @importFrom data.table :=

#' @examples
cell_sort <- function(df,pat,  duration_in_seconds, frame_rate, id,
                      multiple = TRUE, groups, marker, marker_thresh){


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

    data_fus$marker <- rep(long_marker$Mean_Grey, each = dim[1])
    data_fus$marker_positive <-  data_fus$marker > marker_thresh

  }


  #Creating the variable stimulus and adding it to the data
  stimuli_final <- unlist(rep(stimuli_full,dim[2]))
  coverslip_final <- unlist(rep(coverslip_full,dim[2]))

  data_fus$stimulus <- stimuli_final
  data_fus$coverslip <- coverslip_final


  data_fus$group <- rep(groups, dim(data_fus)[1])


  data_fus$Cell_id <- rep(unlist(createId(data_fus, id)), each = dim[1])

  #creating a variable with time frame
  frame_vec <- rep(seq(from = 1, to = dim[1]),times = dim[2])
  data_fus$time_frame <- frame_vec

  # Adding a variable with the actual time in seconds
  time_sec <- rep(seq(from = 1/ frame_rate, to = dim[1]/frame_rate, by = 1/frame_rate ),times = dim[2])
  data_fus$time_seconds <- time_sec

  duration <- duration_in_seconds * frame_rate

  # Adding a variable tracking the frame with each stimulus being the zero reference :

  data_fus <- data.table::setDT(data_fus)[, Time_frame_stim := seq(1:length(Mean_Grey)), by = .(Cell_id, stimulus)]
  data_fus <- data_fus[, Stimulation := Time_frame_stim <= duration]

  return(data_fus)

}


########## Preparation of trackmate input (long format) #############


#' prepareData_track
#'
#' @param folder_name
#' @param frame_rate
#' @param duration_in_seconds
#' @param compare_groups
#'
#' @return
#' @export
#'
#' @examples
prepareData_track <- function(folder_name, frame_rate, target_rate = 0, duration_in_seconds = 30,
                               marker_thresh = 0) {


  # Get the file names and store the content in a list of df :
  myFiles <- list.files(folder_name, pattern = "\\.csv", recursive = T, full.names = T)
  len_before <- length(myFiles)

  # Removing the meta_data file :

  myFiles <- myFiles[!stringr::str_detect(myFiles,pattern="meta")]

  len_after <- length(myFiles)

  meta <- list.files(folder_name, pattern = "meta", recursive = T, full.names = T)

  df_list <- vector(mode = "list", length = length(myFiles))

  # Reading all the files
  df_list <- lapply(myFiles, function(x) data.table::fread(x))


  # Checking if marker files have been added

  if(len_before - len_after > 1){
    marker <- list.files(folder_name, pattern = "marker", recursive = T, full.names = T)

    # Reading the files
    marker_list <- lapply(marker, function(x) data.table::fread(x, skip = 1, header = FALSE))
    marker_list <- lapply(marker_list, function(x) x[,2:length(x)])
    marker_list <- lapply(marker_list, function(x) data.table::setnames(x, paste0(rep("Mean", length(x)), seq(1: length(x)))))
  }

  else{
    marker_list <- NULL
  }


  # Get the coverslip number and add a Letter to it :

  index_cov <-length(stringr::str_split(myFiles[[1]], "/")[[1]])

  coverslip_id <- lapply(myFiles, function(x) as.integer(stringr::str_replace_all(stringr::str_split(x, "/")[[1]][index_cov], "[.csv.]", "")))

  letter_list <- LETTERS[seq(from = 1, to = ceiling(length(myFiles)/9))]
  cov_num <- rep(seq(from = 1, to = 9), times = length(letter_list))

  letter_list <- rep(letter_list, each = 9)

  if(length(letter_list[which(is.na(letter_list))]) != 0){

    na_cov <- length(letter_list[which(is.na(letter_list))])
    second_letter_list <- LETTERS[seq(from = 1, to = length(na_cov))]
    third_letter_list <- LETTERS[seq(from = 1, to = length(na_cov))]
    dbl <- paste0(second_letter_list, third_letter_list)
    letter_list[which(is.na(letter_list))] <- dbl
  }

  coverslip_id <- paste(letter_list, cov_num, sep ="")

  # Fetching the stimuli informations :

  meta_df <- data.table::fread(meta)
  stimuli <- unique(meta_df$stimuli)
  stimuli <- paste(seq(1,length(stimuli)),stimuli, sep=".")


  ### Fetching group information

  index_gr <-length(stringr::str_split(myFiles[[1]], "/")[[1]])-2

  group_list <- lapply(myFiles, function(x) stringr::str_split(x, "/")[[1]][index_gr])

  # now the time informations :

  each <- meta_df$timing
  each <- split(each, ceiling(seq_along(each)/length(stimuli)))

    for(i in 1:length(df_list)){

      df_list[[i]] <- trackmateInput(df_list[[i]],stimuli, each[[i]],
                                     target_rate, coverslip_id = coverslip_id[[i]], id = i, group_list[[i]],
                                     duration_in_seconds, marker_list[[i]], marker_thresh)

    }



  df <- do.call(rbind, df_list)


  return(df)
}


#' trackmateInput
#'
#' @param file
#' @param stimuli
#' @param each
#' @param frame_rate
#' @param coverslip_id
#' @param id
#' @param group
#' @param duration_in_seconds
#'
#' @return
#' @export
#'
#' @examples
trackmateInput <- function(file, stimuli, each, target_rate, coverslip_id, id,
                           group, duration_in_seconds,marker, marker_thresh){

  track <- file[-c(1,2,3),]
  track_final <- data.table::setDT(track)[, .(Cell_id =TRACK_ID, Mean_Grey = as.numeric(MEAN_INTENSITY_CH1),
                                              coverslip = coverslip_id,
                                              time_frame = POSITION_T)]

  Ids <- unlist(createId(track_final, coverslip_id))


  cell_split <- split(track_final, track_final$Cell_id)
  cell_split <- purrr::map2(cell_split, Ids, function(x,y) x[, Cell_id := y])

  #retrieving the total number of frames if the cell was detected from the beginning
  #until the end

  dim <- max(unlist(lapply(cell_split, function(x) length(x$Cell_id))))

  cell_split <- lapply(cell_split, function(x) if(length(x$Cell_id) >= dim/2){x})

  data <- do.call(rbind, cell_split)

  n_cells <- length(unique(data$Cell_id))

  # matching stimuli :


  time <- lapply(each, function(x) as.numeric(x))

  # Converting minutes to frames
  frame_list <- lapply(time, function(x) time_to_f(x,target_rate, unit = unit))


  frame_list <- append(frame_list, dim)


  rep_each <- list()
  count <- 0

  for(i in frame_list){

    x <- i - count
    count <- count + x
    rep_each <- append(rep_each, x)
  }


  rep_each <- rep_each[-1]

  stimuli_full <- data.table("stimuli" = stimuli,"n" = rep_each)[,new := list(rep(stimuli,n))]$new[[1]]



  # Adding a variable stimulus :

  data$stimulus <- rep(stimuli_full, times = n_cells)

  # Adding a variable to track the coverslip Id :
  coverslip_full <- rep(coverslip_id, dim)

  data$coverslip <- unlist(rep(coverslip_full,n_cells))

  # Adding a variable with the actual time in seconds

  data$time_seconds <- rep(seq(from = 1/ frame_rate, to = dim/ frame_rate, by = 1/frame_rate ), times = n_cells)

  duration <- duration_in_seconds * frame_rate

  # Adding a variable tracking the frame with each stimulus being the zero reference :

  data <- data[, Time_frame_stim := seq(c(1:.N)), by = stimulus]
  data <- data[, Stimulation := Time_frame_stim <= duration]
  data <- data[, group := rep(group,each = .N)]

  ### Adding the marker column
  if(is.null(marker)){
    data$marker_positive <- FALSE
  }
  else{
    long_marker <- tidyr::gather(marker,
                                 key = "Cell_id",
                                 value = "Mean_Grey", 1:dim[2])

    data$marker <- rep(long_marker$Mean_Grey, each = dim[1])
    data$marker_positive <-  data$marker > marker_thresh

  }


  return(data)
}





######## Others ###########

#' createId
#'
#' Creates a unique identifier for each Cell which keeps the original
#' cell order in the raw csv file. The identifier contains the coverslip
#  id coupled with a combination of 3 letters. This prevents alphanumeric reordering
#' of the cells.
#'
#' @param df
#' @param coverslip_id
#'
#' @return
#' @export
#'
#' @examples
createId <- function(df, coverslip_id){

  cells <- names(table(df$Cell_id))
  # initializing an empty list
  id_list <- vector(mode = "list", length = length(cells))


  # splitting the cells in chunks of 26 (letters of the alphabet)
  # to be able to iterate over the chunks
  cells <- split(cells, ceiling(seq_along(cells)/26))

  # there are more than 26 chunks so resplitting the chunks
  cells <- split(cells, ceiling(seq_along(cells)/26))

  list_index = 1
  index1 = 1

  if(is.na(coverslip_id)) {
    a <- LETTERS[sample.int(26,1)]
    b <- LETTERS[sample.int(26,1)]
    c <- LETTERS[sample.int(26,1)]

    w <- paste0(paste0(a,b),c)
  }

  else{
    w <- df$coverslip[[1]]
  }


  if(length(w[which(is.na(w))]) != 0){
    na_cov <- length(w[which(is.na(w))])
    second_letter_list <- LETTERS[seq(from = 1, to = length(na_cov))]
    third_letter_list <- LETTERS[seq(from = 1, to = length(na_cov))]
    dbl <- paste0(second_letter_list, third_letter_list)
    w[which(is.na(w))] <- dbl
  }

  for(i in cells){

    x <- letters[index1]

    index1 = index1 + 1

    index2 = 1
    for(j in i) {
      y <- letters[index2]

      index2 = index2 + 1
      index3 = 1

      for(h in j){



        z <- letters[index3]


        id <- paste0(w,x,y,z)

        id_list[[list_index]] <- id

        list_index = list_index + 1
        index3 = index3 + 1

      }

    }

  }

  return(id_list)
}



#' time_to_f
#'
#' Converts time in minutes or seconds in frame
#'
#' @param x
#' @param frame_rate
#' @param unit
#'
#' @return
#' @export
#'
#' @examples
time_to_f <- function(x, frame_rate, unit = c("seconds", "minutes")){

  if(unit == "seconds"){
    y_final <- as.integer(x*frame_rate)
  }

  if(unit == "minutes") {

    if(is.integer(x) == FALSE) {
      y <- as.integer(x)
      y_min <- y*60

      y_final <- (y_min + ((x %% 1) * 100)) * frame_rate
    }

    else{
      y_final <- (x*60) * frame_rate
    }

  }

  return(y_final)
}

#' downsampleCaData
#'
#' @param data data table output from prepareData function
#' @param original_freq Frequency of the data argument (in Hz)
#' @param target_freq Desired Frequency after downsampling (in Hz)
#'
#' @return a data table downsampled at the desired frequency
#' @export
#'
#' @examples
downsampleCaData  <- function(data, original_freq,  target_freq ){

  if(target_freq != 0) {
  dt <- copy(setDT(data))
  factor <- round(original_freq/target_freq)
  dt <- dt[time_frame %% factor == 0 | time_frame == 1]
  dt[, time_frame := seq(1,.N), by = Cell_id]
  dt[, Time_frame_stim := seq(1,.N), by = .(Cell_id,stimulus)]

  }

  if(target_freq == original_freq | target_freq == 0){
  dt <- data
  }

  return(dt)
}

