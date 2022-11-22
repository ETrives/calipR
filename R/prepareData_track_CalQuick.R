
#' prepareData_track
#'
#' @param folder_name
#' @param stim_number
#' @param frame_rate
#' @param duration_in_seconds
#' @param compare_groups
#'
#' @return
#' @export
#'
#' @examples
prepareData_track <- function(folder_name, stim_number, frame_rate,  duration_in_seconds = 30, compare_groups = FALSE
) {


  # Get the file names and store the content in a list of df :
  myFiles <- list.files(folder_name, pattern = "\\.csv", recursive = T, full.names = T)

  # Removing the meta_data file :

  myFiles <- myFiles[!stringr::str_detect(myFiles,pattern="meta")]

  meta <- list.files(folder_name, pattern = "meta", recursive = T, full.names = T)

  df_list <- vector(mode = "list", length = length(myFiles))

  # Reading all the files

  df_list <- lapply(myFiles, function(x) data.table::fread(x))


  # Code pour récupérer uniquement le numéro du coverslip et lui ajouter une lettre :

  coverslip_id <- lapply(myFiles, function(x) as.integer(stringr::str_replace_all(stringr::str_split(x, "/")[[1]][4], "[.csv.]", "")))

  letter_list <- LETTERS[seq(from = 1, to = length(myFiles))]

  coverslip_id <- purrr::map2(letter_list, coverslip_id, function(x,y) paste(x,y,sep =""))



  # Fetching the stimuli informations :

  meta_df <- data.table::fread(meta)
  stimuli <- meta_df$stimuli
  stimuli <- split(stimuli, ceiling(seq_along(stimuli)/stim_number))


  stimuli <- lapply(stimuli, function(x) purrr::map2(seq_along(1:stim_number), x, function(y, z) paste(y,z, sep=".")))


  # now the time informations :

  each <- meta_df$timing
  each <- split(each, ceiling(seq_along(each)/stim_number))

  ### Code pour récupérer uniquement le groupe auquel appartient un coverslip


  group_list <- lapply(myFiles, function(x) stringr::str_split(x, "/")[[1]][2])

  if(compare_groups == TRUE) {

    for(i in 1:length(df_list)){

      # voir pour executer tidy_df sur chaque élément de df_list via un pmap ? :

      df_list[[i]] <- tidy_df(df_list[[i]],stimuli[[i]], each[[i]], frame_rate,
                              coverslip_id = coverslip_id[[i]], id = i,
                              multiple = TRUE, compare_groups = TRUE, group_list[[i]])

    }
  }




  if(compare_groups == FALSE) {

    for(i in 1:length(df_list)){

      df_list[[i]] <- trackmateInput(df_list[[i]],stimuli[[i]], each[[i]],
                      frame_rate, coverslip_id = i, id = i, group_list[[i]],
                      duration_in_seconds)

    }
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
trackmateInput <- function(file, stimuli, each, frame_rate, coverslip_id, id, group, duration_in_seconds){

  track <- file[-c(1,2,3),]
  track_final <- data.table::setDT(track)[, .(Cell_id =TRACK_ID, Mean_Grey = as.numeric(MEAN_INTENSITY_CH1))]
  Ids <- unlist(createId(track_final, id))

  cell_split <- split(track_final, track_final$Cell_id)
  cell_split <- purrr::map2(cell_split, Ids, function(x,y) x[, Cell_id := rep(y, each = dim(x$Mean_Grey)[1])])

  #retrieving the total number of frames if the cell was detected from the beginning
  #until the end

  dim <- max(unlist(lapply(cell_split, function(x) length(x$Cell_id))))

  cell_split <- lapply(cell_split, function(x) if(length(x$Cell_id) == dim){x})

  data <- do.call(rbind, cell_split)

  n_cells <- length(unique(data$Cell_id))

  data$time_frame <- as.numeric(rep(seq(from = 1, to = dim),times = n_cells))


  # matching stimuli :


  time <- lapply(each, function(x) as.numeric(x))

  # Converting minutes to frames
  frame_list <- lapply(time, min_to_f, frame_rate)


  frame_list <- append(frame_list, dim)


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

  #data <- split(fin, fin$Cell_id)

  #fin <- lapply(fin, function(x) x[, stimulus := unlist(stimuli_full)])

  #data <- do.call(rbind, data)


  # Adding a variable stimulus :

  data$stimulus <- rep(unlist(stimuli_full), times = n_cells)

  # Adding a variable to track the coverslip Id :
  coverslip_full <- rep(coverslip_id, dim)

  data$coverslip <- unlist(rep(coverslip_full,n_cells))

  # Adding a variable with the actual time in seconds

  data$time_seconds <- rep(seq(from = 1/ frame_rate, to = dim/ frame_rate, by = 1/frame_rate ), times = n_cells)

  duration <- duration_in_seconds * frame_rate

  # Adding a variable tracking the frame with each stimulus being the zero reference :

  data <- data[, Time_frame_stim := seq(c(1:length(stimulus)))]
  data <- data[, Stimulation := Time_frame_stim <= duration]


return(data)
}

