
#' prepareData
#'
#' @param folder_name
#' @param stim_number
#' @param frame_rate
#' @param duration_in_seconds
#' @param compare_groups
#'
#' @return
#' @export



#' @examples
prepareData <- function(folder_name, stim_number, frame_rate,  duration_in_seconds = 30, compare_groups = FALSE, marker_thresh
) {


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
    print("yes")
    marker <- list.files(folder_name, pattern = "marker", recursive = T, full.names = T)

    # Reading the files
    marker_list <- lapply(marker, function(x) data.table::fread(x, skip = 1, header = FALSE))
    marker_list <- lapply(marker_list, function(x) x[,2:length(x)])
    marker_list <- lapply(marker_list, function(x) data.table::setnames(x, paste0(rep("Mean", length(x)), seq(1: length(x)))))
    print(marker_list)
  }

  else{
    marker_list <- NULL
  }



  # Code pour récupérer uniquement le numéro du coverslip et lui ajouter une lettre :


  index_cov <-length(stringr::str_split(myFiles[[1]], "/")[[1]])

  coverslip_id <- lapply(myFiles, function(x) as.integer(stringr::str_replace_all(stringr::str_split(x, "/")[[1]][index_cov], "[.csv.]", "")))



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

  # Get the pattern to find in the colnames for the cell_srt function :

  pattern <- substr(colnames(df_list[[1]])[2], 1,4)

  ### Code pour récupérer uniquement le groupe auquel appartient un coverslip

  index_gr <-length(stringr::str_split(myFiles[[1]], "/")[[1]])-2

  group_list <- lapply(myFiles, function(x) stringr::str_split(x, "/")[[1]][index_gr])




  if(compare_groups == TRUE) {

    for(i in 1:length(df_list)){

      # voir pour executer tidy_df sur chaque élément de df_list via un pmap ? :

      df_list[[i]] <- tidy_df(df_list[[i]],stimuli[[i]], each[[i]], pattern,
                              duration_in_seconds, frame_rate, coverslip_id = coverslip_id[[i]], id = i,
                              multiple = TRUE, compare_groups = TRUE, group_list[[i]], marker_list[[i]], marker_thresh)

    }
  }




  if(compare_groups == FALSE) {

    for(i in 1:length(df_list)){

      df_list[[i]] <- tidy_df(df_list[[i]],stimuli[[i]], each[[i]], pattern, duration_in_seconds,
                      frame_rate, coverslip_id = i, id = i, multiple = TRUE, compare_groups = FALSE,
                      group_list[[i]], marker_list[[i]], marker_thresh)

    }
  }


  df <- do.call(rbind, df_list)

  print(df)
  #write.csv(df, "df_manip_maxime.csv")

  return(df)
}



