
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

print(df_list)
  # Checking if marker files have been added

  if(len_before - len_after > 1){
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

print(myFiles[[1]])

  index_cov <-length(stringr::str_split(myFiles[[1]], "/")[[1]])

  coverslip_id <- lapply(myFiles, function(x) as.integer(stringr::str_replace_all(stringr::str_split(x, "/")[[1]][index_cov], "[.csv.]", "")))

  letter_list <- LETTERS[seq(from = 1, to = ceiling(length(myFiles)/9))]
  cov_num <- rep(seq(from = 1, to = 9), times = length(letter_list))

  letter_list <- rep(letter_list, each = 9)






  #letter_list <- LETTERS[seq(from = 1, to = length(myFiles))]



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

  stimuli <- split(meta_df, meta_df$coverslip)
  stimuli <- lapply(stimuli, function(x) x$stimuli)

  print(stimuli)
  stim_number <- length(stimuli[[1]])
  print("stim_number")
  print(stim_number)

  stimuli <- split(stimuli, ceiling(seq_along(stimuli)/stim_number))

  #stimuli <- paste(seq(1,stim_number), stimuli, sep = ".")

  stimuli <- lapply(stimuli, function(x) lapply(x, function(y)
    unlist(purrr::map2(seq_along(1:stim_number), y, function(z, a) paste(z,a, sep=".")), recursive = FALSE)))


  # now the time informations :

  each <- meta_df$timing
  each <- split(each, ceiling(seq_along(each)/stim_number))

  # Get the pattern to find in the colnames for the cell_srt function :

  print("df_list[[1]]")
  print(df_list[[1]])
  print(colnames(df_list[[1]]))
  #pattern <- substr(colnames(df_list[[1]])[2], 1,4)
  pattern <- substr(colnames(df_list[[1]])[[1]], 1,4)

  print("pattern")

  print(pattern)
  ### Code pour récupérer uniquement le groupe auquel appartient un coverslip

  index_gr <-length(stringr::str_split(myFiles[[1]], "/")[[1]])-2

  group_list <- lapply(myFiles, function(x) stringr::str_split(x, "/")[[1]][index_gr])


print("stimuli")
print(stimuli)

print("each")
print(each)

print("coverslip_id")
print(coverslip_id)

print("group_list")
print(group_list)

print("marker_list")
print(marker_list)

  if(compare_groups == TRUE) {

    for(i in 1:length(df_list)){

      # voir pour executer tidy_df sur chaque élément de df_list via un pmap ? :

      df_list[[i]] <- tidy_df(df_list[[i]],stimuli[[1]][[i]], each[[i]], pattern,
                              duration_in_seconds, frame_rate, coverslip_id = coverslip_id[[i]], id = i,
                              multiple = TRUE, compare_groups = TRUE, group_list[[i]], marker_list[[i]], marker_thresh)

    }
  }




  if(compare_groups == FALSE) {

    for(i in 1:length(df_list)){

      df_list[[i]] <- tidy_df(df_list[[i]],stimuli[[1]][[i]], each[[i]], pattern, duration_in_seconds,
                      frame_rate, coverslip_id = i, id = i, multiple = TRUE, compare_groups = FALSE,
                      group_list[[i]], marker_list[[i]], marker_thresh)

    }
  }

print("yoooooooooo")
  df <- do.call(rbind, df_list)

  df$marker_positive <- as.factor(df$marker_positive)
  df$group <- as.factor(df$group)
  df$stimulus <- as.factor(df$stimulus)
  df$coverslip <- as.factor(df$coverslip)


  print(df)
  #write.csv(df, "df_manip_maxime.csv")

  print("prepare_data finished" )
  return(df)
}



