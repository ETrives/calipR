

#' eventExtractR
#'
#'This function extracts all the behavioral events onsets
#'
#' @param data A data table containing aligned behavioral and fiber photometry data
#'             (e.g. the output of alignEpocs())
#'
#' @return
#' @export
#'
#' @examples
eventExtractR <- function(data){
#extract behaviors

end <- which(colnames(data) == "ID") -1
events <- colnames(data)[2:end]

print("events")
print(events)

events_bis <- lapply(events, function(x) paste0("status_", x))

onset_list <- lapply(which(colnames(data) %in% events), function(y) data[, paste0("status_", colnames(data)[y]) := rleid(get(colnames(data)[y]))])

print(onset_list)

onset_list <- lapply(events_bis, function(y)
  data[, start_behavior := NA])

print(events_bis)
onset_list <- lapply(events_bis, function(y)
              data[, start_behavior := ifelse(get(y) %% 2 == 0 & !duplicated(get(y)), y, start_behavior)])


print("new_onset_list_bis")
print(onset_list)

return(onset_list)
}




#' PeriEventExtractR
#'
#' @param data A data table with behavioral annotations and photometry data
#'             aligned together, as returned by the alignEpocs function.
#' @param before A numeric indicating how many seconds before stimulus onset should be kept.
#' @param after A numeric indicating how many seconds after stimulus onset should be kept.
#'
#' @return A list of data tables with two levels. The first level is the same length
#'         as the number of different behavioral events in the dataset. For each event, a list
#'         of data table is stored. Each data table contains photometry and behavioral informations
#'         10 seconds before the onset of the event until 10 seconds after the onset.
#' @export
#'
#' @examples
PeriEventExtractR <- function(data,events_list, before, after){

  #extract behaviors

 # end <- which(colnames(data) == "onset") -1
 # events <- colnames(data)[2:end]

  print("data inside peri events")
  print(data)
  
  onset_list <- lapply(seq(1,length(events_list)), function(x) unique(data[ start_behavior == events_list[x] | start_behavior ==  "1", , by = group]$TIME_SECONDS ))

  print("yo")
  dif <- lapply(onset_list, diff)

  print("yi")
  print(onset_list)
  # Keep starting events
  #onset_list <- lapply(seq(1,length(dif)), function(x) onset_list[[x]][which(dif[[x]] > 1)])



  #onset_list <- Filter(function(x) !(length(x) == 0), onset_list)

  n_obs <- unlist(lapply(onset_list, length))

  print(n_obs)
  idx <- lapply(seq(1,length(n_obs)), function(x) if(x == 1) seq(x:n_obs[x])
                else seq(sum(n_obs[1:x -1]+1), (n_obs[x -1] + n_obs[x])))


  print(idx)

  onset_list <- lapply(onset_list, function(x) data.table(before_onset = x -before, offset = x + after))
  onset_list <- lapply(onset_list, function(x) unname(split(x, seq(1,nrow(x)))))
  onset_list <- unlist(lapply(onset_list, function(x) lapply(x, function(y) unname(y))),recursive = FALSE)

print("onset_list final")
print(onset_list)

  # extracting slices from the original data table
  periEventsData <- Map(function(x) data[TIME_SECONDS %between% x,][, RELATIVE_TIME := seq(0,.N -1)], onset_list)

  #print(periEventsData)
  # putting into separate lists, events of different nature
  periEventsData <- lapply(idx, function(x) periEventsData[x[[1]]:x[[length(x)]]])

  names(periEventsData) <- events_list[1:length(idx)]
  print("before")
  print(head(periEventsData))

  
  periEventsData <- lapply(periEventsData, function(x) lapply(x, function(y) data.table::setDT(y)[, TIME_FROM_EVENT := seq(-before,after, by = 0.1)[1:.N]]))
  print("after")
  print(head(periEventsData))
  return(periEventsData)
}


#' PeriEventMergeR
#'
#' @param periEventsData
#'
#' @return
#' @export
#'
#' @examples
PeriEventMergeR <- function(periEventsData){

  events <- unique(names(periEventsData))

  repetition_number <- lapply(events, function(x) length(periEventsData[[x]]))

  data <-  vector(mode = "list", length = length(events))
  for(i in seq(1,length(events))){

    for(j in seq(1,repetition_number[[i]])){
      print(i)
      print(j)
      val <- j
      df <- copy(periEventsData[[i]][[j]][, replicate := val])
      data[[i]][[j]] <- df
    }

  }

  merged_perievents <- lapply(data, function(x) do.call(rbind, x))

  names(merged_perievents) <- events

  lapply(seq(1,length(events)), function(x) merged_perievents[[x]][, EVENT := events[[x]]])

  final_data <- do.call(rbind, merged_perievents)

  final_data <- final_data[, .(mean_ca = mean(CA_TRACE, na.rm = TRUE),
                               sd_ca = sd(CA_TRACE, na.rm = TRUE),
                               ca = CA_TRACE,
                               isos = ISOS_TRACE,
                               replicate = replicate,
                               mean_isos = mean(ISOS_TRACE)), by = .(EVENT, RELATIVE_TIME)]

  final_data[, error_min := mean_ca - (sd_ca/sqrt(replicate)), by = EVENT ][, error_max := mean_ca + (sd_ca/sqrt(replicate)), by = EVENT]

  return(final_data)
}

