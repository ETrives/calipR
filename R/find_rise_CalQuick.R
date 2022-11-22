
#' find_rise
#'
#' @param data
#' @param range
#'
#' @return
#' @export
#'
#' @examples
find_rise <- function(data, range){

  '%!in%' <- Negate('%in%')
  ### Finding if the last zero crossing happened during the same stimulus of the peak
  ### or before and attributing the response TRUE or FALSE to the stimulus where this
  ### zero crossing happened

  dt_1 <- data[[1]]
  dt_2 <- data[[2]]


  # Créer colonne True peak dans data[[1]

  True_peaks <- dt_1[dt_1$True_peak == TRUE]

  print(True_peaks)

  # Pour chaque pic, extraire les 50 frames qui précèdent le pic

  True_peaks$frame_50 <- unlist(lapply(True_peaks$Max_peak_frame, function(x) x - as.integer(range)))

  print(True_peaks)

  # splitter cahque ligne du dt 1  et se servir des infos de chaque item pour filtrer
  # via une boucle, dt_2, les time frame qui m'interessent, par cellule (50 time frame avant un pic)

  find_1 <- split(True_peaks,cumsum(1:nrow(True_peaks) %in% seq(1:nrow(True_peaks))))

  df_list <- vector(mode = "list", length = length(find_1))

  # En enlevant filter et en ayant initialisé df_list, c'est beaucoup plus rapide. C'est toujours long,
  # faudrait voir pour virer le loop et passer en lapply mais pour l'instant c'est correct.

  # Version encore plus rapide : j'ai écris une fonction subset_50_frames qui permet
  # d'utiliser find_1 et cette fonction dans une fonction map. Permet donc d'éviter une boucle for :
  print("Mapping True peaks to subset the original df")
  #df_list <- map(find_1, subset_50_frame, dt_2 = dt_2)
  df_list <- lapply(find_1, subset_50_frames, dt_2 = dt_2)


  print("Done")

  df_list <- lapply(df_list, function(x) x[order(-x$time_frame),])

  max_list <- lapply(df_list, function(x) x[1,]$smooth_z)
  df_list <- lapply(df_list, function(x) x[-1,])
  #max_peak <- filter(data[[2]], Max_Peak == TRUE & Cell_id == find_1[[i]]$Cell_id)

  print("Mapping df_list and max_list to find the zero crossing that has the biggest delta and which is the further")
  z <- purrr::map2(df_list, max_list, zero_crossing_index)
  print("done")


  #df_list[[i]] <- filter(df, Cell_id == zero_cross$Cell_id & time_frame == zero_cross$time_frame)
  #print("this is list")
  #print(df_list[[i]])

  start_peak <- do.call(rbind, z)


  True_peaks$Start_peak_frame <- start_peak$time_frame
  True_peaks$Start_peak_stimulus <- start_peak$stimulus
  True_peaks$Start_peak_rel_frame <- start_peak$Time_frame_stim
  True_peaks$Start_peak_stimulation <- start_peak$Stimulation


  return(list(True_peaks, start_peak))

}
