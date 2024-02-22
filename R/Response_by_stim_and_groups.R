
#' Response_by_stim_and_groups
#'
#' @param data the first output from find_rise() (a data frame with one line summarizing the informations of one response)
#' @param df_clean the output from clean_data() (the full but cleaned data frame)
#'
#' @return
#' @export
#'
#' @examples
Response_by_stim_and_groups <- function(data, df_clean){

  data$Start_peak_stimulus <- str_replace_all(data$Start_peak_stimulus, "[12345.]", "")
  df_clean$stimulus <- str_replace_all(df_clean$stimulus, "[12345.]", "")


  # choper les cellules qui ont plsieurs pics par stimulus :

  tab1 <- data.frame(table(data$Cell_id, data$True_peak, data$Start_peak_stimulus))

  cell_list <- filter(tab1, Freq > 1)$Var1

  # récupérer les indices d'où sont ces doublons
  ind <- which(data$Cell_id %in% cell_list)

  # Après avoir regardé ces cellules, elles ont un profil parfois atypique
  # on peut décider de les enlever totalement :
  '%notin%' <- Negate('%in%')
  data <- data[data$Cell_id %notin% cell_list,]

  # A partir de là, il suffit de prendre les stats faites sur les pics, par groupe et par stimulus et
  # ça correspond au nombre de cellules qui répondent par stimulus et par groupe et par coverslip
  # pour le rentrer en covarié

  # récupérer l'info du coverslip :

  data <- mutate(data, coverslip =
                   unlist(map(data$Cell_id, function(x)
                     unique(df_clean[Cell_id == x,]$coverslip))))

  tab2 <- data.frame(table(data$Start_peak_stimulus, data$group, data$coverslip))
  # ENsuite créer une colonne correspondant au nombre de cellules non répondeuses et la proportion de répondeuse

  group_list <- names(table(data$group))
  stim_list <- names(table(data$Start_peak_stimulus))
  coverslip_list <- names(table(data$coverslip))


  # Récupérer le nombre de cellule par groupe :
  n_cells <- map(group_list, function(x) rep(dim(table(filter(df_clean, group == x)$Cell_id))[1], each = length(stim_list)* length(coverslip_list)))


  tab <- mutate(tab2, Not_Responding = unlist(map2(unlist(n_cells), tab2$Freq, function(x,y) x - y)),
                Percent_Response = unlist(map2(tab2$Freq, unlist(n_cells), function(x,y) (x / y)*100)))

  tab_final <- rename(tab, "Stimulus" = Var1 ,"Group"= Var2, "Coverslip" = Var3, "Responding" = Freq )

  tab_list <- split(tab_final, tab_final$Coverslip)
  tab_list <- lapply(tab_list, function(x) x[1:length(stim_list),])

  tab_bis <- do.call(rbind, tab_list)
  tab_bis$Group <- rep(group_list, each = length(stim_list))

  #tab_bis <- filter(tab_final, Responding != 0)

  write.csv(tab_bis, "tab_stim_by_group.csv")

  return(list("TRUE", "Response_by_group_and_stim" = tab_bis))
}
