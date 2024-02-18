#' dual_prop
#'
#' @param data
#' @param stim1
#' @param stim2
#'
#' @return
#' @export
#'
#' @examples
dual_prop <- function(data, stim1, stim2){

  data$spike_stimulus <- str_replace_all(data$spike_stimulus, "[123456789.]", "")

  stim1 <- str_replace_all(stim1, "[123456789.]", "")
  stim2 <- str_replace_all(stim2, "[123456789.]", "")


  data <- data.table::setDT(data)
  data <- data[, Stim1 := spike_stimulus == stim1]
  data <- data[, Stim2 := spike_stimulus == stim2]

  print(head(data))

  n_cells_stim1 <- length(unique(data[data$Stim1 == TRUE, ]$Cell_id))
  n_cells_stim2 <- length(unique(data[data$Stim2 == TRUE, ]$Cell_id))

  print(n_cells_stim1)
  print(n_cells_stim2)

  # Enlever les numéros devant les stimuli :

  # stocker les noms des stimuli :

  stim <- unique(data$spike_stimulus)

  # Créer le tableau de contingence avec les deux stimuli
  df <- contingency(data)

  # Filter cells responding to both stimuli
  df_true <- dplyr::filter(df, Freq !=0 & Freq.1 !=0)

  # Number of cells that responds to both stimuli
  n_cells_tot <- dplyr::count(df_true)

  # Number of cells responding to the first
  n_cells_first <- dplyr::count(dplyr::filter(df, Freq != 0))

  # Number of cells responding to the second
  n_cells_second <- dplyr::count(dplyr::filter(df, Freq.1 != 0))

  # Proportion of cells responding to both
  prop1 <- n_cells_tot / length(data[["Stim1"]])

  # Proportion of cells reponding to the first that also responds to the second :
  prop2 <- n_cells_tot / n_cells_first

  # Proportion of cells reponding to the second that also responds to the first :
  prop3 <- n_cells_tot / n_cells_second

  # creating a table with all these informations :

  table_final <- data.table::data.table(n_legend = c(stim1, stim2, "Both_stimuli"), n_cells = c(n_cells_first, n_cells_second, n_cells_tot), Prop_legend =c(paste0("Both/", stim1), paste0("Both/", stim2), "Both/all"), props = c(prop2, prop3, prop1))

  return(table_final)

}



#' contingency
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
contingency <- function(data){

  t_s1 <- as.data.frame(table(data$Cell_id, data$Stim1))
  t_s1 <- dplyr::filter(t_s1, Var2 == TRUE)


  t_s2 <- as.data.frame(table(data$Cell_id, data$Stim2))
  t_s2 <- dplyr::filter(t_s2, Var2 == TRUE)


  t_final <- as.data.frame(cbind(t_s1, t_s2))

  # only keep cell_id once and freq for each stimulus
  t_final <- t_final[,c(1,3,6)]


  return(t_final)

}


#' compare_dual_prop
#'
#' @param res1
#' @param stim_pos
#' @param res2
#' @param stim_pos2
#'
#' @return
#' @export
#'
#' @examples
compare_dual_prop <- function(res1, stim_pos, res2, stim_pos2) {

  n_stim1 <- unlist(res1$n_cells[stim_pos])
  n_stim2 <- unlist(res2$n_cells[stim_pos2])

  x1 <- unlist(res1$n_cells[3])
  x2 <- unlist(res2$n_cells[3])

  res <- stats::prop.test(c(x1, x2), c(n_stim1, n_stim2))

  return(res)
}


#' cross_prop.venn
#'
#' @param data
#' @param stim1
#' @param stim2
#'
#' @return
#' @export
#'
#' @examples
cross_prop.venn <- function(data, stim_list){

  data$spike_stimulus <- str_replace_all(data$spike_stimulus, "[123456789.]", "")
  stim_list <- str_replace_all(stim_list, "[123456789.]", "")

  t <- data.table(table(data$spike_stimulus, data$Cell_id))

  data <- lapply(stim_list, function(x) unique(t[t$N != 0 & t$V1 == x,]$V2))

  names(data) <- stim_list

  p <- ggvenn::ggvenn(data)
  return(p)
}



dual_prop_travaux <- function(data, stim_list){

  data$spike_stimulus <- str_replace_all(data$spike_stimulus, "[123456789.]", "")

  stim_list <- unique(str_replace_all(stim_list, "[123456789.]", ""))

  data <- data.table::setDT(data)

  data_list <- lapply(seq(1,length(stim_list)), function(x) data[, paste0("stim",x)
                                                     := spike_stimulus == stim_list[x]] )

  data <- data_list[[length(data_list)]]

  n_resp_tot <- length(unique(data$Cell_id))
  #n_cells_list <- lapply(seq(1,length(stim_list)), function(x)
                   # length(unique(data[data[[paste0("stim",x)]] == TRUE, ]$Cell_id)))


  #n_cells_joint_resp <- lapply(seq(1,length(stim_list)), function(x)
    #length(unique(data[data[[paste0("stim",x)]] == TRUE, ]$Cell_id)))

  # Créer le tableau de contingence avec les deux stimuli
  #f <- contingency(data)

  t <- data.table(table(data$spike_stimulus, data$Cell_id))

  data <- lapply(stim_list, function(x) unique(t[t$N != 0 & t$V1 == x,]$V2))

  names(data) <- stim_list

  print(data)
  # Filter cells responding to both stimuli

  print(data[stim_list])

  n_cells_tot <- 1000
  n_resp <- c(length(data[[1]]), length(data[[2]]))

  dual_resp <- length(unlist(intersect(data[[1]], data[[2]])))

  table_final <- data.table(n_resp, n_resp_tot, dual_resp )
  rownames(table_final) <- c(stim_list[[1]], stim_list[[2]])
  colnames(table_final) <- c("N_Responders", "N_Responders_Tot", "Common_Response")

  return(table_final)

}
