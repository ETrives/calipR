#' Analyze_Responses
#'
#' @param data
#' @param df_clean
#' @param compare_groups
#'
#' @return
#' @export
#'
#' @examples
Analyze_Responses <- function(data, df_clean, compare_groups = FALSE, one_cell = FALSE, marker = FALSE, var_list = NULL, simulation = FALSE){

  ### Adding a variable "Response" for each stimulus in df_clean

  '%notin%' <- Negate('%in%')


  if(simulation == TRUE){
    d <- unique(df_clean[,c("Cell_id", "stimulus")])
  }

  else if(simulation == FALSE){

    if(is.null(var_list) & compare_groups == FALSE) {
    d <- unique(df_clean[,c("Cell_id", "stimulus")])
    }


    else if(is.null(var_list) & compare_groups == TRUE){

      d <- unique(df_clean[,c("Cell_id", "stimulus", "group")])
    }
    else{

      d <- unique(df_clean[,c("Cell_id","stimulus", var_list)])
    }
  }

    d_list <- split(d,cumsum(1:nrow(d) %in% seq(1:nrow(d))))
    d_list <- lapply(d_list, function(x) data.table::setDT(x)[, Response := ifelse(is.na(
      data[data$Cell_id == x$Cell_id &
             data$spike_stimulus == x$stimulus,]$Cell_id[1]),
      FALSE, TRUE) ])

    d <- do.call(rbind, d_list)


    stim_list <- unique(d$stimulus)
    n_cells_tot <- length(unique(d$Cell_id))

    if("marker_positive" %in% var_list){
    marker_list <- unique(d$marker_positive)
    n_cells_marker <- d[, .(n_cells = length(unique(Cell_id))), by = marker_positive]
    }

    if("coverslip" %in% var_list){
      n_cells_cov <- d[, .(n_cells = length(unique(Cell_id))), by = coverslip]
    }

    if("group" %in% var_list){
      n_cells_grp <- d[, .(n_cells = length(unique(Cell_id))), by = group]
      print(n_cells_grp)
    }


    if(simulation == TRUE) {
      print("second part of simulation")
        data <- d[, .(Responders = sum(Response)), by = stimulus]
        data <- data[, c("Prop", "n_cells_tot") := list(Responders/ n_cells_tot, n_cells_tot)]
    }

    else{
    data <- d[, .(Responders = sum(Response)), by = var_list]

    if("coverslip" %notin% var_list & "group" %notin% var_list & "marker_positive" %notin% var_list){
      data <- data[, c("Prop", "n_cells_tot") := list(Responders/ n_cells_tot, n_cells_tot)]
    }

    if("coverslip" %notin% var_list & "stimulus" %notin% var_list & "group" %in% var_list & "marker_positive" %notin% var_list){
      print(n_cells_grp)
      print(n_cells_grp$n_cells)
      data <- data[, c("Prop", "n_cells_grp") := list(Responders/ n_cells_grp$n_cells, n_cells_grp$n_cells)]
    }

    if("coverslip" %notin% var_list & "stimulus" %in% var_list & "group" %in% var_list & "marker_positive" %notin% var_list){
      n_cells_grp <- rep(n_cells_grp$n_cells, each = length(stim_list))
      data <- data[, c("Prop", "n_cells_grp") := list(Responders/ n_cells_grp, n_cells_grp)]
    }


    if("coverslip" %notin% var_list & "stimulus" %notin% var_list & "group" %notin% var_list & "marker_positive" %in% var_list){
      n_cells_tot <- rep(n_cells_tot, each = length(marker_list))
      data <- data[, c("n_cells_tot", "Prop_tot", "n_cells_marker", "Prop_marker_resp", "Prop_marker")
                   := list(n_cells_tot, Responders/ n_cells_tot, n_cells_marker$n_cells,
                           Responders/ n_cells_marker$n_cells, n_cells_marker$n_cells/n_cells_tot )]
    }


    if("coverslip" %notin% var_list & "group" %notin% var_list & "stimulus" %in% var_list & "marker_positive" %in% var_list){
      n_cells_marker <- rep(n_cells_marker$n_cells, each = length(stim_list))
      n_cells_tot <- rep(n_cells_tot, each = length(marker_list)*length(stim_list))
      data <- data[, c("n_cells_tot", "Prop_tot", "n_cells_marker", "Prop_marker_resp", "Prop_marker")
                   := list(n_cells_tot, Responders/ n_cells_tot, n_cells_marker,
                           Responders/ n_cells_marker, n_cells_marker/n_cells_tot )]
      }


    if("coverslip" %notin% var_list & "group" %in% var_list & "stimulus" %in% var_list & "marker_positive" %in% var_list){
      group_list <- unique(d$group)
      n_cells_marker <- rep(n_cells_marker$n_cells, each = length(stim_list)*length(group_list))
      n_cells_tot <- rep(n_cells_tot, each = length(marker_list)*length(stim_list))
      n_cells_grp <- rep(n_cells_grp$n_cells, each = length(stim_list)*length(marker_list))
      data <- data[, c("n_cells_tot", "Prop_tot", "n_cells_marker", "Prop_marker_resp", "Prop_marker")
                   := list(n_cells_tot, Responders/ n_cells_tot, n_cells_marker,
                           Responders/ n_cells_marker, n_cells_marker/n_cells_tot )]
    }

    if("coverslip" %notin% var_list & "group" %in% var_list & "stimulus" %notin% var_list & "marker_positive" %in% var_list){
      group_list <- unique(d$group)
      n_cells_marker <- rep(n_cells_marker$n_cells, times = length(group_list))
      n_cells_tot <- rep(n_cells_tot, each = length(marker_list)*length(group_list))
      n_cells_grp <- rep(n_cells_grp$n_cells, each = length(marker_list))
      data <- data[, c("n_cells_grp", "Prop_tot", "n_cells_marker", "Prop_marker_resp", "Prop_marker")
                   := list(n_cells_grp, Responders/ n_cells_grp, n_cells_marker,
                           Responders/ n_cells_marker, n_cells_marker/n_cells_grp )]
    }



    if("coverslip" %in% var_list & "stimulus" %notin% var_list){
       data <- data[, c("Prop", "n_cells_cov") := list(data$Responders/ n_cells_cov$n_cells, n_cells_cov$n_cells)]
    }



    if("coverslip" %in% var_list & "stimulus" %in% var_list){
      n_cells_cov <- rep(n_cells_cov$n_cells, each = length(stim_list))
      data <- data[, c("Prop", "n_cells_cov") := list(Responders/ n_cells_cov, n_cells_cov)]
    }

    if("coverslip" %in% var_list & "stimulus" %in% var_list & "marker_positive" %in% var_list){
      n_cells_cov <- rep(n_cells_cov$n_cells, each = length(stim_list)* length(marker_list))
      n_cells_marker <- rep(n_cells_marker$n_cells, each = length(stim_list))
      data <- data[, c("Prop", "n_cells_cov") := list(Responders/ n_cells_cov, n_cells_cov)]
    }



}

    if(one_cell == FALSE){
      res <- Compare_props(d)
    }


    if(one_cell == TRUE){
      res = NULL
    }


    if(compare_groups == TRUE){
      res <- "NO STATS"
    }
    #res <- glmer(Response ~ group * stimulus + (1|Cell_id), family = binomial, data = d)


  print("Analyze Responses OK")
  return(list(data, res))
}


#' Compare_props
#'
#' @param data the full output from Count_responders_stim()
#'
#' @return a list with 2 elements. The first is the general summary of the cochran's q test
#' the second is the results of the pairwise comparisons between stimuli with McNemar test with correction for multiple comparisons
#' @export
#'
#' @examples
Compare_props <- function(data){

  res_tot <- rstatix::cochran_qtest(data, Response~stimulus|Cell_id)
  res_post_hoc <- rstatix::pairwise_mcnemar_test(data, Response~stimulus|Cell_id)

  return(list(res_tot, res_post_hoc))
}
