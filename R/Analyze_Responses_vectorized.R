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


  data$spike_stimulus <- str_replace_all(data$spike_stimulus, "[123456789.]", "")
  df_clean$stimulus <- str_replace_all(df_clean$stimulus, "[123456789.]", "")

  ### Adding a variable "Response" for each stimulus in df_clean

  '%notin%' <- Negate('%in%')
  mark_lev <- unique(df_clean$marker_positive)
  df_clean$marker_positive <- factor(df_clean$marker_positive, levels = mark_lev, ordered = TRUE)

  group_lev <- unique(df_clean$group)
  df_clean$group  <- factor(df_clean$group, levels = group_lev, ordered = TRUE)


  stim_lev <- unique(df_clean$stimulus)
  df_clean$stimulus  <- factor(df_clean$stimulus, levels = stim_lev, ordered = TRUE)

  cov_lev <- unique(df_clean$coverslip)
  df_clean$coverslip  <- factor(df_clean$coverslip, levels = cov_lev, ordered = TRUE)


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

      d <- unique(df_clean[,c(c("Cell_id","stimulus"), var_list)])

    }
  }


    setkey(d, Cell_id, stimulus)
    setkey(data, Cell_id, spike_stimulus)

    response_indices <- unique(d[data, which = TRUE])
    print(response_indices)
    print(d[response_indices])

    d$Response <- FALSE
    d$Response[response_indices] <- TRUE


    stim_list <- unique(d$stimulus)

    n_cells_tot <- length(unique(d$Cell_id))

    if("coverslip" %in% var_list){
      n_cells_cov <- d[, .(n_cells = length(unique(Cell_id))), by = coverslip]
    }


    if("group" %in% var_list){
      n_cells_grp <- d[, .(n_cells = length(unique(Cell_id))), by = group]
      print(n_cells_grp)
    }

    if("marker_positive" %in% var_list){
      marker_list <- unique(d$marker_positive)
    }

    n_cells_cond <- d[, .(n_cells = length(unique(Cell_id))), by = var_list]

    if(simulation == TRUE) {
      print("second part of simulation")
        data <- d[, .(Responders = sum(Response)), by = stimulus]
        data <- data[, c("Prop", "n_cells_tot") := list(Responders/ n_cells_tot, n_cells_tot)]
    }

    else{

      if(is.null(var_list)){
        d <- unique(d[,c("Cell_id", "Response", "stimulus")])
        print(d)
        print("houla")
        data <- d[, .(Responders = sum(Response))]


      }
      else{
      d <- unique(d[,c(c("Cell_id", "Response"), ..var_list)])
      data <- d[, .(Responders = sum(Response)), by = var_list]

      }


  print(data)
  print("diii")

    if("coverslip" %notin% var_list & "group" %notin% var_list & "marker_positive" %notin% var_list){
      print("yyrhy")
      data <- data[, c("Prop", "n_cells_tot") := list(Responders/ n_cells_tot, n_cells_tot)]
    }

    if("coverslip" %notin% var_list & "stimulus" %notin% var_list & "group" %in% var_list & "marker_positive" %notin% var_list){
      print("yydfy")
      print(n_cells_grp)
      print(n_cells_grp$n_cells)
      data <- data[, c("Prop", "n_cells_grp") := list(Responders/ n_cells_grp$n_cells, n_cells_grp$n_cells)]
    }

    if("coverslip" %notin% var_list & "stimulus" %in% var_list & "group" %in% var_list & "marker_positive" %notin% var_list){
      print("yyyze")
      n_cells_grp <- rep(n_cells_grp$n_cells, each = length(stim_list))
      data <- data[, c("Prop", "n_cells_grp") := list(Responders/ n_cells_grp, n_cells_grp)]
    }


    if("coverslip" %notin% var_list & "stimulus" %notin% var_list & "group" %notin% var_list & "marker_positive" %in% var_list){
      print("yyz")
      n_cells_tot <- rep(n_cells_tot, each = length(marker_list))
      data <- data[, c("n_cells_tot", "Prop_tot", "n_cells_marker", "Prop_marker_resp", "Prop_marker")
                   := list(n_cells_tot, Responders/ n_cells_tot, n_cells_cond$n_cells,
                           Responders/ n_cells_cond$n_cells, n_cells_cond$n_cells/n_cells_tot )]
    }


    if("coverslip" %notin% var_list & "group" %notin% var_list & "stimulus" %in% var_list & "marker_positive" %in% var_list){
      print("y")
      #n_cells_cond <- rep(n_cells_cond$n_cells, each = length(stim_list))
      n_cells_tot <- rep(n_cells_tot, each = length(marker_list)*length(stim_list))
      data <- data[, c("n_cells_tot", "Prop_tot", "n_cells_marker", "Prop_marker_resp", "Prop_marker")
                   := list(n_cells_tot, Responders/ n_cells_tot, n_cells_cond$n_cells,
                           Responders/ n_cells_cond$n_cells, n_cells_cond$n_cells/n_cells_tot )]
      }


    if("coverslip" %notin% var_list & "group" %in% var_list & "stimulus" %in% var_list & "marker_positive" %in% var_list){
      data$marker_positive <- factor(data$marker_positive, levels = marker_list, ordered = TRUE)

      data <- data[order(marker_positive)]
      print(n_cells_cond)
      n_cells_cond <- n_cells_cond[order(marker_positive)]
      print(data)
      print(n_cells_cond)

      print("dddattta")
      #n_cells_cond <- rep(n_cells_cond$n_cells, each = length(stim_list))
      print(n_cells_cond)
      print("n_cells_cond")

      group_list <- unique(d$group)
      n_cells_grp <- rep(rep(n_cells_grp$n_cells, each = length(stim_list)), times = length(marker_list))
      print(n_cells_grp)
      print("ncellsgrp")
      data <- data[, c("n_cells_grp", "Prop_tot", "n_cells_marker", "Prop_marker_resp", "Prop_marker")
                   := list(n_cells_grp, Responders/ n_cells_grp, n_cells_cond$n_cells,
                           Responders/ n_cells_cond$n_cells, n_cells_cond$n_cells/n_cells_grp )]
    }

    if("coverslip" %notin% var_list & "group" %in% var_list & "stimulus" %notin% var_list & "marker_positive" %in% var_list){

      #data$marker_positive <- factor(data$marker_positive, levels = marker_list, ordered = TRUE)

      data <- data[order(marker_positive)]
      n_cells_cond <- n_cells_cond[order(marker_positive)]


      group_list <- unique(d$group)
      n_cells_tot <- rep(n_cells_tot, each = length(marker_list)*length(group_list))
      n_cells_grp <- rep(n_cells_grp$n_cells, times = length(marker_list))
      data <- data[, c("n_cells_grp", "Prop_tot", "n_cells_marker", "Prop_marker_resp", "Prop_marker")
                   := list(n_cells_grp, Responders/ n_cells_grp, n_cells_cond$n_cells,
                           Responders/ n_cells_cond$n_cells, n_cells_cond$n_cells/n_cells_grp )]
    }



    if("coverslip" %in% var_list & "stimulus" %notin% var_list & "group" %notin% var_list & "marker_positive" %notin% var_list){
      print("yyyddd")
       data <- data[, c("Prop", "n_cells_cov") := list(data$Responders/ n_cells_cov$n_cells, n_cells_cov$n_cells)]
    }



    if("coverslip" %in% var_list & "stimulus" %in% var_list & "marker_positive" %notin% var_list & "group" %notin% var_list){
      print("yyzzzy")
      n_cells_cov <- rep(n_cells_cov$n_cells, each = length(stim_list))
      data <- data[, c("Prop", "n_cells_cov") := list(Responders/ n_cells_cov, n_cells_cov)]
    }

    if("coverslip" %in% var_list & "stimulus" %in% var_list & "marker_positive" %in% var_list & "group" %notin% var_list){
      print("yyysdg")

      #n_cells_cov_marker <- d[, .(n_cells = length(unique(Cell_id))), by = list(coverslip, marker_positive)]
      data <- data[order(marker_positive)]
      n_cells_cond <- n_cells_cond[order(marker_positive)]

      #n_cells_cond <- rep(n_cells_cond$n_cells, each = length(stim_list))
      n_cells_cov <- rep(rep(n_cells_cov$n_cells, each = length(stim_list)), times = length(marker_list))

      data <- data[, c("Prop_cov_marker", "n_cells_cov_marker", "Prop_cov", "n_cells_cov", "Prop_pos_cells_cov") := list(Responders/ n_cells_cond$n_cells, n_cells_cond$n_cells, Responders/ n_cells_cov, n_cells_cov, n_cells_cond$n_cells/n_cells_cov)]
    }

    if("coverslip" %in% var_list & "stimulus" %notin% var_list & "marker_positive" %in% var_list & "group" %notin% var_list){
      print("yyysdg")

      print(n_cells_cond)
      n_cells_cond <- d[, .(n_cells = length(unique(Cell_id))), by = list(coverslip, marker_positive)]
      data <- data[order(marker_positive)]
      n_cells_cov <- rep(n_cells_cov$n_cells, times = length(marker_list))

      #n_cells_cov_marker <- rep(n_cells_cov_marker$n_cells, times = length(marker_list))

      data <- data[, c("Prop_cov_marker", "n_cells_cov_marker", "Prop_cov", "n_cells_cov", "Prop_pos_cells_cov") := list(Responders/ n_cells_cond$n_cells, n_cells_cond$n_cells, Responders/ n_cells_cov , n_cells_cov, n_cells_cond/n_cells_cov)]
    }


    if("coverslip" %in% var_list & "stimulus" %notin% var_list & "marker_positive" %in% var_list & "group" %in% var_list){
      print("yyysdg")

      #n_cells_grp_cov_marker <- d[, .(n_cells = length(unique(Cell_id))), by = list(group, coverslip, marker_positive)]
      #print(n_cells_grp_cov_marker)
      print(data)
      print("datt")
      data <- data[order(marker_positive)]
      n_cells_cond <- n_cells_cond[order(marker_positive)]
      print(n_cells_cond)

      n_cells_cov <- rep(n_cells_cov$n_cells, times = length(marker_list))

      print(n_cells_cov)
      data <- data[, c("Prop", "n_cells_cov_marker", "Prop_cov", "n_cell_cov") := list(Responders/ n_cells_cond$n_cells, n_cells_cond$n_cells, Responders/n_cells_cov, n_cells_cov)]

      n_cells_cond <- n_cells_cond[order(group, coverslip)]
      print(n_cells_cond)

      n_cov_group <-  d[, .(n_cov = length(unique(coverslip))), by = group]
  print(n_cov_group)


  data <- data[order(coverslip)]
  print(data)
  #n_cells_grp_cov_marker <- n_cells_grp_cov_marker[order(marker_positive)]

      n_cells_grp <- unlist(lapply(seq(length(n_cov_group$n_cov)), function(x) rep(n_cells_grp$n_cells[[x]], each = n_cov_group$n_cov[[x]]*length(marker_list))))
      print(n_cells_grp)

      data <- data[, c("n_cells_grp", "Prop_marker_resp_grp", "Prop_marker_cov_grp", "Prop_marker_cov") := list(n_cells_grp, Responders / n_cells_grp, n_cells_cond$n_cells/n_cells_grp, n_cells_cond$n_cells/n_cell_cov )]

    }



  if("coverslip" %in% var_list & "stimulus" %in% var_list & "marker_positive" %in% var_list & "group" %in% var_list){
    print("yyysdg")

    #n_cells_grp_cov_marker <- d[, .(n_cells = length(unique(Cell_id))), by = list(group, coverslip, marker_positive)]
    #print(n_cells_grp_cov_marker)
    print(data)
    print("datt")
    data <- data[order(marker_positive)]
    n_cells_cond <- n_cells_cond[order(marker_positive)]
    print(n_cells_cond)
    print(n_cells_cov)
    print("n_cells_cov")
    n_cells_cov <- rep(rep(n_cells_cov$n_cells, each = length(stim_list), times = length(marker_list)))

    print(n_cells_cov)
    data <- data[, c("Prop", "n_cells_cov_marker", "Prop_cov", "n_cell_cov") := list(Responders/ n_cells_cond$n_cells, n_cells_cond$n_cells, Responders/n_cells_cov, n_cells_cov)]

    n_cells_cond <- n_cells_cond[order(group, coverslip)]
    print(n_cells_cond)

    n_cov_group <-  d[, .(n_cov = length(unique(coverslip))), by = group]
    print(n_cov_group)


    data <- data[order(coverslip)]
    print(data)
    #n_cells_grp_cov_marker <- n_cells_grp_cov_marker[order(marker_positive)]

    n_cells_grp <- rep(unlist(lapply(seq(length(n_cov_group$n_cov)), function(x) rep(n_cells_grp$n_cells[[x]], each = n_cov_group$n_cov[[x]]*length(marker_list)))), each = length(stim_list))
    print(n_cells_grp)

    data <- data[, c("n_cells_grp", "Prop_marker_resp_grp", "Prop_marker_cov_grp", "Prop_marker_cov") := list(n_cells_grp, Responders / n_cells_grp, n_cells_cond$n_cells/n_cells_grp, n_cells_cond$n_cells/n_cell_cov )]

  }




  if("coverslip" %in% var_list & "stimulus" %in% var_list & "marker_positive" %notin% var_list & "group" %in% var_list){
    print("yyysdg")


    #data <- data[order(marker_positive)]
    n_cells_cond <- n_cells_cond[order(group)]

    n_cells_cov <- rep(n_cells_cov$n_cells, each = length(stim_list))

    print(n_cells_cov)
    data <- data[, c("Prop", "n_cells_cov_marker", "Prop_cov", "n_cell_cov") := list(Responders/ n_cells_cond$n_cells, n_cells_cond$n_cells, Responders/n_cells_cov, n_cells_cov)]

    n_cells_cond <- n_cells_cond[order(group, coverslip)]
    print(n_cells_cond)

    n_cov_group <-  d[, .(n_cov = length(unique(coverslip))), by = group]
    print(n_cov_group)


    data <- data[order(coverslip)]
    print(data)
    #n_cells_grp_cov_marker <- n_cells_grp_cov_marker[order(marker_positive)]

    n_cells_grp <- rep(unlist(lapply(seq(length(n_cov_group$n_cov)), function(x) rep(n_cells_grp$n_cells[[x]], each = n_cov_group$n_cov[[x]]))), each = length(stim_list))
    print(n_cells_grp)

    data <- data[, c("n_cells_grp", "Prop_resp_grp", "Prop_cov_grp", "Prop_cov") := list(n_cells_grp, Responders / n_cells_grp, n_cells_cond$n_cells/n_cells_grp, n_cells_cond$n_cells/n_cell_cov )]

  }


}

    if(one_cell == FALSE & compare_groups == FALSE) {
      res <- Compare_props(d)
    }



    if(one_cell == TRUE){
      res = NULL
    }


    if(one_cell == FALSE & compare_groups == TRUE){
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
