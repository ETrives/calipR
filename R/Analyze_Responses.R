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
Analyze_Responses <- function(data, df_clean, compare_groups = FALSE, one_cell = FALSE){

  ### Adding a variable "Response" for each stimulus in df_clean

  if(compare_groups == FALSE){

    d <- unique(df_clean[,c("Cell_id", "stimulus", "coverslip")])
    d_list <- split(d,cumsum(1:nrow(d) %in% seq(1:nrow(d))))

    d_list <- lapply(d_list, function(x) data.table::setDT(x)[, Response := ifelse(is.na(
      data[data$Cell_id == x$Cell_id &
             data$Start_peak_stimulus == x$stimulus,]$Cell_id[1]),
      FALSE, TRUE) ])


    d <- do.call(rbind, d_list)

    stim_list <- unique(d$stimulus)

    cov_list <- unique(d$coverslip)

    n_cells <- length(unique(d$Cell_id))

    n_cells_by_cov <- lapply(cov_list, function(x) length(unique(d[d$coverslip == x]$Cell_id)))

    n_responders <- length(unique(d[d$Response == TRUE]$Cell_id))

    prop_total <- n_responders / n_cells

    stats <- data.table::setDT(list("n_cells" = n_cells, "n_responders" = n_responders, "Proportion" = prop_total))


    n_responses_by_stim <- unlist(lapply(stim_list, function(x) sum(d$stimulus == x & d$Response == TRUE)))

    resp_by_cov_and_stim <- unlist(lapply(cov_list, function(x) lapply(stim_list, function(y) sum(d$coverslip == x & d$stimulus == y & d$Response == TRUE))))

    print(cov_list)
    print(resp_by_cov_and_stim)

    prop_by_stim <- n_responses_by_stim / n_cells
    prop_by_stim_responders <- n_responses_by_stim / n_responders

    prop_by_stim_cov <- resp_by_cov_and_stim / unlist(rep(n_cells_by_cov, each = length(stim_list)))

    print(prop_by_stim_cov)


    df_final <- data.frame(Stimulus = stim_list)
    df_final$Resp <- n_responses_by_stim


    df_final$Proportion_of_responders <- prop_by_stim_responders
    df_final$Proportion_of_total_cells <- prop_by_stim


    d$Response <- ifelse(d$Response == TRUE, 1,0)


    if(one_cell == FALSE){
    res <- Compare_props(d)

    }

    if(one_cell == TRUE){
    res = NULL


    }
    ### Peak description by stimulus :

    if(length(data$Cell_id) != 0){
    data <- data[, peak_duration := End_peak_frame - Start_peak_frame]
}

    df_by_cov <- data.frame(Coverslip = rep(cov_list, each = length(stim_list)))
    df_by_cov$Stimulus <- rep(stim_list, times = length(cov_list))
    df_by_cov$Responses <- resp_by_cov_and_stim
    df_by_cov$Proportion <- prop_by_stim_cov
    df_by_cov$n_cells <- unlist(rep(n_cells_by_cov, each = length(stim_list)))

    df_by_cov <- df_by_cov[df_by_cov$Responses != 0, ]
  }

  if(compare_groups == TRUE) {

    d <- unique(df_clean[,c("Cell_id", "stimulus", "group", "coverslip")])


    d_list <- split(d,cumsum(1:nrow(d) %in% seq(1:nrow(d))))

    d_list <- lapply(d_list, function(x) data.table::setDT(x)[, Response := ifelse(is.na(
      data[data$Cell_id == x$Cell_id &
             data$Start_peak_stimulus == x$stimulus &
             data$group == x$group,]$Cell_id[1]),
      FALSE, TRUE)])


    d <- do.call(rbind, d_list)

    stim_list <- unique(d$stimulus)
    group_list <- unique(d$group)
    cov_list <- unique(d$coverslip)

    n_cov_by_group <- unlist(lapply(group_list, function(x) length(unique(d[d$group == x]$coverslip))))


    n_cells <- length(unique(d$Cell_id))
    n_cells_by_group <- lapply(group_list, function(x) as.character(dim(d[d$group == x, .(unique(Cell_id))])[1]))
    n_cells_by_cov <- lapply(cov_list, function(x) length(unique(d[d$coverslip == x]$Cell_id)))

    n_responders <- length(unique(d[d$Response == TRUE]$Cell_id))


    # Réponses par groupe :

    n_responses_by_group <- lapply(group_list, function(x) as.character(sum(d$group == x & d$Response == TRUE)))


    prop_by_group <- as.character(as.numeric(n_responses_by_group) / as.numeric(unlist(n_cells_by_group)))


    stats <- data.table::setDT(list("Group" = group_list, "n_cells" = n_cells_by_group, "n_responders" = n_responses_by_group,
                                    "Proportion" = prop_by_group))


    resp_by_group_and_stim <- unlist(lapply(group_list, function(x) lapply(stim_list, function(y) sum(d$group == x & d$stimulus == y & d$Response == TRUE))))

    prop_by_group_and_stim_responders <- resp_by_group_and_stim / rep(unlist(as.numeric(n_responses_by_group)), each = length(stim_list))
    prop_by_group_and_stim <- resp_by_group_and_stim / rep(unlist(as.numeric(n_cells_by_group)), each = length(stim_list))

    resp_by_group_stim_cov <- unlist(lapply(cov_list,function(x) lapply(stim_list, function(y) sum(d$coverslip == x & d$stimulus == y & d$Response == TRUE))))


    n_cells_by_cov <- rep(n_cells_by_cov, each = length(stim_list))

    prop_by_group_stim_cov <- resp_by_group_stim_cov / unlist(n_cells_by_cov)

    group_list <- rep(unlist(purrr::map2(group_list, n_cov_by_group, function(x,y) rep(x, y) )), each = length(stim_list))


    df_by_cov <- data.frame(Coverslip = rep(cov_list, each = length(stim_list)))
    df_by_cov$Group <- group_list
    df_by_cov$Stimulus <- rep(stim_list, times = length(cov_list))
    df_by_cov$Responses <- resp_by_group_stim_cov
    df_by_cov$Proportion <- prop_by_group_stim_cov
    df_by_cov$n_cells <- unlist(n_cells_by_cov)

    df_by_cov <- df_by_cov[df_by_cov$Responses != 0, ]
    #res <- glmer(Response ~ group * stimulus + (1|Cell_id), family = binomial, data = d)
    res <- "NO STATS"
  }

  print("Analyze Responses OK")
  #return(list(data_count, data_count_stim[[2]], between_stim[[1]], between_stim[[2]]))
  return(list(stats, df_by_cov, res))
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
