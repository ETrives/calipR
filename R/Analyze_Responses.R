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
Analyze_Responses <- function(data, df_clean, compare_groups = FALSE){

  ### Adding a variable "Response" for each stimulus in df_clean

  if(compare_groups == FALSE){

    d <- unique(df_clean[,c("Cell_id", "stimulus")])
    d_list <- split(d,cumsum(1:nrow(d) %in% seq(1:nrow(d))))

    d_list <- lapply(d_list, function(x) data.table::setDT(x)[, Response := ifelse(is.na(
      test_classify_1Hz[test_classify_1Hz$Cell_id == x$Cell_id &
                          test_classify_1Hz$Start_peak_stimulus == x$stimulus,]$Cell_id[1]),
      FALSE, TRUE) ])

    d <- do.call(rbind, d_list)

    stim_list <- unique(d$stimulus)

    n_cells <- length(unique(d$Cell_id))

    n_responders <- sum(d$Response == TRUE)
    prop_total <- n_responders / n_cells

    n_responses_by_stim <- unlist(lapply(stim_list, function(x) sum(d$stimulus == x & d$Response == TRUE)))
    prop_by_stim <- n_responses_by_stim / n_cells
    prop_by_stim_responders <- n_responses_by_stim / n_responders

    df_final <- data.frame(Stimulus = stim_list)
    df_final$Resp <- n_responses_by_stim
    df_final$Proportion_of_responders <- prop_by_stim_responders
    df_final$Proportion_of_total_cells <- prop_by_stim


    d$Response <- ifelse(d$Response == TRUE, 1,0)
    res <- Compare_props(d)
  }

  if(compare_groups == TRUE) {

    d <- unique(df_clean[,c("Cell_id", "stimulus", "group")])
    d_list <- split(d,cumsum(1:nrow(d) %in% seq(1:nrow(d))))

    d_list <- lapply(d_list, function(x) data.table::setDT(x)[, Response := ifelse(is.na(
      test_classify_1Hz[test_classify_1Hz$Cell_id == x$Cell_id &
                          test_classify_1Hz$Start_peak_stimulus == x$stimulus &
                          test_classify_1Hz$group == x$group,]$Cell_id[1]),
      FALSE, TRUE) ])

    d <- do.call(rbind, d_list)
    stim_list <- unique(d$stimulus)
    group_list <- unique(d$group)

    n_cells <- length(unique(d$Cell_id))

    # Proportions totales
    n_responders <- sum(d$Response == TRUE)
    prop_total <- n_responders / n_cells

    #n_responses_by_stim <- unlist(lapply(stim_list, function(x) sum(d$stimulus == x & d$Response == TRUE)))
    # prop_by_stim <- n_responses_by_stim / n_cells
    # prop_by_stim_responders <- n_responses_by_stim / n_responders


    # Réponses par groupe :

    n_responses_by_group <- unlist(lapply(group_list, function(x) sum(d$group == x & d$Response == TRUE)))
    prop_by_group <- n_responses_by_group / n_cells
    resp_by_group_and_stim <- unlist(lapply(group_list, function(x) lapply(stim_list, function(y) sum(d$group == x & d$stimulus == y & d$Response == TRUE))))

    prop_by_group_and_stim_responders <- resp_by_group_and_stim / n_responders
    prop_by_group_and_stim <- resp_by_group_and_stim / n_cells

    df_final <- data.frame(Stimulus = rep(stim_list, times = length(group_list)))
    df_final$group <- rep(group_list, each = length(stim_list))
    df_final$resp <- resp_by_group_and_stim
    #df_final$non_resp <- n_responders - resp_by_group_and_stim
    #df_final$non_resp_total_cells <- n_cells - resp_by_group_and_stim
    df_final$prop_responders <- prop_by_group_and_stim_responders
    df_final$prop_total_cells <- prop_by_group_and_stim

    #res <- glmer(Response ~ group * stimulus + (1|Cell_id), family = binomial, data = d)
    res <- "NO STATS"
  }

  #return(list(data_count, data_count_stim[[2]], between_stim[[1]], between_stim[[2]]))
  return(list("n_cells" = n_cells, "n_responders" = n_responders, "Proportion" = prop_total, df_final, res))
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
