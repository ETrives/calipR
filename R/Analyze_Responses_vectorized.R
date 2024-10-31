#' Analyze_Responses
#'
#' Takes the full data table after the cleaning step and the peaks data table
#' (one line per calcium event) to provide counts and proportions of responding cells as a function of
#' a (combination of) user defined variable(s). If values are repeated measures, a cochran q test evaluates difference in at
#' least one condition then post hoc pairwise comparisons are calculated with a mcnemar test.
#'
#' @param data
#' @param df_clean
#' @param compare_groups
#'
#' @return
#' @export
#'
#' @examples
Analyze_Responses <- function(data, df_clean, compare_groups = FALSE,
                              one_cell = FALSE, marker = FALSE, var_list = NULL,
                              simulation = FALSE, by = NULL, var = NULL, stims,
                              alt = alt){


  data <- setDT(data)
  df_clean <- setDT(df_clean)

  print("data")
  print(data)

  print("df_clean")
  print(df_clean)

  'isnotna' <- Negate('is.na')

  df_clean <- df_clean[ isnotna(stimulus)]

  data <- data[ isnotna(stimulus)]

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


      d <- unique(df_clean[,c(c("Cell_id","stimulus"), ..var_list)])

    }
  }




  d <- data.table::setDT(d)

  data <- data.table::setDT(data)

    setkey(d, Cell_id, stimulus)
    #setkey(data, Cell_id, spike_stimulus)
    setkey(data, Cell_id, stimulus)

    response_indices <- unique(d[data, which = TRUE])

    d$Response <- FALSE
    d$Response[response_indices] <- TRUE


    stim_list <- unique(d$stimulus)

    n_cells_tot <- d[, .(.SD[stimulus == stim_list[[1]],.N]), by = var]$V1

    if("coverslip" %in% var_list){
      n_cells_cov <- d[, .(n_cells = length(unique(Cell_id))), by = coverslip]
    }


    if("group" %in% var_list){
      n_cells_grp <- d[, .(n_cells = length(unique(Cell_id))), by = group]
    }

    if("marker_positive" %in% var_list){
      marker_list <- unique(d$marker_positive)
    }

    n_cells_cond <- d[, .(n_cells = length(unique(Cell_id))), by = var_list]

    if(simulation == TRUE) {

        data <- d[, .(Responders = sum(Response)), by = stimulus]
        data <- data[, c("Prop", "n_cells_tot") := list(Responders/ sum(n_cells_tot), sum(n_cells_tot))]

    }

    else{

      if(is.null(var_list)){
        d <- unique(d[,c("Cell_id", "Response", "stimulus")])

        data <- d[, .(Responders = sum(Response),
                      n_cells = .N), by = stimulus]


      }
      else{
      d <- unique(d[,c(c("Cell_id", "Response"), ..var_list)])

      data <- d[, .(Responders = sum(Response),
                    n_cells = .N), by = var_list]

      }


    if("coverslip" %notin% var_list & "group" %notin% var_list & "marker_positive" %notin% var_list ){
      print("yoush")
      print(data)
      data <- data[, c("Prop", "n_cells_tot") := list(Responders/ n_cells, n_cells)]
      print("yash")
    }

    if("coverslip" %notin% var_list & "stimulus" %notin% var_list & "group" %in% var_list & "marker_positive" %notin% var_list){
      data <- data[, c("Prop", "n_cells_grp") := list(Responders/ n_cells_grp$n_cells, n_cells_grp$n_cells)]
    }

    if("coverslip" %notin% var_list & "stimulus" %in% var_list & "group" %in% var_list & "marker_positive" %notin% var_list){
      n_cells_grp <- rep(n_cells_grp$n_cells, each = length(stim_list))
      data <- data[, c("Prop", "n_cells_grp") := list(Responders/ n_cells_grp, n_cells_grp)]
    }


    if("coverslip" %notin% var_list & "stimulus" %notin% var_list & "group" %notin% var_list & "marker_positive" %in% var_list){
      n_cells_tot <- rep(sum(n_cells_tot), each = length(marker_list))
      data <- data[, c("n_cells_tot", "Prop_tot", "n_cells_marker", "Prop_marker_resp", "Prop_marker")
                   := list(n_cells_tot, Responders/ n_cells_tot, n_cells_cond$n_cells,
                           Responders/ n_cells_cond$n_cells, n_cells_cond$n_cells/n_cells_tot )]
    }


    if("coverslip" %notin% var_list & "group" %notin% var_list & "stimulus" %in% var_list & "marker_positive" %in% var_list){
      n_cells_tot <- rep(sum(n_cells_tot), each = length(marker_list)*length(stim_list))
      data <- data[, c("n_cells_tot", "Prop_tot", "n_cells_marker", "Prop_marker_resp", "Prop_marker")
                   := list(n_cells_tot, Responders/ n_cells_tot, n_cells_cond$n_cells,
                           Responders/ n_cells_cond$n_cells, n_cells_cond$n_cells/n_cells_tot )]
      }


    if("coverslip" %notin% var_list & "group" %in% var_list & "stimulus" %in% var_list & "marker_positive" %in% var_list){
      data$marker_positive <- factor(data$marker_positive, levels = marker_list, ordered = TRUE)

      data <- data[order(marker_positive)]
      n_cells_cond <- n_cells_cond[order(marker_positive)]

      group_list <- unique(d$group)
      n_cells_grp <- rep(rep(n_cells_grp$n_cells, each = length(stim_list)), times = length(marker_list))

      data <- data[, c("n_cells_grp", "Prop_tot", "n_cells_marker", "Prop_marker_resp", "Prop_marker")
                   := list(n_cells_grp, Responders/ n_cells_grp, n_cells_cond$n_cells,
                           Responders/ n_cells_cond$n_cells, n_cells_cond$n_cells/n_cells_grp )]
    }

    if("coverslip" %notin% var_list & "group" %in% var_list & "stimulus" %notin% var_list & "marker_positive" %in% var_list){


      data <- data[order(marker_positive)]
      n_cells_cond <- n_cells_cond[order(marker_positive)]


      group_list <- unique(d$group)
      n_cells_tot <- rep(sum(n_cells_tot), each = length(marker_list)*length(group_list))
      n_cells_grp <- rep(n_cells_grp$n_cells, times = length(marker_list))
      data <- data[, c("n_cells_grp", "Prop_tot", "n_cells_marker", "Prop_marker_resp", "Prop_marker")
                   := list(n_cells_grp, Responders/ n_cells_grp, n_cells_cond$n_cells,
                           Responders/ n_cells_cond$n_cells, n_cells_cond$n_cells/n_cells_grp )]
    }



    if("coverslip" %in% var_list & "stimulus" %notin% var_list & "group" %notin% var_list & "marker_positive" %notin% var_list){
       data <- data[, c("Prop", "n_cells_cov") := list(data$Responders/ n_cells_cov$n_cells, n_cells_cov$n_cells)]
    }



    if("coverslip" %in% var_list & "stimulus" %in% var_list & "marker_positive" %notin% var_list & "group" %notin% var_list){
      n_cells_cov <- rep(n_cells_cov$n_cells, each = length(stim_list))
      data <- data[, c("Prop", "n_cells_cov") := list(Responders/ n_cells_cov, n_cells_cov)]
    }

    if("coverslip" %in% var_list & "stimulus" %in% var_list & "marker_positive" %in% var_list & "group" %notin% var_list){

      data <- data[order(marker_positive)]
      n_cells_cond <- n_cells_cond[order(marker_positive)]

      n_cells_cov <- rep(rep(n_cells_cov$n_cells, each = length(stim_list)), times = length(marker_list))

      data <- data[, c("Prop_cov_marker", "n_cells_cov_marker", "Prop_cov", "n_cells_cov", "Prop_pos_cells_cov") := list(Responders/ n_cells_cond$n_cells, n_cells_cond$n_cells, Responders/ n_cells_cov, n_cells_cov, n_cells_cond$n_cells/n_cells_cov)]
    }

    if("coverslip" %in% var_list & "stimulus" %notin% var_list & "marker_positive" %in% var_list & "group" %notin% var_list){
      n_cells_cond <- d[, .(n_cells = length(unique(Cell_id))), by = list(coverslip, marker_positive)]
      data <- data[order(marker_positive)]
      n_cells_cov <- rep(n_cells_cov$n_cells, times = length(marker_list))

      data <- data[, c("Prop_cov_marker", "n_cells_cov_marker", "Prop_cov", "n_cells_cov", "Prop_pos_cells_cov") := list(Responders/ n_cells_cond$n_cells, n_cells_cond$n_cells, Responders/ n_cells_cov , n_cells_cov, n_cells_cond/n_cells_cov)]
    }


    if("coverslip" %in% var_list & "stimulus" %notin% var_list & "marker_positive" %in% var_list & "group" %in% var_list){

      data <- data[order(marker_positive)]
      n_cells_cond <- n_cells_cond[order(marker_positive)]

      n_cells_cov <- rep(n_cells_cov$n_cells, times = length(marker_list))

      data <- data[, c("Prop", "n_cells_cov_marker", "Prop_cov", "n_cell_cov") := list(Responders/ n_cells_cond$n_cells, n_cells_cond$n_cells, Responders/n_cells_cov, n_cells_cov)]

      n_cells_cond <- n_cells_cond[order(group, coverslip)]

      n_cov_group <-  d[, .(n_cov = length(unique(coverslip))), by = group]

  data <- data[order(coverslip)]

      n_cells_grp <- unlist(lapply(seq(length(n_cov_group$n_cov)), function(x) rep(n_cells_grp$n_cells[[x]], each = n_cov_group$n_cov[[x]]*length(marker_list))))

      data <- data[, c("n_cells_grp", "Prop_marker_resp_grp", "Prop_marker_cov_grp", "Prop_marker_cov") := list(n_cells_grp, Responders / n_cells_grp, n_cells_cond$n_cells/n_cells_grp, n_cells_cond$n_cells/n_cell_cov )]

    }



  if("coverslip" %in% var_list & "stimulus" %in% var_list & "marker_positive" %in% var_list & "group" %in% var_list){

    data <- data[order(marker_positive)]
    n_cells_cond <- n_cells_cond[order(marker_positive)]

    n_cells_cov <- rep(rep(n_cells_cov$n_cells, each = length(stim_list), times = length(marker_list)))

    data <- data[, c("Prop", "n_cells_cov_marker", "Prop_cov", "n_cell_cov") := list(Responders/ n_cells_cond$n_cells, n_cells_cond$n_cells, Responders/n_cells_cov, n_cells_cov)]

    n_cells_cond <- n_cells_cond[order(group, coverslip)]

    n_cov_group <-  d[, .(n_cov = length(unique(coverslip))), by = group]

    data <- data[order(coverslip)]

    n_cells_grp <- rep(unlist(lapply(seq(length(n_cov_group$n_cov)), function(x) rep(n_cells_grp$n_cells[[x]], each = n_cov_group$n_cov[[x]]*length(marker_list)))), each = length(stim_list))

    data <- data[, c("n_cells_grp", "Prop_marker_resp_grp", "Prop_marker_cov_grp", "Prop_marker_cov") := list(n_cells_grp, Responders / n_cells_grp, n_cells_cond$n_cells/n_cells_grp, n_cells_cond$n_cells/n_cell_cov )]

  }




  if("coverslip" %in% var_list & "stimulus" %in% var_list & "marker_positive" %notin% var_list & "group" %in% var_list){

    n_cells_cond <- n_cells_cond[order(group)]

    n_cells_cov <- rep(n_cells_cov$n_cells, each = length(stim_list))

    data <- data[, c("Prop", "n_cells_cov_marker", "Prop_cov", "n_cell_cov") := list(Responders/ n_cells_cond$n_cells, n_cells_cond$n_cells, Responders/n_cells_cov, n_cells_cov)]

    n_cells_cond <- n_cells_cond[order(group, coverslip)]

    n_cov_group <-  d[, .(n_cov = length(unique(coverslip))), by = group]

    data <- data[order(coverslip)]

    n_cells_grp <- rep(unlist(lapply(seq(length(n_cov_group$n_cov)), function(x) rep(n_cells_grp$n_cells[[x]], each = n_cov_group$n_cov[[x]]))), each = length(stim_list))

    data <- data[, c("n_cells_grp", "Prop_resp_grp", "Prop_cov_grp", "Prop_cov") := list(n_cells_grp, Responders / n_cells_grp, n_cells_cond$n_cells/n_cells_grp, n_cells_cond$n_cells/n_cell_cov )]

  }


}

    if(one_cell == FALSE & compare_groups == FALSE) {
      res <- Compare_props(d, by = by, var = var, stims,alt=alt)
    }



    if(one_cell == TRUE){
      res = NULL
    }


    if(one_cell == FALSE & compare_groups == TRUE){
      res <- "NO STATS"
    }


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
Compare_props <- function(data, by = NULL, var = NULL, stim_list,alt=alt){

  if(is.null(by)){
  data_bis <- data
  res_tot <- rstatix::cochran_qtest(data, Response~stimulus|Cell_id)
  res_post_hoc <- rstatix::pairwise_mcnemar_test(data, Response~stimulus|Cell_id)
  }

  else{

    print(stim_list)
  data <- data[get(by) %in% stim_list]

  print(data)
  lev <- unique(data[[eval(by)]])
  it <- seq(1,length(lev))

  res_tot <- list()

  for(i in it){

    dt <- data[get(by) == lev[i]]
    cont <- table(dt$Response, dt[[eval(var)]])

    print(length(cont))
    print(cont)
    if(length(cont) > 4){
      cont <- t(cont)
      res_tot[[i]] <- prop.test(cont)

    }

    if(length(cont) == 4){

      res_tot[[i]] <- fisher.test(cont, alternative = alt)

    }




    res_post_hoc <- NULL
  }

  }

  return(list(res_tot, res_post_hoc))
}

lm_tidy <- function(df, x, y) {
  # take the arguments as code pieces instead to evaluate them:
  .x <- as.name(x)
  .y <- as.name(y)
  .df <- substitute(df)
  # take the code piece `y ~ x` and substitute using list lookup table
  .fm <- substitute(y ~ x, list(y=.y, x=.x))

  print(.fm)
  # take the code `lm(fm, data=df)` and substitute with the code pieceses defined by the lookup table
  # by replacing them by the code pieces stored in `.fm` and `.df`
  # and finally: evaluate the substituted code in the parent environment (the environment where the function was called!)
  eval.parent(substitute(rstatix::cochran_qtest(formula = fm, data=df), list(fm=.fm, df=.df)))
}

formulate <- function(x, y, z) {
  .x <- as.name(x)
  .y <- as.name(y)
  .z <- as.name(z)

  .fm <- substitute(y ~ x | z, list(y=.y, x=.x, z=.z))

  return(.fm)
  #eval.parent(substitute(rstatix::cochran_qtest(formula = fm, data=df), list(fm=.fm, df=.df)))

  #eval.parent(substitute(lm(fm, data=df), list(fm=.fm, df=.df)))
}


#' base_resp.rm
#'
#' Identifies and removes baseline responders from the full and the peaks datatable
#' @param borders
#'
#' @return each dataset without data of cells responding to baseline
#' @export
#'
#' @examples
base_resp.rm <- function(borders, full){



  '%notin%' <- Negate('%in%')


  cell_split <- split(borders, borders$Cell_id)


  #base.rm <- lapply(cell_split, function(x) if ("1.Baseline" %notin% x$spike_stimulus) {x})

  base.rm <- lapply(cell_split, function(x) if ("1.Baseline" %notin% x$stimulus) {x})

  base_resp <- names(base.rm[lengths(base.rm) == 0])

  final_peaks <- do.call(rbind, base.rm)

  base.rm_n <- length(cell_split) - length(unique(final_peaks$Cell_id))

  print(paste0(paste0("Removed ", base.rm_n), " cells that responded to baseline"))
  print("The cells removed are: ")
  print(base_resp)



  final_full <- full[full$Cell_id %notin% base_resp,]


  return(list(final_peaks, final_full))
}

