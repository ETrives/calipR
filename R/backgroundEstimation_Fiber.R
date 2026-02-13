
#' rolling_pct
#'
#' Estimates the baseline by iteratively keeping only values in a user defined
#' rolling quantile (only keeps the values between 10 to 40 % lowest values).
#' This computation is done on a user-defined rolling time window, and the number
#' of iteration can also be controlled.
#'
#'
#' @param data
#' @param w An integer indicating the width of the rolling time window
#' @param pct_lower A numeric indicating the lower boundary of the quantile window
#' @param pct_upper A numeric indicating the upper boundary of the quantile window
#' @param it An integer indicating the number of iterations
#' @param var a character string indicating the name of the variable to be used
#'
#' @returns
#' @export
#'
#' @examples
rolling_pct <- function(data, w, pct_lower,pct_upper, it, var){

  data <- setDT(lapply(seq(0,length(it)),function(x) data[,paste0("Mean_Grey",x) := get(var)])[[length(it)]])

  print(data)
  for (i in it){

    print(paste0("round", i))
    print(paste0("w", w))

    print(length(data$time_frame))
    data[, rolling_quant_lower := gplots::wapply(time_frame, .SD[[paste0("Mean_Grey",i-1)]],fun =
                                                   function(x) quantile(x,probs =pct_lower, na.rm = TRUE)[[1]],
                                                 n = length(time_frame),  width = w, method = "nobs", drop.na = FALSE)[[2]]]


    data[, rolling_quant_upper := gplots::wapply(time_frame, .SD[[paste0("Mean_Grey",i-1)]],fun =
                                                   function(x) quantile(x,probs =pct_upper, na.rm = TRUE)[[1]],
                                                 n = length(time_frame),  width = w, method = "nobs", drop.na = FALSE)[[2]]]


    print("yo")
    data[, below_rolling_quant := ifelse( get(rlang::sym(paste0("Mean_Grey",i))) < rolling_quant_upper & get(rlang::sym(paste0("Mean_Grey",i))) > rolling_quant_lower, TRUE, FALSE)]

    print(data)


    print("yak")
    data[, paste0("Mean_Grey",i) := ifelse(below_rolling_quant == TRUE, get(rlang::sym(paste0("Mean_Grey",i))), NA)]

    print(data)
    print("youk")
    data[, paste0("Mean_Grey",i) := ifelse(time_frame == 1 |time_frame == .N, rolling_quant_upper,get(rlang::sym(paste0("Mean_Grey",i)))), by = Cell_id]


    print(data)

    print("yek")
    data[, paste0("Mean_Grey",i) := approxfun(which(!is.na(data[[paste0("Mean_Grey",i)]])), na.omit(data[[paste0("Mean_Grey",i)]]))(seq_along(data[[paste0("Mean_Grey",i)]]))]

    print(data)
    print("yok")


    #data[which(is.na(Mean_Grey))]$Mean_Grey <- data$rolling_med[1]
  }


  return(data)
}


periExtractR_one_indiv <- function(data, indiv, var, before, after){

  peri_extracted <- PeriEventExtractR(data[ID == unique(data[["ID"]])[indiv]], var, before,after)

  lapply(seq(1,length(peri_extracted[[var]])),
         function(x) peri_extracted[[var]][[x]][, event_id := x])

  data <- do.call(rbind,peri_extracted[[var]])
  return(data)
}

# Extraction des perievents pour tous les individus

periExtractR_all_indiv <- function(data, indiv_var, event_var, before, after){

  res <- lapply(seq(1,length(unique(indiv_var))), function(x) periExtractR_one_indiv(data,x,event_var, before, after))
  res_final <- do.call(rbind, res)
  return(res_final)
}

# Calcul de l'aire sous la courbe pour chaque évenement

auc_computR <- function(data, neg_val = FALSE, by_evt = FALSE, var = "base_norm",
                        time_limit = 30, time_period, volatile = FALSE){

  data[, period := ifelse(TIME_FROM_EVENT <= 0, "before","after")]

  if(volatile == FALSE){
    if(by_evt == TRUE){
      data[TIME_FROM_EVENT <= time_limit, auc := flux::auc(TIME_SECONDS, get(var)), by = .(event_id,period, group,ID,event)]
    }

    if(by_evt == FALSE){
      data[TIME_FROM_EVENT <= time_limit, auc := flux::auc(TIME_SECONDS, get(var)), by = .(event_id,period, group,ID)]
    }
  }

  if(volatile == TRUE){
    print("volatile")

    data[, period := ifelse(TIME_FROM_EVENT %between% time_period,"after","before")]
    print("unique(data$period)")
    print(unique(data$period))

    data[, auc := flux::auc(TIME_SECONDS, get(var)), by = .(event_id, period, group,ID, event)]
    print("volatile done")
  }

  data$period <- factor(data$period, levels = c("before", "after"))

  data$group <- factor(data$group, levels = unique(data$group))
  data$start_behavior <- factor(data$start_behavior, levels = unique(data$start_behavior))

  if(neg_val == FALSE){

    data[, auc := ifelse(auc < 0, 0, auc)]

  }


  # Computing a delta AUC between before and after :
  #data[, delta_auc := unique(.SD[period == "after"]$auc) - unique(.SD[period == "before"]$auc)
  #      , by = .(event_id, group,ID)]

  return(data)

}

rm_out <- function(dt, type = c("tag", "remove")){
  '%notin%' <- Negate("%in%")

  dt <- na.omit(dt)
  sum <- dt[, boxplot(mean_auc_indivs)$out, by = .(group, period)]

  if(type == "remove"){
    dt <- dt[mean_auc_indivs %notin% sum$V1]
  }
  return(dt)

}
