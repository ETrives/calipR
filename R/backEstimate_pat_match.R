#' patDetectR
#'
#' Computes the dynamic time warping distance between all the rolling subsequences
#' of a given length (or several given lengths) and all the patterns in two pattern
#' banks: a positive bank and a negative bank (and returns the distance for the
#' best match in each bank).So each index end up with two distance values for each
#' specified length (window). The value couple (pos;neg)
#' containing the value that is minimal, is kept. This value couple is then
#' used to compute a ratio (pos/neg), used to infer if the index belongs to signal
#' or noise
#'
#' @param dt
#' @param window
#' @param step
#' @param posBank
#' @param negBank
#' @param new_len
#' @param Var
#' @param Norm
#'
#' @return A data table containing the ratio for each index and related informations
#' @export
#'
#' @examples
patDetectR <- function(dt, posBank, negBank, Var, Norm = TRUE, windows = NULL) {

if(is.null(windows)){
  posBank<-posBank[!sapply(posBank,is.null)]
  negBank<-negBank[!sapply(negBank,is.null)]

  max_window <- max(unlist(lapply(posBank, length)), na.rm = TRUE)
  min_window <- min(unlist(lapply(posBank, length)), na.rm = TRUE)

  max_window_neg <- max(unlist(lapply(negBank, length)), na.rm = TRUE)
  min_window_neg <- min(unlist(lapply(negBank, length)), na.rm = TRUE)

  min_w <- min(min_window,min_window_neg, na.rm=TRUE)
  max_w <- max(max_window,max_window_neg, na.rm=TRUE)
  med_w <- round(median(min_w,max_w))

  window <- c(min_w, med_w, max_w)

  new_len <- max_w
}

  else{
    window <- windows
    new_len <- 30
  }

  # Extending the trace so that patterns in its end can be fully screened with large
  # windows :
  dt <- dt[, rbind(.SD,.SD[rep(.N, max(window)),]), by = Cell_id]

  # Computing steps for rolling subsequencing:
  step <- as.integer(sqrt(window))* 2

  # Subsequencing, Interpolation and Normalization
  data <- subinoR(dt, window, step, new_len, posBank, negBank, var = Var, norm = Norm)

  # DTW distance computing between each subsequence and each pattern
  pos <- distcomputR(data[[2]], data[[1]], step, window)
  neg <- distcomputR(data[[3]], data[[1]], step, window)


  # Median distance extraction by index and Ratio between pos and neg median dist
  res <- mdRatio(pos, neg)

  return(res)


}

# Depends on : -------------

#' subinoR
#'
#' Wrapper which does three preparation steps before computing the DTW distance
#' between the banks: subsequencing, interpolation, z normalization
#'
#' @param dt
#' @param window
#' @param step
#' @param new_len
#' @param posBank
#' @param negBank
#' @param norm
#' @param var
#'
#' @return a list containing a data table with all the interpolated and
#' normalized subsequences for each trace, two data tables with the interpolated
#' and normalized positive and negative banks and a list with the number of subsequences
#' for each specified subsequence window.
#'
#' @export
#'
#' @examples
subinoR <- function(dt, window, step, new_len, posBank, negBank, norm = TRUE, var = "Mean_Grey"){


  # Subsequence segmentation for each chosen window, for each cell in the dt :


  # Extracting the minimum time series size to resize them
  resizing <- min(unlist(lapply(split(dt, dt$coverslip), function(x) x[, .N ,by = Cell_id]$N[[1]])))


  subseq_list <- lapply(seq(1,length(window)), function(x)dt[, subsequencR(get(var),
                                                                           window[[x]], step[[x]], resizing), by = Cell_id])

  n_sub_seq_list <- lapply(subseq_list, function(x) length(names(x)) -1)


  # Intrerpolation to standardize requests and banks lengths:

  interp_patBank2 <- data.table::as.data.table(interpolR(posBank, new_len))
  interp_anomBank <- data.table::as.data.table(interpolR(negBank, new_len))

  interp_subseq_vec_list <- lapply(subseq_list, function(x)
    x[, data.table::as.data.table(interpolR(as.list(.SD),
                                            new_len)), by = Cell_id, .SDcols = -c("Cell_id")])

  # If wanted, z-normalization of each bank and requests :

  if(norm == TRUE){
    interp_patBank2 <- as.matrix(interp_patBank2[, lapply(.SD, function(x)
      matrixprofiler::znorm(x))])
    interp_anomBank <- as.matrix(interp_anomBank[, lapply(.SD, function(x)
      matrixprofiler::znorm(x))])
    interp_subseq_vec_list <- lapply(interp_subseq_vec_list, function(x)
      x[, lapply(.SD, function(x)matrixprofiler::znorm(x)),
        by = Cell_id])
  }


  return(list(interp_subseq_vec_list, interp_patBank2, interp_anomBank, n_sub_seq_list))

}

#' mdRatio
#'
#' Computes the median distance ratio between the best match in the positive and
#' the negative banks, for each index.
#'
#' @param pos
#' @param neg
#'
#' @return a data table with the median ratio in one column
#' @export
#'
#' @examples
mdRatio <- function(pos, neg){

  pos[, neg_val := neg$value]

  pos <- pos[, matFillR_bis_bis(.SD),by = .(win, Cell_id)]

  pos[ , c("min_val", "min_neg_val") := list(min(.SD$pos_val),min(.SD$neg_val)) ,
       by = .(Cell_id, idx), .SDcols = c("pos_val","neg_val")]

  pos2 <- pos[min_val == pos_val | min_neg_val == neg_val][order(Cell_id, idx)]
  pos2[, min_min := min(.SD) , by = .(Cell_id, idx,win), .SDcols = c("min_val", "min_neg_val")]

  pos2 <- pos2[pos_val == min_min |neg_val == min_min]

  pos2[, ratio := pos_val/neg_val]

  return(pos2)
}

#' distcomputR
#'
#'Computing and extracting the distance between each subsequence and
# the positive and negative banks. The distance with the pattern which is minimal
# is retrieved.
#'
#' @param patMat
#' @param subseqMat
#' @param step
#' @param window
#'
#' @return
#' @export
#'
#' @examples
distcomputR <- function(patMat, subseqMat, step, window){

  pos_dist_list <- lapply(subseqMat, function(x) x[,  lapply(.SD, function(y)
    rucrdtw::ucrdtw_mv(patMat, y, dtwwindow =0.05)$distance), by = Cell_id,
    .SDcols = -c("Cell_id")])

  col_len_list <- lapply(pos_dist_list, names)

  pos_dist_long_list <- purrr::map2(pos_dist_list, col_len_list, function(x,y)
    data.table::melt(x, measure.vars = 2:length(y))[order(Cell_id)])

  lapply(seq(1,length(pos_dist_long_list)), function(x)
    pos_dist_long_list[[x]][, variable2 := seq(1,.N*step[[x]], by = step[[x]]), by = Cell_id])

  # Adding the sequences indices

  lapply(seq(1,length(pos_dist_long_list)), function(x)
    pos_dist_long_list[[x]][, sub_seq := .(.(seq(0,window[[x]]) + variable2)), by = .(Cell_id, variable2)])

  lapply(seq(1,length(pos_dist_long_list)), function(x) pos_dist_long_list[[x]][, win := window[[x]]])

  final <- do.call(rbind, pos_dist_long_list)

  return(final)
}

#' matFillR_bis_bis
#'
#' Creates a matrix to align all the distance values obtained with the different
#' subsequences together with the indexes of the original trace. For each index,
#' the kept distance is the median across all the overlapping distances
#'
#' @param dt
#'
#' @return
#' @export
#'
#' @examples
matFillR_bis_bis <- function(dt){

  idx <- matrix(unlist(dt$sub_seq), ncol = length(dt$sub_seq))
  mat_pos <- matrix(nrow = length(dt$sub_seq), ncol = max(unlist(dt$sub_seq)))
  mat_neg <- mat_pos

  for(i in seq(1,length(dt$value))){

    mat_pos[i,idx[,i]] <- dt$value[i]
    mat_neg[i,idx[,i]] <- dt$neg_val[i]

  }


  pos_val <- matrixStats::colMins(mat_pos, na.rm = TRUE)
  neg_val <- matrixStats::colMins(mat_neg, na.rm = TRUE)

  dt_pos <- data.table::as.data.table(pos_val)[, c("neg_val", "idx") := list( neg_val, seq(1,.N))]


  return(dt_pos)
}



#' subsequencR
#'
#' Vectorized function to subset equal length windows from a longer time series.
#'
#' @param time_series
#' @param window
#' @param step
#'
#' @return a data table of subsequences
#' @export
#'
#' @examples
subsequencR <- function(time_series, window, step, resizing){

  if(window > length(time_series)){
    stop("Window length cannot be bigger than time series length")
  }

  if(step > length(time_series)){
    stop("step length cannot be bigger than time series length")
  }


  time_series <- interpolR(list(time_series), resizing)


  s0 <- seq(0,window)

  # définir un itérateur qui va de 1 à longueur du tracé à découper (l) - m

  l <- length(time_series)
  it <- seq(1,l-window, by = step)
  sub_seqs <- lapply(it, function(x) s0 + x)

  # Creating a 2-column dt: 1 is the base sequence the other contains a list with all subsequences

  sub_dt <- data.table::data.table("orig_seq" = list(time_series),
                                   "sub_seq" = sub_seqs)[, id := seq(1,.N)]


  # Vectorized implementation to subset subsequences

  sub_dt[, sub_seq_final := .(.(unlist(.(.(orig_seq)[1])[[1]])[sub_seq[[1]]])), by = id]


  dt <- data.table::setDT(sub_dt$sub_seq_final)

  return(dt)

}

#' interpolR
#'
#' Takes a list of time series patterns and the length to which each must be
#' linearly interpolated
#'
#' @param list
#' @param len
#'
#' @return a matrix where each initial pattern has length len.
#' @export
#'
#' @examples
interpolR <- function(list, len, type = c("one","multiple")){


  dt <- data.table::data.table(list)[, id := seq(1,.N)]

  dt[,  final := .(.(approx(seq(1,length(unlist(.(.(list)[1])[[1]]))),
                            unlist(.(.(list)[1])[[1]]), method = "linear", ties = mean,
                            n = len)$y)), by = id]


  mat <- do.call(cbind, dt$final)


  return(mat)
}



#' backEstimatR
#'
#' Estimates background fluorescence changes not related to the signal identified
#' by the user (through the positive bank vs negative bank provided in the
#' previous step (patDetectR))
#'
#' @param dt
#'
#' @param patdet_out
#'
#' @return a data table with the estimated background trace in a new column called
#' "background" and the detrended Mean Grey values in a column called "background detrended"
#' @export
#'
#' @examples
backEstimatR <- function(dt, patdet_out) {

  patdet_out[, smooth_min_ratio := gplots::wapply(seq(1,.N), ratio,fun = min,
                                                 n = .N,  width = 20, method = "nobs")[[2]], by = Cell_id]


  patdet_out[, time_frame := seq(1,.N), by = Cell_id]

  data.table::setkey(dt, Cell_id, time_frame)
  data.table::setkey(patdet_out, Cell_id, time_frame)

  full_dt <- patdet_out[dt, on = c("Cell_id", "time_frame")]

  full_dt[, signal := ifelse(smooth_min_ratio > 0.95 & smooth_Diff < 2*median(smooth_Diff),
                             'Noise', ifelse(smooth_min_ratio < 0.95 & local_mean > median(local_mean),  'Signal', NA)), by = Cell_id]


  full_dt[, rolling_min_new := gplots::wapply(time_frame, Mean_Grey,fun = function(x) quantile(x, probs = 0.2, names = FALSE),
                                              n = length(time_frame),  width = 30, method = "nobs")[[2]], by = Cell_id]


  full_dt[, mean_grey_wo_peaks_new_new := ifelse(signal %in% c("Noise", NA), Mean_Grey, NA), by = Cell_id]

  # If all values have been removed : fix the first and last values

  full_dt[, mean_grey_wo_peaks_new_new := ifelse(time_frame %in% c(1,.N), rolling_min_new, mean_grey_wo_peaks_new_new), by = Cell_id]
  #full_dt[, mean_grey_wo_peaks_new_new := ifelse(time_frame %in% c(1,.N), Mean_Grey, mean_grey_wo_peaks_new_new), by = Cell_id]


  # Linear interpolation between removed values

  full_dt[,labels := cumsum(!is.na(mean_grey_wo_peaks_new_new)) , by = Cell_id]
  full_dt[,labels2 := seq(1,.N) , by = .(Cell_id, labels)]
  full_dt[,Diff := dplyr::lead(labels) - labels , by = Cell_id]

  full_dt[,lag_roll_min := dplyr::lag(rolling_min_new) , by = Cell_id]

  full_dt[,mean_grey_wo_peaks_new_new := ifelse(labels2 == 1 & Diff == 0,
                                                lag_roll_min[[1]],
                                                ifelse(labels2 == .N &
                                                         is.na(mean_grey_wo_peaks_new_new),lag_roll_min[[.N]],mean_grey_wo_peaks_new_new)),
          by = .(Cell_id, labels)]

  full_dt[, mean_grey_wo_peaks_new_new := ifelse(time_frame %in% c(1,.N), rolling_min_new, mean_grey_wo_peaks_new_new), by = Cell_id]

  full_dt[, mean_grey_wo_peaks_new_new := approxfun(which(!is.na(mean_grey_wo_peaks_new_new)), na.omit(mean_grey_wo_peaks_new_new))(seq_along(mean_grey_wo_peaks_new_new)), by = Cell_id]


  # Background estimation through a rolling median (1st round) :


  full_dt[, rolling_med_new := gplots::wapply(time_frame, mean_grey_wo_peaks_new_new,fun = median,
                                              n = length(time_frame),  width = 30, method = "nobs")[[2]], by = Cell_id]


  full_dt[, below_med := ifelse(mean_grey_wo_peaks_new_new < rolling_med_new, TRUE, FALSE), by = Cell_id]

  # Removing values upper the rolling median :
  full_dt[, mean_grey_wo_peaks_new_new := ifelse(below_med == TRUE, mean_grey_wo_peaks_new_new, NA)]

  full_dt[, mean_grey_wo_peaks_new_new := ifelse(time_frame %in% c(1,.N), rolling_min_new, mean_grey_wo_peaks_new_new), by = Cell_id]

  # Linear interpolation of removed values :
  full_dt[, mean_grey_wo_peaks_new_new := approxfun(which(!is.na(mean_grey_wo_peaks_new_new)), na.omit(mean_grey_wo_peaks_new_new))(seq_along(mean_grey_wo_peaks_new_new)), by = Cell_id]

  # Last rolling median to estimate the background
  full_dt[, background := gplots::wapply(time_frame, mean_grey_wo_peaks_new_new,fun = median,
                                         n = length(time_frame),  width = 30, method = "nobs")[[2]], by = Cell_id]

  full_dt[, background := ifelse(time_frame %in% c(seq(1,10),.N), Mean_Grey, background)]

  full_dt[, background_detrended := Mean_Grey - background, by = .(Cell_id, coverslip)]

  final_dt <- full_dt[, .(Cell_id, Mean_Grey, time_frame, time_seconds, stimulus,
                          coverslip,  group, Stimulation, marker_positive, local_mean,
                          first_derivative, smooth_Diff, signal, background, background_detrended,
                          Time_frame_stim
                          )]

  return(final_dt)
}

