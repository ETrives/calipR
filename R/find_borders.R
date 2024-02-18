
#' find_borders
#'
#' This function finds the start and the end of each peak that has been found
#' from the previous step
#'
#'
#' @param data
#' @param range
#'
#' @return
#' @export
#'
#' @examples
find_borders <- function(data, range){


True_peaks <- data[[1]]
dt_2 <- data[[2]]

print( True_peaks)
print(dt_2)
#True_peaks <- dt_1[dt_1$True_peak == TRUE]


# Defining the range in which to find before and after the peak to find the
# start/end

True_peaks$start_window <- unlist(lapply(True_peaks$Max_peak_frame, function(x) x - as.integer(range)))
True_peaks$end_window <- unlist(lapply(True_peaks$Max_peak_frame, function(x) x + as.integer(range)))

# Splitting the first data table into a list of data table with each element
# containing the informations regarding one peak

find_1 <- split(True_peaks,cumsum(1:nrow(True_peaks) %in% seq(1:nrow(True_peaks))))

start_area_list <- find_start(find_1, dt_2, range)


peak_start <- do.call(rbind, start_area_list)


peak_start$Cell_id == start_area_list$Cell_id

end_area_list <- find_end(find_1, dt_2, range)
peak_end <- do.call(rbind, end_area_list)


True_peaks$Start_peak_frame <- peak_start$time_frame
True_peaks$Start_peak_derivative <- peak_start$smooth_Diff

True_peaks$Start_peak_stimulus <- peak_start$stimulus
True_peaks$Start_peak_rel_frame <- peak_start$Time_frame_stim
True_peaks$Start_peak_stimulation <- peak_start$Stimulation

True_peaks$End_peak_frame <- peak_end$time_frame
True_peaks$End_peak_derivative <- peak_end$smooth_Diff

True_peaks$End_peak_frame <- peak_end$time_frame

True_peaks$End_peak_rel_frame <- peak_end$Time_frame_stim

True_peaks$group <- peak_start$group


peaks <- split(True_peaks,cumsum(1:nrow(True_peaks) %in% seq(1:nrow(True_peaks))))

isnot.na <- Negate(is.na)

peaks <- lapply(peaks, function(x) if(isnot.na(x$End_peak_frame)) {x})

True_peaks <- do.call(rbind, peaks)

return(list(True_peaks, peak_start, peak_end))


}


