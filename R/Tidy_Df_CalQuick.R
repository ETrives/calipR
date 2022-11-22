
#' tidy_df
#'
#' @param data the original raw dataset
#' @param stimuli a character vector with the names of the stimuli used
#' @param each an integer vector with the number of repetitions for each stimulus
#' @param pattern a character pattern in the colnames of the cells to be used
#' to gather the cells in one column
#'
#' @return A fully tidied dataset ready for filtering step
#' @export
#'
#' @examples

tidy_df <- function(data, stimuli, each, pattern, duration_in_seconds,
                    frame_rate, coverslip_id, id, multiple = FALSE, compare_groups = FALSE, groups) {

  df_stim <- stim_var(data, stimuli, each, frame_rate, coverslip_id)
  df_final <- cell_sort(df_stim, pattern, duration_in_seconds, frame_rate, id = id, multiple = multiple, compare_groups, groups)


  return(df_final)
}
