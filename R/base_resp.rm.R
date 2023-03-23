
#' base_resp.rm
#'
#' @param borders
#'
#' @return The dataset containing the peaks informations without the peaks of
#' the cells that responded to baseline.
#' @export
#'
#' @examples
base_resp.rm <- function(borders, full){

print(borders)
print(str(borders))

#cell_split <- split(borders[[1]], borders[[1]]$Cell_id)

'%notin%' <- Negate('%in%')


cell_split <- split(borders, borders$Cell_id)


base.rm <- lapply(cell_split, function(x) if ("1.Baseline" %notin% x$Start_peak_stimulus) {x})

base_resp <- names(base.rm[lengths(base.rm) == 0])

final_peaks <- do.call(rbind, base.rm)

base.rm_n <- length(cell_split) - length(unique(final_peaks$Cell_id))

print(paste0(paste0("Removed ", base.rm_n), " cells that responded to baseline"))
print("The cells removed are: ")
print(base_resp)



  final_full <- full[full$Cell_id %notin% base_resp,]


return(list(final_peaks, final_full))
}

