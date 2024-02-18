#' prepareClustData
#'
#' @param dt
#' @param clust_var
#' @param norm
#'
#' @return
#' @export
#'
#' @examples
prepareClustData <- function(dt, clust_var, norm = TRUE){

  sub_dt <- setDT(dt)[,.(Clustering_variable = get(clust_var),Cell_id)]
  print(sub_dt)
  cell_split <- split(sub_dt, sub_dt$Cell_id)
  print(cell_split[[1]])
  cell_split <- lapply(cell_split, function(x) data.table(as.double(t(x[,1]))))
  print(cell_split[[1]])
  final <- do.call(cbind, cell_split)
  print(final)

  if(norm == TRUE){
    final <- final[,lapply(.SD, function(x) matrixprofiler::znorm(x))]
  }

  final <- t(final)

  return(final)
}
