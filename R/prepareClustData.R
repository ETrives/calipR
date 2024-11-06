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
  cell_split <- split(sub_dt, sub_dt$Cell_id)
  cell_split <- lapply(cell_split, function(x) data.table(as.double(t(x[,1]))))

  print("cell_split")
  print(cell_split)

  final <- do.call(cbind, cell_split)

  if(norm == TRUE){
    final <- final[,lapply(.SD, function(x) matrixprofiler::znorm(x))]
  }

  final <- t(final)

  return(final)
}


#' clustCellID
#'
#' Labels each cell in peaks_data with the corresponding cluster.
#'
#' @param clust_res dtwclust object resulting from a clustering step
#' @param peaks data table containing peaks in lines
#' @param to_rm a vector containing cell names (e.g. "A1aaa) to exclude from the identification. Leave empty keeping all cells.
#'
#' @return the peaks_data data table annotated with a new variable "cluster"
#' @export
#'
#' @examples
clustCellID <- function(clust_res, peaks, to_rm){

  '%notin%' <- Negate('%in%')

  # Identifying cells within each cluster
  cluster_num <- unique(clust_res@cluster)
  clust_cells <- lapply(seq(1,length(cluster_num)), function(x) which(clust_res@cluster == x))

  peaks <- setDT(peaks)[Cell_id %notin% to_rm]
  peaks <- peaks[, cell_id_bis := rleid(Cell_id)]

  lapply(seq(1,length(clust_cells)), function(x) peaks[cell_id_bis %in% clust_cells[[x]], cluster := x])

  return(peaks)
}

