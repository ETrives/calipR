# Workflow pour faire le clustering avec la méthode DFT EMD :


DFT <- function(x){

d <- GeneCycle::periodogram(x$gam_detrended)

return(d)
}


cell_split <- split(test_VNO_back_1Hz, test_VNO_back_1Hz$Cell_id)

data <- lapply(cell_split, function(x)  DFT(x))


matrix_gen <- function(f.data) {

  m1 <- matrix(f.data$spec)
  m1 <- cbind(m1, matrix(f.data$freq))

  return(m1)
}


data_m <- lapply(data, function(x) matrix_gen(x))

purrr::map2(rep(seq(1,length(data_m)), each = length(data_m)), data_m    ,function(x,y)

res <- lapply(seq(1,length(data_m)), function(x) lapply(data_m, function(y) emd(data_m[[x]], y)   ))
#data_full <- do.call(cbind, data)


### cette implémentation est beaucoup trop longue

data_r <- data_m[1:300]
res <- lapply(seq(1,length(data_r)), function(x) lapply(data_r, function(y) emd(data_r[[x]], y)   ))
