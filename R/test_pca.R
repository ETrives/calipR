### Possibilité de faire un clustering avec ACP / T-SNE / UMAP


install.packages("FactoMineR")
library(FactoMineR)

install.packages("ggcorrplot")
library(ggcorrplot)

install.packages("corrr")
library(corrr)

install.packages("factoextra")
library(factoextra)

library(devtools)

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")

d <- lapply(cell_split, function(x) data.table(x = GeneCycle::periodogram(x$gam_detrended)$spec))
d <- lapply(d, function(x) data.table::setDT(x)[, y := seq(1,length(x[[1]]))])
d <- lapply(d, function(x) tidyr::spread(data.frame(x), y, x.V1))

d[[2]]

d_final <- do.call(rbind, d)

#data_normalized <- scale(d)
#head(data_normalized)




corr_matrix <- cor(d_final)
ggcorrplot(corr_matrix)


data.pca <- princomp(corr_matrix)
summary(data.pca)

data.pca$loadings[, 1:2]

fviz_eig(data.pca, addlabels = TRUE)
