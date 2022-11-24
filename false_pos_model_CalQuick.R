### Generation du modèle pour prédire les faux positifs :

VNO <- prepareData("VNO", 5, 0.25, compare_groups = TRUE)
test_VNO_clean_1Hz <- clean_data(VNO, 0.1, 2, mean_width = 15, DPA_width = 5, mean_width_diff = 5)
test_VNO_back_1Hz <- backEstimate(test_VNO_clean_1Hz, method = "gam")
test_VNO_norm_1Hz <- norm_df(test_VNO_back_1Hz, var = "gam", width = 10)
test_VNO_peaks_1Hz <- find_peaks(test_VNO_norm_1Hz, threshold = 3, smooth = TRUE)


### Les cellules ci dessous viennent des coverslips du dossier VNO de maxime (3 groupes de chevres)
cell_label <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                1,1,1,1,
                1,1,1,1,1,1,1,0,1,1,1,1,0,1,1,1,1,1,1,1,0,
                1,1,1,1,1,1,0,1,0,1,1,1,0,0,0,
                1,1,1,0,0,0,1,0,1,1,0,0,1,0,
                0,1,1,1)

cell_id <- c("aaao", "aaba", "aabg", "aabi", "aaby", "aack","aact", "aadc","aadg","aadj","aadk","aadz","aaea","aaeh","aael", "aaem",

             "baib", "bakd","bakh","bapk",
             "caap", "cabc", "cabk", "cabz","caco","cacw","cadg","cadi","cadk","cadp","caea","caeb","caei","caew","casi","casj", "casv","catp", "catu","cbln","cblt",

             "daqo", "daqo", "daqo","dask", "datz","daue", "daue", "dauz", "dbor","dbqi", "dchz", "dcid", "dckf", "dckf", "dckf",
             "eagq", "eagw", "eagx", "eagy", "eagy", "eahb", "eahv", "eain", "eaio", "eair", "eaym", "eaym", "ebas", "ebdq",

             "faqg", "faqh", "fauu", "favc")


  # créer un data frame avec le nom de la cellule, le label et le nombre de valeurs inférieures

length(cell_id)
length(t$Cell_id)
t <- test_VNO_best_1Hz[[1]][test_VNO_best_1Hz[[1]]$Cell_id %in% cell_id]
t <- t[t$True_peak == TRUE]

t  <- t[, label := cell_label]

# ajouter une variable pour avoir l'info sur la bimodalité :

cell_split <- split(test_VNO_norm_1Hz, test_VNO_norm_1Hz$Cell_id)

y <- lapply(cell_split, function(x) x[, bimodal := is.bimodal(x$Mean_Grey)])

data <- do.call(rbind, y)
data <- unique(data[,c("Cell_id", "bimodal")])
data <- data[data$Cell_id %in% cell_id]

data <- split(data, data$Cell_id)

lapply(data, function(x) t[t$Cell_id == x$Cell_id, bimodal := x$bimodal])



# maintenant régression logistique :

model <- glm(formula = label ~  infValue + bimodal, family = "binomial", data = t)

summary(model)
pscl::pR2(model)["McFadden"]

saveRDS(model, file = "R/model.rds")


### Quand je calcule la bimodalité sur Mean Grey et que je mets ça en prédicteur, ça prédit 35% de la variance du label
### et quand j'ajoute inf value, les deux variables sont significatives et ça permet d'expliquer 78% de la variance
### il n'y a pas d'interaction entre les deux

# Sur mean_Grey wo peaks, ça explique plus que 10%
# sur la local mean ça explique 7%
