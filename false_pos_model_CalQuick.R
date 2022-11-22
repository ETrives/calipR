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

t <- test_VNO_best_1Hz[[1]][test_VNO_best_1Hz[[1]]$Cell_id %in% cell_id]

t <- t[, label := cell_label]


# maintenant régression logistique :

model <- glm(formula = label ~ infValue, family = "binomial", data = t)

summary(model)
pscl::pR2(model)["McFadden"]

saveRDS(model, file = "R/model.rds")
