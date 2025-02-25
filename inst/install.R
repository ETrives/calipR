message("Exécution du script d'installation de calipR...")

# Déterminer si on est en train d’installer le package ou s’il est déjà installé
is_install <- grepl("file\\d+", getwd())

# Définir le chemin du package
if (is_install) {
    package_path <- getwd()  # Pendant l'installation
    setup_script <- file.path(package_path, "inst", "setup_python.R")  # setup_python.R encore dans inst/
    env_path <- file.path(package_path, "inst", "calipr_env.tar.gz")
} else {
    package_path <- system.file(package = "calipR")  # Après installation
    setup_script <- file.path(package_path, "setup_python.R")  # setup_python.R déplacé à la racine
    env_path <- file.path(package_path, "calipr_env.tar.gz")
}

# Vérifier si Miniconda est installé
if (!dir.exists(reticulate::miniconda_path())) {
    message("Miniconda non trouvé, installation en cours...")
    reticulate::install_miniconda()
    message("✅ Miniconda installé avec succès.")
}

# Activation de l'environnement Python embarqué
packageStartupMessage("⚙️ Activation de l'environnement Python embarqué...")

# 🚀 Vérifier si le package est bien installé avant de charger `setup_python.R`
if (file.exists(setup_script) && file.exists(env_path)) {
    message("Exécution de setup_python.R...")
    source(setup_script)
} else {
    warning("L'environnement Conda embarqué est introuvable. Vérifiez l'installation.")
}

message("✅ Installation de calipR terminée.")
