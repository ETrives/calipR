library(reticulate)

# Déterminer si on est en train d'installer ou si le package est déjà installé
if ("calipR" %in% installed.packages()[, "Package"]) {
    package_path <- system.file(package = "calipR")  # Package installé
} else {
    package_path <- getwd()  # Pendant l'installation
}

# Définir les chemins
env_path <- file.path(package_path, "calipr_env")
env_archive <- file.path(package_path, "inst", "calipr_env.tar.gz")

# Extraire l’environnement s'il n'existe pas déjà
if (!dir.exists(env_path) && file.exists(env_archive)) {
    message("Extraction de l’environnement Conda...")
    untar(env_archive, exdir = package_path)
}

# Définir l'exécutable Python selon l'OS
if (Sys.info()[["sysname"]] == "Windows") {
    python_bin <- file.path(env_path, "python.exe")
} else {
    python_bin <- file.path(env_path, "bin", "python")
}

# Vérifier que Python a bien été extrait
if (!file.exists(python_bin)) {
    stop("L’exécutable Python de l’environnement Conda est introuvable.")
}

# Configurer `reticulate` pour utiliser l'environnement Python embarqué
Sys.setenv(RETICULATE_PYTHON = python_bin)
use_python(python_bin, required = TRUE)
