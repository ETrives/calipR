library(reticulate)

# Vérifier si on est dans un environnement temporaire (devtools::check)
is_check <- grepl("file\\d+", getwd())

# Définir le chemin de l'environnement Conda
if (is_check) {
    package_path <- getwd()  # Lors de devtools::check()
} else {
    package_path <- system.file(package = "calipR")  # Après installation normale
}

env_path <- file.path(package_path, "conda_env")

# Vérifier si l'environnement Conda existe
if (!dir.exists(env_path)) {
    warning("L'environnement Conda embarqué est introuvable à : ", env_path)
} else {
    # Déterminer l'exécutable Python en fonction de l'OS
    if (Sys.info()[["sysname"]] == "Windows") {
        python_bin <- file.path(env_path, "python.exe")
    } else {
        python_bin <- file.path(env_path, "bin", "python")
    }

    # Vérifier si l’exécutable Python est présent
    if (file.exists(python_bin)) {
        # Forcer reticulate à utiliser l’environnement embarqué
        Sys.setenv(RETICULATE_PYTHON = python_bin)
        use_python(python_bin, required = TRUE)
    } else {
        warning("L'exécutable Python de l'environnement Conda embarqué est introuvable : ", python_bin)
    }
}
