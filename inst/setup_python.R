library(reticulate)

# Vérifier si on est dans un environnement temporaire (devtools::check)
is_check <- grepl("file\\d+", getwd())


# Définir le chemin de l'environnement Conda
if (is_check) {
    package_path <- getwd()  # Lors de devtools::check()
} else {
    package_path <- system.file(package = "calipR")  # Après installation normale
}

env_path <- file.path(package_path, "calipr_env")

# Extraire l’environnement s'il n'existe pas déjà
if (!dir.exists(env_path) && file.exists(env_archive)) {
    message("Extraction de l’environnement Conda...")
    untar(env_archive, exdir = package_path)
}

# Vérifier si l'environnement Conda existe
if (!dir.exists(env_path)) {
    warning("L'environnement Conda embarqué est introuvable à : ", env_path)
} else {
    # Déterminer l'exécutable Python en fonction de l'OS
    if (Sys.info()[["sysname"]] == "Windows") {
        python_bin <- file.path(env_path, "python.exe")
    }

  else {
        python_bin <- file.path(env_path, "bin", "python")
    }

    # Vérifier si l’exécutable Python est présent
    if (file.exists(python_bin)) {
        # Forcer reticulate à utiliser l’environnement embarqué
        Sys.setenv(RETICULATE_PYTHON = python_bin)
        use_python(python_bin, required = TRUE)
        required_packages <- c("pandas", "tdt", "opencv-python")

    # Vérifier et installer les packages manquants
    for (pkg in required_packages) {
        if (!py_module_available(pkg)) {
            message(paste("Installation de", pkg, "via pip..."))
            py_install(pkg, pip = TRUE)
        }

      else {
            message(paste(pkg, "est déjà installé."))
        }
    }

     message("Tous les packages Python nécessaires sont installés !")


    }


  else {
        warning("L'exécutable Python de l'environnement Conda embarqué est introuvable : ", python_bin)
  }

}




