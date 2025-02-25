library(reticulate)

    # Vérifier si on est en train d’installer (devtools::check) ou si le package est installé
    is_check <- grepl("file\\d+", getwd())

    # Définir le chemin du package
    if (is_check) {
      package_path <- getwd()  # Lors de devtools::check()
    } else {
      package_path <- system.file(package = "calipR")  # Après installation
    }

    # Définir les chemins vers l’environnement et l’archive compressée
    env_path <- file.path(package_path, "calipr_env")
    
    # **Correction : L'archive n'est plus dans `inst/` après installation**
    if (is_check) {
      env_archive <- file.path(package_path, "inst", "calipr_env.tar.gz")  # Lors du check

      } else {
      env_archive <- file.path(package_path, "calipr_env.tar.gz")  # Après installation

      }

    # 🔍 Vérifier si l’archive est bien là
    if (!file.exists(env_archive)) {
      stop("L’archive Conda compressée est introuvable à : ", env_archive)
    } else {
      message("Archive Conda trouvée à : ", env_archive)
    }

    # Extraire l’environnement s'il n'existe pas déjà
    if (!dir.exists(env_path) && file.exists(env_archive)) {
      message("Extraction de l’environnement Conda...")
      untar(env_archive, exdir = package_path)
      message("✅ Extraction terminée !")
    }

    #Vérifier si l'environnement Conda existe après extraction
    if (!dir.exists(env_path)) {
      stop("L'environnement Conda embarqué est introuvable après extraction : ", env_path)
    }

    #Déterminer l'exécutable Python selon l'OS
    if (Sys.info()[["sysname"]] == "Windows") {
      python_bin <- file.path(env_path, "python.exe")
    } else {
      python_bin <- file.path(env_path, "bin", "python")
    }

    #Vérifier que Python a bien été extrait
    if (!file.exists(python_bin)) {
      stop("L’exécutable Python de l’environnement Conda est introuvable : ", python_bin)
    }

    #Configurer `reticulate` pour utiliser cet environnement
    Sys.setenv(RETICULATE_PYTHON = python_bin)
    use_python(python_bin, required = TRUE)

    #Installer les packages Python si besoin
    required_packages <- c("pandas", "tdt", "opencv-python")
    for (pkg in required_packages) {
      if (!py_module_available(pkg)) {
        message(paste("📦 Installation de", pkg, "via pip..."))
        py_install(pkg, pip = TRUE)
      } else {
        message(paste("✅", pkg, "est déjà installé."))
      }
    }

    message("Tous les packages Python nécessaires sont installés !")