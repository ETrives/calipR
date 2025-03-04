

#' onload
#'
#' @param libname
#' @param pkgname
#'
#' @return
#' @export
#'
#' @examples
.onLoad <- function(libname, pkgname) {
  # 📌 Ajouter le chemin du logo pour l'interface Shiny
  shiny::addResourcePath(
    prefix = "logo",
    directoryPath = system.file(
      "logo",
      package = "calipR"
    )
  )

  # Déterminer si on est en cours d'installation
  is_install <- grepl("file\\d+", getwd())

  # Définir les chemins en fonction de l'état d'installation
  if (is_install) {
    package_path <- getwd()  # Pendant l'installation
    install_script <- file.path(package_path, "inst", "install.R")  # `install.R` encore dans `inst/`
    setup_script <- file.path(package_path, "inst", "setup_python.R")  # `setup_python.R` encore dans `inst/`
    first_run_flag <- file.path(package_path, "inst", ".calipR_first_run")  # Flag pendant installation
    env_path <- file.path(package_path, "inst", "calipr_env")  # Env Conda dans `inst/`
  } else {
    package_path <- system.file(package = pkgname)  # Après installation
    install_script <- file.path(package_path, "install.R")  # `install.R` est maintenant à la racine
    setup_script <- file.path(package_path, "setup_python.R")  # `setup_python.R` est maintenant à la racine
    first_run_flag <- file.path(package_path, ".calipR_first_run")  # Flag après installation
    env_path <- file.path(package_path, "calipr_env")  # Env Conda déplacé à la racine
  }

  # Configurer Python à chaque session
  if (Sys.info()[["sysname"]] == "Windows") {
    python_bin <- file.path(env_path, "python.exe")
  } else {
    python_bin <- file.path(env_path, "bin", "python")
  }

  # Vérifier que l’environnement Python embarqué existe
  if (file.exists(python_bin)) {
    Sys.setenv(RETICULATE_PYTHON = python_bin)
    message("✅ Python embarqué configuré pour cette session : ", python_bin)
  } else {
    warning("⚠️ L’exécutable Python est introuvable dans l’environnement Conda embarqué.")
  }

  # Vérifier si c'est la première exécution après installation
  if (!file.exists(first_run_flag)) {
    writeLines("first run", first_run_flag)

    # Exécuter `install.R` si trouvé
    if (file.exists(install_script)) {
      message("Exécution de install.R après installation...")
      source(install_script)
    } else {
      warning("⚠️ install.R introuvable après installation.")
    }
  } else {
    message("✅ calipR a déjà été configuré, `install.R` ne sera pas ré-exécuté.")
  }

  # 🚀 Exécuter `setup_python.R` à chaque session pour s'assurer que Python est bien configuré
  if (file.exists(setup_script)) {
    message("Exécution de setup_python.R...")
    source(setup_script)
  } else {
    warning("⚠️ setup_python.R introuvable. Vérifiez l'installation.")
  }

  # Afficher un message de confirmation
  message("calipR chargé. Python devrait être configuré correctement.")
}
