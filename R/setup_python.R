library(reticulate)

#' setup_python_calipR
#'
#' Run this function to setup the python environment required for calipR
#'
#' @returns
#' @export
#'
#' @examples
setup_python_calipR <- function(){

  package_path <- system.file(package = "calipR")
  first_run_flag <- file.path(package_path, ".calipR_first_run")  # Flag après installation

  # Définir les chemins vers l’environnement et l’archive compressée
  env_path <- file.path(package_path, "calipr_env")
  archive_path <- file.path(package_path, "calipr_env.tar.gz")

  # Vérifier si c'est la première exécution après installation
  if (!file.exists(first_run_flag)) {
    writeLines("first run", first_run_flag)

    # Extraire l’environnement s'il n'existe pas déjà
    if (!dir.exists(env_path) && file.exists(env_archive)) {
      message("Extraction de l’environnement Conda...")
      untar(env_archive, exdir = package_path)
      message("✅ Extraction terminée !")
    }

  }

  else{

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
    required_packages <- list(pandas = "pandas", tdt = "tdt", cv2 ="opencv-python")
    for (pkg in seq(1,length(required_packages))) {
      if (!py_module_available(names(required_packages)[pkg])) {
        message(paste("Installation de", pkg, "via pip..."))
        py_install(required_packages[[pkg]], pip = TRUE)
      } else {
        message(paste("✅", required_packages[[pkg]], "est déjà installé."))
      }
    }

    message("Tous les packages Python nécessaires sont installés !")

}
}
