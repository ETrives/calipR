

#' set_calipR_env
#'
#'This function is used to set the current environment to the portable conda
#'environment included in calipR
#'
#'
#' @return
#' @export
#'
#' @examples
set_calipR_env <- function(){


  is_check <- grepl("file\\d+", getwd())

  # Définir le chemin du package
  if (is_check) {
    package_path <- getwd()  # Lors de devtools::check()
  } else {
    package_path <- system.file(package = "calipR")  # Après installation
  }

  env_path <- file.path(package_path, "calipr_env")
  env_archive <- file.path(package_path, "calipr_env.tar.gz")

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
reticulate::use_python(python_bin, required = TRUE)

}
