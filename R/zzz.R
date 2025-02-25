

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


  shiny::addResourcePath(
    prefix = "logo",
    directoryPath = system.file(
      "logo",
      package = "calipR"
    )
  )

}



#' onAttach
#'
#' @param libname
#' @param pkgname
#'
#' @return
#' @export
#'
#' @examples
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Activation de l'environnement Python embarqué...")

  # Vérifier que le package est bien installé avant de charger le script
  setup_script <- system.file("setup_python.R", package = pkgname)
  env_path <- system.file("calipr_env.tar.gz", package = pkgname)

  if (setup_script != "" && file.exists(env_path)) {
    source(setup_script)

  } else {
    warning("L'environnement Conda embarqué est introuvable. Vérifiez l'installation.")
  }
}


