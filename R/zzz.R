

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


  # Afficher un message de confirmation
  message("calipR chargé")
}
