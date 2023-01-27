

#' .onLoad
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
