#' fiberBodyModuleUI
#'
#' UI of the fiber body
#'
#' @param id
#'
#' @returns
#' @export
#'
#' @examples
fiberBodyModuleUI <- function(id){
  shinydashboard::tabItems(
    shinydashboard::tabItem("prep", prepModuleUI("prep1")),
    shinydashboard::tabItem("viz", vizRawModuleUI("viz1")),
    shinydashboard::tabItem("ana", analyzeFiberModuleUI("analyzeFiber1"))
  )
}
