#' fiberSideBarModuleUI
#'
#' UI of the fiber sidebar
#'
#' @param id
#' @param orig_freq
#'
#' @returns
#' @export
#'
#' @examples
fiberSideBarModuleUI <- function(id, orig_freq) {
  ns <- NS(id)
  shinydashboard::sidebarMenu(
    id = "sidebarid",
    shinydashboard::menuItem("Prepare Your Data", tabName = "prep"),
    shiny::conditionalPanel(
      'input.sidebarid === "prep"',
      shiny::checkboxInput(ns("create"), label = "Create New Project"),
      shiny::checkboxInput(ns("load"), label = "Load Existing Project")
    ),

    shinydashboard::menuItem("Visualize Raw Data", tabName = "viz"),
    shiny::conditionalPanel(
      'input.sidebarid === "viz"',
      shiny::sliderInput(
        ns("filter"),
        label = "Choose a frequency filter",
        min = 2,
        max = 10000,
        value = 2,
        step = 1
      ),
      shiny::sliderInput(
        ns("downslider"),
        label = "Try Different Downsampling Values (Hz)",
        min = 0,
        max = 100,
        value = orig_freq(),
        step = 1
      ),
      shiny::actionButton(ns("saveFilteredData"), "Save Filtered Data")

    ),
    shinydashboard::menuItem("Analyze your data", tabName = "ana")
  )
}

