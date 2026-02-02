
#' invitroSideBarModuleUI
#'
#' UI of the invitro sidebar module
#'
#' @param id
#'
#' @returns
#' @export
#'
#' @examples
invitroSideBarModuleUI <- function(id) {
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
      shiny::uiOutput(ns("downsampling")),
      shiny::actionButton(ns("saveFilteredData"), "Save Filtered Data")
    ),
    shinydashboard::menuItem("Annotate Your Data", tabName = "annotation"),
    shiny::conditionalPanel(
      'input.sidebarid === "annotation"',
      shiny::textInput(ns("db_name_bis"), label = NULL, placeholder = "Database name"),
      shiny::uiOutput(ns("warning_db_bis"), class = "warn-side"),
      shiny::textInput(
        ns("annotation_project"),
        "How do you want to call this annotation project ?"
      ),
      shiny::actionButton(ns("start_creation_bis"), "Load Data", align = "center")
    ),

    shinydashboard::menuItem(
      "Create Your Banks", tabName = "bank"),
    shiny::conditionalPanel( 'input.sidebarid === "bank"',
                             shiny::textInput(ns("db_name"), label = NULL, placeholder = "Database name"),
                             shiny::uiOutput(ns("warning_db"), class = "warn-side"),
                             shiny::textInput(ns("bankName"), "How do you want to call this bank ?"),
                             shiny::actionButton(ns("start_creation"), "Load Data", align = "center")),
    shinydashboard::menuItem(
      "Optimize Analysis Parameters", tabName = "opt"),
    shinydashboard::menuItem(
      "Launch Full Analysis", tabName = "ana_full"),
    shiny::conditionalPanel( 'input.sidebarid === "ana_full"',
                             shiny::numericInput(ns("peak_thresh_full_z"), label = "Peak Threshold (z)", value = 3, min = 0),
                             shiny::numericInput(ns("peak_thresh_full_delta"), label = "Peak Threshold (deltaf/f)", value = 0, min =0),
                             shiny::textInput(ns("lambda_full"), label = "Lambda", value = "1000"),
                             shiny::textInput(ns("gam_full"), label = "gam", value = "0.9"),
                             shiny::selectInput(inputId = ns("norm_method_full"), label = "Choose method to compute z-score",
                                                list("Baseline Period" = "baseline", "Baseline Period without Peaks" = "estimate")),
                             shiny::checkboxInput(ns("patMatch"), label = "Background Estimation with SBPC"),
                             shiny::uiOutput(ns('posBank_field_full')),
                             shiny::uiOutput(ns('negBank_field_full')),
                             shiny::uiOutput(ns('warning_bank'), class = "warn-side"),
                             shiny::uiOutput(ns('warning_data'), class = "warn-side-long"),
                             fluidRow(shiny::actionButton(ns("ana_full_button"), "Launch Full Analysis", class ="launch-btn"
                             ))),
    shinydashboard::menuItem(ns("Visualize Results"), tabName = "viz_res")
  )
}
