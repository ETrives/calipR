#' invitroBodyModuleUI
#'
#' UI of the invitro body module
#'
#' @param id
#'
#' @returns
#' @export
#'
#' @examples
invitroBodyModuleUI <- function(id){
  shinydashboard::tabItems(
  shinydashboard::tabItem("prep", prepModuleUI("prep1")),

  ##### Bank Tab #########
  shinydashboard::tabItem("bank", bankModuleUI("bank1")),


  ##### Annotation Tab #########
  shinydashboard::tabItem("annotation", annotationModuleUI("annotation1")),

  ###

  shinydashboard::tabItem("viz", vizRawModuleUI("viz1")),

  ##### Optimization Tab #########
  shinydashboard::tabItem("opt", optimisationModuleUI("opt1")),


  ##### Analysis Tab #########
  shinydashboard::tabItem("ana_full", anaFullModuleUI("ana_full1")),



  ##### Results Visualization Tab #########
  shinydashboard::tabItem("viz_res", vizResModuleUI("viz_res1"))

)
}
