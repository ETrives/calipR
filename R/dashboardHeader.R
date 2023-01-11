#' dashboardHeader
#'
#' A variant of the dashboardHeader function from shinydashboard package that
#' allows to add a navbar title.
#' Taken from : https://github.com/rstudio/shinydashboard/issues/41
#'
#' @param ...
#' @param title
#' @param disable
#' @param title.navbar
#' @param .list
#'
#' @return
#' @export
#'
#' @examples
dashboardHeader <- function(..., title = NULL, disable = FALSE,title.navbar=NULL, .list = NULL) {
  items <- c(list(...), .list)
  #lapply(items, tagAssert, type = "li", class = "dropdown")
  shiny::tags$header(class = "main-header",
              style = if (disable) "display: none;",
              shiny::span(class = "logo", title),
              shiny::tags$nav(class = "navbar navbar-static-top", role = "navigation",
                       # Embed hidden icon so that we get the font-awesome dependency
                       shiny::span(shiny::icon("bars"), style = "display:none;"),
                       # Sidebar toggle button
                       shiny::a(href="#", class="sidebar-toggle", `data-toggle`="offcanvas",
                         role="button",
                         shiny::span(class="sr-only", "Toggle navigation")
                       ),title.navbar,
                       shiny::div(class = "navbar-custom-menu",
                           shiny::tags$ul(class = "nav navbar-nav",
                                   items
                           )
                       )
              )
  )
}
