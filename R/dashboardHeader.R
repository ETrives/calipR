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
  tags$header(class = "main-header",
              style = if (disable) "display: none;",
              span(class = "logo", title),
              tags$nav(class = "navbar navbar-static-top", role = "navigation",
                       # Embed hidden icon so that we get the font-awesome dependency
                       span(shiny::icon("bars"), style = "display:none;"),
                       # Sidebar toggle button
                       a(href="#", class="sidebar-toggle", `data-toggle`="offcanvas",
                         role="button",
                         span(class="sr-only", "Toggle navigation")
                       ),title.navbar,
                       div(class = "navbar-custom-menu",
                           tags$ul(class = "nav navbar-nav",
                                   items
                           )
                       )
              )
  )
}
