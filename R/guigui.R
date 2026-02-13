
#' guigui
#'
#' Exectue this function without any argument to launch the Graphical User Interface (GUI)
#'
#'
#' @return
#' @export
#'
#' @examples
guigui <- function(){



  ui <-
    shinydashboard::dashboardPage(skin = "blue",


                                  calipR::dashboardHeader(disable = TRUE),


                                  shinydashboard::dashboardSidebar(
                                    # Inclusion du JS pour envoyer le choix à l'input client
                                    shiny::tags$head(
                                      shiny::tags$script(shiny::HTML("
      Shiny.addCustomMessageHandler('setUserChoice', function(value) {
        Shiny.setInputValue('user_choice_js', value);
      });
    "))
                                    ),

                                    # Logo de l'application
                                    shiny::img(
                                      src = "logo/calipR_logo.png",
                                      width = "70%", height = "70%",
                                      style = "position: relative; top: -30px; left: 35px;"
                                    ),

                                    # SidebarMenu toujours visible avec l'accueil
                                    shinydashboard::sidebarMenu(id = "sidebarid",
                                                                shinydashboard::menuItem("Accueil", tabName = "start")
                                    ),

                                    shiny::uiOutput("dynamicSidebarMenu")),

                                  shinydashboard::dashboardBody(
                                    shinyjs::useShinyjs(),  # <= indispensable
                                    shiny::tags$head(shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles_final.css")),
                                    shinydashboard::tabItems(
                                      shinydashboard::tabItem(tabName = "start",
                                                              div(id = "loading_overlay",
                                                                  h3("Initializing..."),
                                                                  style = "padding:20px; color:#7a3193; font-weight:bold;"
                                                              ),
                                                              shiny::fluidRow(
                                                                shiny::column(6, shiny::actionButton("invitro", "In Vitro Project", width = "100%")),
                                                                shiny::column(6, shiny::actionButton("fiber", "Fiber Photometry Project", width = "100%"))
                                                              )
                                      ),
                                      shiny::uiOutput("dynamicTabItems"))))



  server <- function(input, output, session){


    shinyjs::hide("loading_overlay")

    user_choice <- reactiveVal("start")
    project <- reactiveValues(dir_path = "no path")
    orig_freq <- reactiveVal(value = 1)
    video_status <- reactiveValues(annotation = NULL)
    db <- reactiveValues()
    db$aligned <- NULL
    db$load <- c(1,2)

    output$dynamicSidebarMenu <- renderUI({
      if (user_choice() == "fiber") {
        fiberSideBarModuleUI("fiberSidebar", orig_freq = orig_freq)  # PAS de ()
      } else if (user_choice() == "invitro") {
        invitroSideBarModuleUI("invitroSidebar")  # éventuellement passer aussi
      } else {
        NULL
      }
    })
    output$dynamicTabItems <- renderUI({
      # Interface en fonction du choix
      if (user_choice() == "start") {
        return(NULL)
      }

      if (user_choice() == "invitro") {
        invitroBodyModuleUI("Body_invitro")

      } else {
        fiberBodyModuleUI("Body_fiber")


      }
    })


    ### dynamic sidebars :

    observeEvent(input$invitro, {
      shinyjs::show("loading_overlay")  #
      user_choice("invitro")

      set_calipR_env()

      prepModuleServer("prep1", create_input = reactive(input[["invitroSidebar-create"]]),
                       load_input = reactive(input[["invitroSidebar-load"]]),
                       project_type = user_choice(), db, project, orig_freq)

      vizRawModuleServer("viz1", db, project, orig_freq,
                         filter = reactive(input[["invitroSidebar-filter"]]),
                         downslider = reactive(input[["invitroSidebar-downslider"]]))


      session$sendCustomMessage("setUserChoice", "invitro")

      shinyjs::hide("loading_overlay")
      user_choice("invitro")
    })


    observeEvent(input$fiber, {
      shinyjs::show("loading_overlay")
      user_choice("fiber")

      set_calipR_env()


      prepModuleServer("prep1", create_input = reactive(input[["fiberSidebar-create"]]),
                       load_input = reactive(input[["fiberSidebar-load"]]),
                       project_type = user_choice(), db, project, orig_freq, video_status)

      vizRawModuleServer("viz1", db, project, orig_freq,
                         filter = reactive(input[["fiberSidebar-filter"]]),
                         downslider = reactive(input[["fiberSidebar-downslider"]]), video_status)

      analyzeFiberModuleServer("analyzeFiber1", db)

      session$sendCustomMessage("setUserChoice", "fiber")

      shinyjs::hide("loading_overlay")
      user_choice("fiber")
    })


    #observeEvent(input$invitro, {
    #shinydashboard::updateTabItems(session, "sidebarid", "prep")  # ou autre tabName selon ton ordre
    #prepModuleServer("prep1", create_input = reactive(input[["invitroSidebar-create"]]),
    #                project_type = user_choice())


    #})

    #    observeEvent(input$fiber, {
    #shinydashboard::updateTabItems(session, "sidebarid", "prep")  # ou autre tabName selon ton ordre
    # prepModuleServer("prep1", create_input = reactive(input[["fiberSidebar-create"]]),
    #                 project_type = user_choice())     })


    ############### Visualization Module ###################

    #####


    ### Analyze the full dataset
    ###

    ### Ajouter des visualisations des pourcentages par groupe, par coverslip etc. :

  }
  shiny::shinyApp(ui, server)

}
