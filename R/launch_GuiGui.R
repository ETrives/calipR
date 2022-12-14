
#' launch_GuiGui
#'
#' Exectue this function without any argument to launch the Graphical User Interface (GUI)
#'
#'
#' @return
#' @export
#'
#' @examples
launch_GuiGui <- function(){
ui <-
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(
      shiny::tags$li(class = "dropdown",
      shiny::tags$style(".main-header {max-height: 180px}"),
      shiny::tags$style(".main-header .logo {height: 180px}"),
      shiny::tags$style(".main-header .logo {padding-top: 1%}"),
      shiny::tags$style(".main-header .logo {padding-bottom: 1%}")
      ),
      title = shiny::div(shiny::img(src = "logo/CalQuick_bis.png", width = "70%", height = "70%", align = "center"))),


    shinydashboard::dashboardSidebar(

      shiny::tags$style(".left-side, .main-sidebar {padding-top: 180px}"),
      shinydashboard::sidebarMenu(id = "sidebarid",
                  shinydashboard::menuItem(
                    "Description", tabName = "des"),


                  shinydashboard::menuItem(
                    "Tutorial", tabName = "tuto"),

                  shinydashboard::menuItem(
                    "Analyze Your Data", tabName = "ana"),

                  shinydashboard::menuSubItem("Prepare Data", tabName = "prep"),
                  shinydashboard::menuSubItem("Visualize Raw Data", tabName = "viz"),
                  shinydashboard::menuSubItem("Optimize Analysis Parameters", tabName = "opt"),
                  shinydashboard::menuSubItem("Analyze data", tabName = "ana_full"),



                  shiny::conditionalPanel( 'input.sidebarid == "prep"',
                                   shiny::selectInput("stim_num", label ="Stimuli number (/cell)", c("1" = "1", "2" = "2", "3" = "3",
                                                                                                                     "4" = "4", "5" = "5", "6"="6", "7"="7",
                                                                                                                      "8"="8", "9"="9","10"="10")),
                                   shiny::textInput("folder", label = NULL, placeholder = "Write folder's name (where all the files are)"),
                                   shiny::actionButton("launch", "Load & Tidy Data", align = "center")),

                 shiny::conditionalPanel( 'input.sidebarid == "viz"',
                                   shiny::textInput("cell", label = NULL, placeholder = "Enter the name of the cell you want to plot"),
                                   shiny::actionButton("cell_click", "Plot Cell", align = "center")
                                   ),


                 shiny::conditionalPanel( 'input.sidebarid == "ana_full"',
                                   shiny::textInput("peak_thresh_full", label = NULL, placeholder = "Peak Threshold"),
                                   shiny::textInput("rise_full", label = NULL, placeholder = "Frame before peak"),
                                   shiny::checkboxInput("groups", label = "Compare groups"),
                                   shiny::actionButton("ana_full_button", "Launch Full Analysis", align = "center")



                                   )



      ))
    ,



    shinydashboard::dashboardBody(

      shinydashboard::tabItems(

        shinydashboard::tabItem("des"),


        shinydashboard::tabItem("ana"),

        shinydashboard::tabItem("tuto"),

        shinydashboard::tabItem("prep",

      shiny::fluidRow(
        shinydashboard::box(title = "Dataset Prepared", width = 12, solidHeader = TRUE, status = "primary",
            DT::dataTableOutput("df_sql")))),


      shinydashboard::tabItem("viz",
      shiny::fluidRow(
        shinydashboard::box(title = "Plotting Cells", width = 6, solidHeader = TRUE, status = "primary",
            shiny::textInput("cell", label = NULL, placeholder = "Enter the name of the cell you want to plot"),
            shiny::actionButton("cell_click", "Plot Cell", align = "center"),
            shiny::plotOutput(outputId = "plot_cell")))),

      shinydashboard::tabItem("opt",

      shiny::fluidRow(
      shinydashboard::box(title = "Optimize Parameters", width = 6, solidHeader = TRUE, status = "primary",
            shiny::textInput("peak_thresh", label = "Peak Threshold", placeholder = "Enter the peak threshold you want to try (z score)"),
            shiny::textInput("rise_range", label = "Range", placeholder = "Enter the range taken to find a peak start (integer between 5 and 60)"),
            shiny::textInput("lambda", label = "Lambda", placeholder = "Enter the Lambda parameter for the Deconvolution (integer)"),
            shiny::textInput("gam", label = "Gam", placeholder = "Enter the Gam parameter for the Deconvolution (double between 0-1)"),
            shiny::textInput("n_cells", label = "Number of cells", placeholder = "Enter the number of cells you want to run the stimulation on"),

            shiny::checkboxInput("false_pos", label = "Estimate False Positives"),
            shiny::checkboxInput("show_peak", label = "Show Peaks"),

            shiny::uiOutput('responders'),
            shiny::uiOutput('non_responders'),


            shiny::actionButton("sim", "Simulate Analysis", align = "center"),
            shiny::actionButton("plot_responders", "Plot Responder", align = "center"),
            shiny::actionButton("plot_non_responders", "Plot Non Responder", align = "center")),



      shinydashboard::box(title = "Try other parameters on a given cell", width = 6, solidHeader = TRUE, status = "primary",
            shiny::textInput("cell_opt", label = "Cell", placeholder = "On which cell do you want to try new parameters ?"),

            shiny::textInput("peak_thresh_bis", label = "Threshold", placeholder = "Enter the peak threshold you want to try (z score)"),
            shiny::textInput("rise_range_bis", label = "Range", placeholder = "Enter the range taken to find a peak start (integer between 5 and 60)"),

            shiny::textInput("lambda_bis", label = "Lambda", placeholder = "Enter the Lambda parameter for the Deconvolution (integer)"),
            shiny::textInput("gam_bis", label = "Gam", placeholder = "Enter the Gam parameter for the Deconvolution (double between 0-1)"),

            shiny::checkboxInput("false_pos_bis", label = "Estimate False Positives"),
            shiny::checkboxInput("show_peak_bis", label = "Show Peaks"),


            shiny::actionButton("sim_bis", "Simulate Analysis", align = "center"),
            shiny::actionButton("plot_simulation_bis", "Plot Random Cells", align = "center"))),

      shiny::fluidRow(
      shinydashboard::box(title = "Plot Window", width = 6, solidHeader = TRUE, status = "primary",
                          shiny::plotOutput(outputId = "plot_cell_sim")),
      shinydashboard::box(title = "Plot Window", width = 6, solidHeader = TRUE, status = "primary",
                          shiny::plotOutput(outputId = "plot_cell_sim_bis")))),


      shinydashboard::tabItem("ana_full",

      shiny::fluidRow(
        shinydashboard::box(title = "Description of Responses", width = 12, solidHeader = TRUE, status = "primary",
            shiny::textOutput("resp_count"),
            DT::dataTableOutput("resp_group_stim"),
            shiny::textOutput("n_peaks"),
            DT::dataTableOutput("peaks_by_class"),
            DT::dataTableOutput("overall_q"),
            DT::dataTableOutput("post_hoc_mcnemar")
            )),

        shiny::fluidRow(
          shinydashboard::box(title = "Dual Proportions", width = 12, solidHeader = TRUE, status = "primary",
              shiny::textInput("stim1", label = NULL, placeholder = "Stimulus 1"),
              shiny::textInput("stim2", label = NULL, placeholder = "Stimulus 2"),
              shiny::actionButton("dual_button", "Compute Dual Proportions", align = "center" ),
              shiny::br(),
              shiny::br(),
              DT::dataTableOutput("dual_prop"),
        ))


        )
    )
    ))



server <- function(input, output){

sqlitePath <- getwd()


folder <- shiny::reactive({

  if(is.null(input$folder)) {return()}

  else{
    folder <- input$folder
    folder

  }

})


  stim_numb <- shiny::reactive({

    if(is.null(input$stim_num)) {return()}

    else{
      stim_numb <- as.numeric(input$stim_num)
      stim_numb

    }

  })

  df_final <- shiny::eventReactive(input$launch, {

      df <- CalQuick::prepareData(folder(), stim_numb(), 0.25, compare_groups = TRUE)

      CalQuick::saveData(df, "db_cq.sqlite", "df_full")

      df
    })




  output$df_sql <- DT::renderDataTable({

  if(is.null(df_final())) {return()}

  else{
  df <- CalQuick::loading100("db_cq.sqlite", "df_full")
  df <- DT::datatable(df)
  df
  }
  })


    df_plot <- shiny::eventReactive(input$cell_click, {

      df <- CalQuick::get_cell(input$cell, "db_cq.sqlite", "df_full")
      df
    })


    output$plot_cell <- shiny::renderPlot({

      p <- CalQuick::cell_plot_shiny(df_plot())
      p


  })



    res_sim <- shiny::reactiveValues(res = NULL)


    observeEvent(input$sim, {

      print("Simulation started")
      df_sub <- CalQuick::get_sub_df("db_cq.sqlite", "df_full", input$n_cells)

      res_sim$res <- downstream_analysis(df_sub, threshold = input$peak_thresh,
                                         borders_range = input$rise_range, lambda = input$lambda, gam = input$gam,
                                         false_pos = input$false_pos)

      data <- res_sim$res

      cells <- unique(data[[2]]$Cell_id)

      responders <- unique(data[[1]]$Cell_id)

      non_responders <- cells %in% responders

      non_responders <- unlist(purrr::map2(cells, non_responders, function(x,y) if(y == FALSE){x}))

      output$responders <- shiny::renderUI({

        shiny::selectInput(inputId = "responders", "Responders", responders)
      })

    output$non_responders <- shiny::renderUI({

      shiny::selectInput(inputId = "non_responders", "Non Responders", non_responders)
    })
})




    shiny::observeEvent(input$plot_responders, {

      data <- res_sim$res


      output$plot_cell_sim <- shiny::renderPlot({

        p <- cell_plot(data[[2]], data[[1]], var = "Mean_Grey", cell = input$responders, line = "gam", show_peak = input$show_peak)
        p


    })
    })

      shiny::observeEvent(input$plot_non_responders, {

        data <- res_sim$res

      output$plot_cell_sim <- shiny::renderPlot({

        p <- cell_plot(data[[2]], data[[1]], var = "Mean_Grey", cell = input$non_responders,
                       line = "gam", show_peak = input$show_peak)
        p


      })
    })

      shiny::observeEvent(input$sim_bis, {

        print("second stim started")
        df <- res_sim$res[[2]]

        print("df ok ")
        df_sub_bis <- df[df$Cell_id == input$cell_opt]

        print("df_sub ok")

        res_sim$res_bis <- downstream_analysis(df_sub_bis, threshold = input$peak_thresh_bis,
                                               borders_range = input$rise_range_bis, lambda = input$lambda_bis,
                                               gam = input$gam_bis, false_pos = input$false_pos_bis)

      })




      shiny::observeEvent(input$plot_simulation_bis, {

        data <- res_sim$res_bis

        output$plot_cell_sim_bis <- shiny::renderPlot({

          p <- cell_plot(data[[2]], data[[1]], var = "Mean_Grey", cell = input$cell_opt, line = "gam", show_peak = input$show_peak_bis)
          p


        })
  })


      ### Analyze the full dataset


      # This block makes the check box "compare groups" reactive
      shiny::observe({

        group_value <- input$groups
        shiny::updateCheckboxInput(shiny::getDefaultReactiveDomain(), "groups", value = group_value)
      })


      ### This block codes alows to launch the analysis when the button analyze dataset is clicked on. It runs the whole analysis, on the whole dataset



      res <- shiny::eventReactive(input$ana_full_button, {
        df_full <- CalQuick::get_full_df("db_cq.sqlite", "df_full")
        res <- CalQuick::downstream_analysis(df_full, 4,2,4,input$peak_thresh_full, input$rise_full,2, compare_groups = input$groups)

        # Extracting and saving the data table containing one row for each peak with the informations
        #about the peak
        res1 <- res[[2]]

        CalQuick::saveData(res1, "db_cq.sqlite", "peak_res")

        # Extracting and saving the full data table updated
        res2 <- res[[3]]
        CalQuick::saveData(res2, "db_cq.sqlite", "df_final")

        res
      })


      output$resp_count <- shiny::renderText({

        res <- res()

        res1 <- res[[1]][1][[1]]
        res1

      })


      output$resp_group_stim <- DT::renderDataTable({

        res <- res()

        res2 <- res[[1]][2][[1]]
        res2

      })

      output$n_peaks <- shiny::renderText({

        res <- res()

        res3 <- res[[1]][3][[1]]
        res3

      })

      output$peaks_by_class <- DT::renderDataTable({

        res <- res()

        res4 <- res[[1]][4][[1]]
        res4

      })


      output$overall_q <- DT::renderDataTable({

        res <- res()

        res4 <- res[[1]][5][[1]]
        res4

      })

      output$post_hoc_mcnemar <- DT::renderDataTable({

        res <- res()

        res4 <- res[[1]][6][[1]]
        res4

      })


      ### Computing dual proportions (proportion of cells responding to one stimulus that also responds to another stimulus)

      t <- shiny::eventReactive(input$dual_button, {

        res <- get_full_df("db_cq.sqlite", "peak_res")

        t <- dual_prop(res, input$stim1, input$stim2)

      })


      output$dual_prop <- DT::renderDataTable({
       t()
      })


}

shiny::shinyApp(ui, server)


}


