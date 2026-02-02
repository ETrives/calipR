
#' vizResModuleUI
#'
#' UI of the result visualization module
#'
#' @param id
#'
#' @returns
#' @export
#'
#' @examples
vizResModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      shinydashboard::box(
        title = "Plotting Results",
        width = 6,
        solidHeader = TRUE,
        status = "primary",
        shiny::uiOutput(ns("var_selector")),
        plotly::plotlyOutput(outputId = ns("viz"))
      ),
      shinydashboard::box(
        title = "Plotting Cluster Results",
        width = 6,
        solidHeader = TRUE,
        status = "primary",
        shiny::uiOutput(ns("var_selector_clust")),
        plotly::plotlyOutput(outputId = ns("viz_clust_auc"))
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        title = "Plotting Cells",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        shiny::uiOutput(ns('resp_viz')),
        shiny::uiOutput(ns('non_resp_viz')),
        shiny::actionButton(ns("plot_button"), "Plot Responder", align = "right"),
        shiny::actionButton(ns("plot_button_bis"), "Plot Non Responder", align = "right"),
        shiny::checkboxInput(ns("show_peaks_box"), label = "Show Peaks"),
        shiny::plotOutput(outputId = ns("plot_resp_viz"))
      ),
      shiny::div(style = "height:1000px;")
    )
  )
}

#' vizResModuleServer
#'
#' Server of the result visualization module
#'
#' @param id
#'
#' @returns
#' @export
#'
#' @examples
vizResModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$var_selector <- shiny::renderUI({

      list(
        shiny::selectInput(inputId = "x_var", label = NULL, names(result$resp_desc)),
        shiny::selectInput(inputId = "y_var", label = NULL, names(result$resp_desc)),
        shiny::selectInput(inputId = "z_var", label = NULL, names(result$resp_desc))
      )
    })


    output$viz <- plotly::renderPlotly({


      plotly::plot_ly(

        type = 'bar',

        x = result$resp_desc[[input$x_var]],

        y = result$resp_desc[[input$y_var]],
        text = paste("Group: ", result$resp_desc[["group"]],

                     "<br>Stimulus:  ", res()[[1]][["stimulus"]],

                     "<br>Responders: ", res()[[1]][["Responses"]],

                     "<br>Proportion: ", res()[[1]][["Prop"]],
                     "<br> Total cells: ", res()[[1]][["n_cells_grp"]]),

        hoverinfo = 'text',

        marker = list(size = 2),


        color = res()[[1]][[input$z_var]],

      )  %>%
        plotly::layout(barmode ="group", yaxis = list(automargin = TRUE),
                       xaxis = list(automargin = TRUE), bargap = -2, bargroupgap = 0)





    })


    output$resp_viz <- shiny::renderUI({

      if(length(result$full) == 0) {
      }
      else{

        responders <- unique(result$peaks[["Cell_id"]])
        shiny::selectInput(inputId = "resp_viz", "Responders", responders)
      }
    })




    output$non_resp_viz <- shiny::renderUI({
      if(length(result$full) == 0) {
      }
      else{

        cells <- unique(result$full[["Cell_id"]])
        responders <- unique(result$peaks[["Cell_id"]])
        non_responders <- cells %in% responders
        non_responders <- unlist(purrr::map2(cells, non_responders, function(x,y) if(y == FALSE){x}))
        shiny::selectInput(inputId = "non_resp_viz", "Non Responders", non_responders)
      }
    })




    shiny::observeEvent(input$plot_button,{

      output$plot_resp_viz <- renderPlot({

        cnames <- colnames(result$full)
        back_estim_opt <- c("gam_fit", "background")

        cnames_check <- back_estim_opt %in% cnames
        back_var <- back_estim_opt[[which(cnames_check == TRUE)]]


        p <- cell_plot(result$full, result$peaks, var = "Mean_Grey", cell = input$resp_viz, line = back_var, show_peak = input$show_peaks_box)
        p


      })
    })


    shiny::observeEvent(input$plot_button_bis,{



      output$plot_resp_viz <- renderPlot({

        cnames <- colnames(result$full)
        back_estim_opt <- c("gam_fit", "background")

        cnames_check <- back_estim_opt %in% cnames
        back_var <- back_estim_opt[[which(cnames_check == TRUE)]]

        p <- cell_plot(result$full, result$peaks, var = "Mean_Grey", cell = input$non_resp_viz, line = back_var, show_peak = input$show_peaks_box)
        p



      })


    })



    ### Exporting results

    output$export_csv <- shiny::renderUI({

      list(
        shiny::textInput("fName", "File name"),
        shiny::actionButton("exportPeakResults", "Export Data", align = "right")
      )

    })

    observeEvent(input$exportPeakResults, {
      write.csv(result$peaks, paste(root_path, paste(project$name, paste0(input$fName, ".csv"),sep = "/"),sep="/"))

    })

  })
}
