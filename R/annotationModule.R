
#' annotationModuleUI
#'
#' UI of the annotation module
#'
#' @param id
#'
#' @returns
#' @export
#'
#' @examples
annotationModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      shinydashboard::box(
        title = "Annotate cells by clicking on the graph",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        shiny::uiOutput(ns('cell_selector_bis')),
        shiny::uiOutput(ns('warning_bis')),
        shiny::selectInput(
          ns("displayType_bis"),
          "Type of data display",
          choices = list("points" = "markers", "line" = "line")
        ),
        plotly::plotlyOutput(ns("myPlot_bis"))
      ),

      shinydashboard::box(
        title = "View and Add annotation to project",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        shiny::actionButton(ns("viewPattern_bis"), "View Selected Pattern"),
        plotly::plotlyOutput(ns("pattern_bis")),
        shiny::actionButton(ns("saveAnnotation"), "Save Annotation"),
        shiny::actionButton(ns("newAnnotation"), "Start New Annotation Project"),
      )
    ))


}

#' annotationModuleServer
#'
#' server of the annotation module
#'
#' @param id
#'
#' @returns
#' @export
#'
#' @examples
annotationModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Code to store user clicks to define the patterns adapted from here :
    # https://stackoverflow.com/questions/56193127/plotly-click-events-from-anywhere-on-the-plot/58766072#58766072

    js_bis <- "
    function(el, x, inputName){
      var id = el.getAttribute('id');
      var gd = document.getElementById(id);
      var d3 = Plotly.d3;
      Plotly.update(id).then(attach);
        function attach() {
          gd.addEventListener('click', function(evt) {
            var xaxis = gd._fullLayout.xaxis;
            var yaxis = gd._fullLayout.yaxis;
            var bb = evt.target.getBoundingClientRect();
            var x = xaxis.p2d(evt.clientX - bb.left);
            var y = yaxis.p2d(evt.clientY - bb.top);
            var coordinates = [x, y];
            Shiny.setInputValue(inputName, coordinates);
          });
        };
  }
  "

    db_name_bis <- shiny::reactiveValues()

    shiny::observeEvent(input$start_creation_bis, {

      db_name_bis$name <- input$db_name_bis

    })


    df_full_bis <- shiny::eventReactive(input$start_creation_bis, {


      if(paste0(db_name_bis$name,".sqlite") %in% list.files(project$dir_path)){

        output$warning_db_bis <- NULL

        db_path_bis <- paste(paste(root_path, db_name_bis$name, sep = "/"),db_name_bis$name, sep = "/")

        df_full_bis <- calipR::get_full_df(paste0(db_path_bis, ".sqlite"), "df_full")

        output$cell_selector_bis <- shiny::renderUI({shiny::numericInput("cell_bis",
                                                                         label = "cell_number", value = 1, min = 1, max = length(df_full_bis$Cell_id))


        })

      }

      else{

        output$warning_db_bis <- shiny::renderUI({"This database doesn't exist"})
      }

      df_full_bis


    })




    new_DF_bis <- shiny::reactiveValues(data = data.frame(x = seq(1,10), y = seq(1,10)))

    shiny::observeEvent(input$start_creation_bis, {

      if(paste0(db_name_bis$name,".sqlite") %in% list.files(project$dir_path)){

        output$warning_db_bis <- NULL

        new_DF_bis$data <- df_full_bis()

        new_DF_bis$data <- data.frame(x = data.table::setDT(df_full_bis())[Cell_id == unique(df_full_bis()$Cell_id)[[1]]]$time_seconds,
                                      y = data.table::setDT(df_full_bis())[Cell_id == unique(df_full_bis()$Cell_id)[[1]]]$Mean_Grey )

      }
      else{
        output$warning_db_bis <- shiny::renderUI({"This database doesn't exist"})
      }
    })

    coordinates <- list()

    observeEvent(input$clickposition_bis, {
      new_DF_bis$data <- rbind(new_DF_bis$data, input$clickposition_bis)

      plotly::plotlyProxyInvoke(myPlotProxy_bis, "restyle", list(x = list(new_DF_bis$data[['x']]), y = list(new_DF_bis$data[['y']])))

    })

    pattern_list <- reactiveValues('1' =  c(1,1,1))


    pattern_viewer_bis <- reactiveValues('1' = c(0,0,0))

    observeEvent(input$viewPattern_bis, {
      start <- as.integer(new_DF_bis$data[['x']][length(new_DF_bis$data[['x']]) -1])
      end <-  as.integer(new_DF_bis$data[['x']][length(new_DF_bis$data[['x']])])

      pattern <- new_DF_bis$data[['y']][start:end]
      whole_trace <- new_DF_bis$data[['y']]

      whole_trace[c(1:start,end:length(new_DF_bis$data[['x']]))] <- min(pattern, na.rm = TRUE)

      pattern_viewer_bis[['1']] <- whole_trace

    })



    observeEvent(input$saveAnnotation, {

      db_path <- paste(paste(root_path, db_name_bis$name, sep = "/"),db_name_bis$name, sep = "/")

      start <- as.integer(new_DF_bis$data[['x']][length(new_DF_bis$data[['x']]) -1])
      end <-  as.integer(new_DF_bis$data[['x']][length(new_DF_bis$data[['x']])])

      pattern <- new_DF_bis$data[['y']][start:end]
      whole_trace <- new_DF_bis$data[['y']]

      whole_trace[c(1:start,end:length(new_DF_bis$data[['x']]))] <- min(pattern, na.rm = TRUE)

      pattern_viewer[['1']] <- whole_trace

      d <- setDT(df_full_bis())[Cell_id == unique(df_full_bis()$Cell_id)[[input$cell_bis]]]

      d[, Annotation := ifelse(time_frame %between% c(start,end),1,0)]

      saveData(d, paste(project$dir_path,project$db_file, sep ="/"), input$annotation_project,
               over= FALSE,
               append = TRUE)


    })




    output$myPlot_bis <- plotly::renderPlotly({

      plotly::plot_ly(new_DF_bis$data, x = ~x, y = ~y, type = "scatter", mode = input$displayType_bis) %>%
        htmlwidgets::onRender(js_bis, data = "clickposition_bis")
    })

    output$pattern_bis <- plotly::renderPlotly({
      end <-  as.integer(new_DF_bis$data[['x']][length(new_DF_bis$data[['x']])])
      pattern_len <- length(pattern_viewer_bis[['1']])
      to_add <- end - pattern_len
      fluo <- pattern_viewer_bis[['1']]
      time <- seq(1,length(fluo))
      df <- as.data.frame(y = fluo, x = time)
      plotly::plot_ly(df, x = ~time, y = ~fluo, type = "scatter", mode = "line")
    })


    myPlotProxy_bis <- plotly::plotlyProxy("myPlot_bis", session)


    observeEvent(input$cell_bis,{

      if(input$cell_bis > length(unique(df_full_bis()$Cell_id)) | input$cell_bis == 0 | is.na(input$cell_bis)){
        output$warning_bis <- shiny::renderUI({paste(paste("There are", length(unique(df_full_bis()$Cell_id)),"cells.
                                    Please enter a valid number"))
        })
      }
      else{

        output$warning_bis <- NULL
        new_DF_bis$data <- data.frame(x = data.table::setDT(df_full_bis())[Cell_id == unique(df_full_bis()$Cell_id)[[input$cell_bis]]]$time_seconds,
                                      y = data.table::setDT(df_full_bis())[Cell_id == unique(df_full_bis()$Cell_id)[[input$cell_bis]]]$Mean_Grey )
      }

    })

  })
}
