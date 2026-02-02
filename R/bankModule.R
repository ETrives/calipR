
#' bankModuleUI
#'
#' UI of the bank creation module
#'
#' @param id
#'
#' @returns
#' @export
#'
#' @examples
bankModuleUI <- function(id) {
  ns <- NS(id)
  tagList(shiny::fluidRow(
    shinydashboard::box(
      title = "Select Patterns by clicking on the graph",
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      shiny::uiOutput(ns('cell_selector')),
      shiny::uiOutput(ns('warning'), class = "warn-main"),
      shiny::selectInput(
        ns("displayType"),
        "Type of data display",
        choices = list("points" = "markers", "line" = "line")
      ),
      plotly::plotlyOutput(ns("myPlot"))
    ),

    shinydashboard::box(
      title = "View and Add Patterns to Bank",
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      shiny::actionButton(ns("viewPattern"), "View Selected Pattern"),
      shiny::actionButton(ns("submitPattern"), "Submit Pattern to Bank"),
      plotly::plotlyOutput(ns("pattern")),
      shiny::actionButton(ns("printList"), "Print Bank"),
      shiny::actionButton(ns("saveNewBank"), "Save Bank"),
      shiny::actionButton(ns("start_newbank"), "Start New Bank")
    )
  ))


}

#' bankModuleServer
#'
#' Server of the bank creation module
#'
#' @param id
#'
#' @returns
#' @export
#'
#' @examples
bankModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Code to store user clicks to define the patterns adapted from here :
    # https://stackoverflow.com/questions/56193127/plotly-click-events-from-anywhere-on-the-plot/58766072#58766072

    js <- "
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

    db_name <- shiny::reactiveValues()

    shiny::observeEvent(input$start_creation, {

      db_name$name <- input$db_name

    })


    df_full <- shiny::eventReactive(input$start_creation, {


      if(paste0(db_name$name,".sqlite") %in% list.files(project$dir_path)){

        output$warning_db <- NULL

        db_path <- paste(paste(root_path, db_name$name, sep = "/"),db_name$name, sep = "/")

        df_full <- calipR::get_full_df(paste0(db_path, ".sqlite"), "df_full")


        output$cell_selector <- shiny::renderUI({shiny::numericInput("cell",
                                                                     label = "cell_number", value = 1, min = 1, max = length(df_full$Cell_id))


        })

      }

      else{

        output$warning_db <- shiny::renderUI({"This database doesn't exist"})
      }

      df_full


    })




    new_DF <- shiny::reactiveValues(data = data.frame(x = seq(1,10), y = seq(1,10)))

    shiny::observeEvent(input$start_creation, {

      if(paste0(db_name$name,".sqlite") %in% list.files(project$dir_path)){

        output$warning_db <- NULL

        new_DF$data <- df_full()

        new_DF$data <- data.frame(x = data.table::setDT(df_full())[Cell_id == unique(df_full()$Cell_id)[[1]]]$time_seconds,
                                  y = data.table::setDT(df_full())[Cell_id == unique(df_full()$Cell_id)[[1]]]$Mean_Grey )

      }
      else{
        output$warning_db <- shiny::renderUI({"This database doesn't exist"})
      }
    })

    coordinates <- list()

    observeEvent(input$clickposition, {
      new_DF$data <- rbind(new_DF$data, input$clickposition)

      plotly::plotlyProxyInvoke(myPlotProxy, "restyle", list(x = list(new_DF$data[['x']]), y = list(new_DF$data[['y']])))

    })

    pattern_list <- reactiveValues('1' =  c(1,1,1))


    observeEvent(input$saveNewBank, {

      final_bank <- reactiveValuesToList(pattern_list)
      saveRDS(final_bank, file = paste(project$dir_path,input$bankName, sep = "/"))

    })

    counter <- reactiveValues(value = 0)

    observeEvent(input$submitPattern, {
      counter$value <- counter$value + 1
      start <- as.integer(new_DF$data[['x']][length(new_DF$data[['x']]) -1])
      end <-  as.integer(new_DF$data[['x']][length(new_DF$data[['x']])])

      pattern <- new_DF$data[['y']][start:end]
      pattern_list[[as.character(counter$value)]] <- pattern


    })

    observeEvent(input$start_newbank, {

      for (i in seq(1,counter$value)){

        pattern_list[[as.character(i)]] <- NULL

      }

      counter$value <- 0

    })

    pattern_viewer <- reactiveValues('1' = c(0,0,0))

    observeEvent(input$viewPattern, {
      start <- as.integer(new_DF$data[['x']][length(new_DF$data[['x']]) -1])
      end <-  as.integer(new_DF$data[['x']][length(new_DF$data[['x']])])

      pattern <- new_DF$data[['y']][start:end]
      whole_trace <- new_DF$data[['y']]

      whole_trace[c(1:start,end:length(new_DF$data[['x']]))] <- min(pattern, na.rm = TRUE)

      pattern_viewer[['1']] <- whole_trace

    })

    observeEvent(input$printList,{
      test <- reactiveValuesToList(pattern_list)
    })



    output$myPlot <- plotly::renderPlotly({

      plotly::plot_ly(new_DF$data, x = ~x, y = ~y, type = "scatter", mode = input$displayType) %>%
        htmlwidgets::onRender(js, data = "clickposition")
    })

    output$pattern <- plotly::renderPlotly({
      end <-  as.integer(new_DF$data[['x']][length(new_DF$data[['x']])])
      pattern_len <- length(pattern_viewer[['1']])
      to_add <- end - pattern_len
      fluo <- pattern_viewer[['1']]
      time <- seq(1,length(fluo))
      df <- as.data.frame(y = fluo, x = time)
      plotly::plot_ly(df, x = ~time, y = ~fluo, type = "scatter", mode = "line")
    })


    myPlotProxy <- plotly::plotlyProxy("myPlot", session)


    observeEvent(input$cell,{

      if(input$cell > length(unique(df_full()$Cell_id)) | input$cell == 0 | is.na(input$cell)){
        output$warning <- shiny::renderUI({paste(paste("There are", length(unique(df_full()$Cell_id)),"cells.
                                    Please enter a valid number"))
        })
      }
      else{

        output$warning <- NULL
        new_DF$data <- data.frame(x = data.table::setDT(df_full())[Cell_id == unique(df_full()$Cell_id)[[input$cell]]]$time_seconds,
                                  y = data.table::setDT(df_full())[Cell_id == unique(df_full()$Cell_id)[[input$cell]]]$Mean_Grey )
      }

    })


  })
}
