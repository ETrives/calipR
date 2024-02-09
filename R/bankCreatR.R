library(plotly)
library(shiny)
library(htmlwidgets)

cells <- unique(d$Cell_id)


#DF <- data.frame(x = d[Cell_id == "1aaa"]$time_frame, y = d[Cell_id == "1aaa"]$Mean_Grey)

bankCreatR <- function(db_name) {

ui <- fluidPage(
  textInput("bankName", "How do you want to call this bank ?"),


  actionButton("viewPattern", "View Selected Pattern"),
  actionButton("submitPattern", "Submit Pattern to Bank"),

  actionButton("printList", "Print Bank"),
  actionButton("saveNewBank", "Save Bank"),

  numericInput("cell",label = "cell_number", value = 1),
  selectInput("displayType", "Type of data display", choices = list("points" = "markers", "line" = "line",
                                                                    "both" = "lines+markers"),selected = "points" ),
  plotlyOutput("myPlot"),
  plotlyOutput("pattern"),

  verbatimTextOutput("click")
)

server <- function(input, output, session) {

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



  df_full <- calipR::get_full_df(paste0(db_name, ".sqlite"), "df_full")
  print(df_full)
  cells <- unique(df_full$Cell_id)


  new_DF <- reactiveVal(df_full)

  #clickposition_history <- reactiveVal(df_full)


  #clickposition_history <- reactiveVal(initDF)

  coordinates <- list()

  observeEvent(input$clickposition, {
    new_DF(rbind(new_DF(), input$clickposition))



  })

  pattern_list <- reactiveValues('1' =  c(1,1,1))
  observeEvent(input$saveNewBank, {

  final_bank <- reactiveValuesToList(pattern_list)
  saveRDS(final_bank, file = paste(input$bankName))

  })

  counter <- reactiveValues(value = 0)

  test <- reactiveValues(value = "empty")



  observeEvent(input$submitPattern, {
    counter$value <- counter$value + 1
    start <- as.integer(new_DF()$x[length(new_DF()$x) -1])
    end <-  as.integer(new_DF()$x[length(new_DF()$x)])

    pattern <- new_DF()$y[start:end]
    print(pattern)
    pattern_list[[as.character(counter$value)]] <- pattern

    test$value <- new_DF()

    print("test")
    print(test$value[['x']])

    })

  pattern_viewer <- reactiveValues('1' = c(0,0,0))

  observeEvent(input$viewPattern, {
    start <- as.integer(new_DF()$x[length(new_DF()$x) -1])
    end <-  as.integer(new_DF()$x[length(new_DF()$x)])

    pattern <- new_DF()$y[start:end]
    whole_trace <- new_DF()$y

    whole_trace[c(1:start,end:length(new_DF()$x))] <- min(pattern, na.rm = TRUE)

    pattern_viewer[['1']] <- whole_trace

  })

  observeEvent(input$printList,{
  test <- reactiveValuesToList(pattern_list)

})



  output$myPlot <- renderPlotly({
    print("new_DF")
    print(new_DF())
    plot_ly(new_DF(), x = ~x, y = ~y, type = "scatter", mode = input$displayType) %>%
      onRender(js, data = "clickposition")
  })

  output$pattern <- renderPlotly({
    end <-  as.integer(new_DF()$x[length(new_DF()$x)])
    pattern_len <- length(pattern_viewer[['1']])
    to_add <- end - pattern_len
    fluo <- pattern_viewer[['1']]
    time <- seq(1,length(fluo))
    df <- as.data.frame(y = fluo, x = time)
    plot_ly(df, x = ~time, y = ~fluo, type = "scatter", mode = "line")
  })

  observeEvent(input$cell, {
    new_DF(data.frame(x = data.table::setDT(df_full)[Cell_id == cells[[input$cell]]]$time_frame,
                      y = data.table::setDT(df_full)[Cell_id == cells[[input$cell]]]$Mean_Grey ))
  })

  myPlotProxy <- plotlyProxy("myPlot", session)

  observe({
    plotlyProxyInvoke(myPlotProxy, "restyle", list(x = list(new_DF()$x), y = list(new_DF()$y)))
  })

  output$click <- renderPrint({
    #clickposition_history()
    new_DF()
  })
}

shinyApp(ui, server)
}

bank <- readRDS("bank_test")
