library(plotly)
library(shiny)
library(htmlwidgets)

cells <- unique(d$Cell_id)


#DF <- data.frame(x = d[Cell_id == "1aaa"]$time_frame, y = d[Cell_id == "1aaa"]$Mean_Grey)

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




  new_DF <- reactiveVal(DF)

  #clickposition_history <- reactiveVal(DF)


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

  observeEvent(input$submitPattern, {
    counter$value <- counter$value + 1
    start <- as.integer(new_DF()$x[length(new_DF()$x) -1])
    end <-  as.integer(new_DF()$x[length(new_DF()$x)])

    pattern <- new_DF()$y[start:end]
    print(pattern)
    pattern_list[[as.character(counter$value)]] <- pattern


    })

  pattern_viewer <- reactiveValues('1' = c(0,0,0))

  observeEvent(input$viewPattern, {
    start <- as.integer(new_DF()$x[length(new_DF()$x) -1])
    end <-  as.integer(new_DF()$x[length(new_DF()$x)])

    pattern <- new_DF()$y[start:end]
    pattern_viewer[['1']] <- pattern

  })

  observeEvent(input$printList,{
  test <- reactiveValuesToList(pattern_list)
  print("test")
  print(test)
})

  output$myPlot <- renderPlotly({
    plot_ly(initDF, x = ~x, y = ~y, type = "scatter", mode = input$displayType) %>%
      onRender(js, data = "clickposition")
  })

  output$pattern <- renderPlotly({
    fluo <-  c(pattern_viewer[['1']], rep(0, 100))
    print(fluo)
    plot_ly(as.data.frame(y = fluo, x = seq(1,length(fluo))), x = ~x, y = ~fluo, type = "scatter", mode = "line")
  })

  observeEvent(input$cell, {
    new_DF(data.frame(x = d[Cell_id == cells[[input$cell]]]$time_frame,
                      y = d[Cell_id == cells[[input$cell]]]$Mean_Grey ))
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


bank <- readRDS("bank_test")
