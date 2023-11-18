library(plotly)
library(shiny)
library(htmlwidgets)

cells <- unique(d$Cell_id)


DF <- data.frame(x = d[Cell_id == "1aaa"]$time_frame, y = d[Cell_id == "1aaa"]$Mean_Grey)

ui <- fluidPage(
  numericInput("cell",label = "cell_number", value = 1),
  actionButton("submitPattern", "Submit Pattern"),
  actionButton("printList", "Print List"),

  plotlyOutput("myPlot"),
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

  pattern_list <- reactiveValues()

  observeEvent(input$submitPattern, {
    start <- as.integer(new_DF()$x[length(new_DF()$x) -1])
    end <-  as.integer(new_DF()$x[length(new_DF()$x)])

    pattern <- new_DF()$y[start:end]
    print(pattern)
    pattern_list[[as.character(input$cell)]] <- pattern

  })

  observeEvent(input$printList,{
  test <- reactiveValuesToList(pattern_list)
  print("test")
  print(test)
})

  output$myPlot <- renderPlotly({
    plot_ly(initDF, x = ~x, y = ~y, type = "scatter", mode = "markers") %>%
      onRender(js, data = "clickposition")
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
