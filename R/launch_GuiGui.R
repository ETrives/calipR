
#' launch_GuiGui
#'
#' Exectue this function without any argument to launch the Graphical User Interface (GUI)
#'
#'
#' @return
#' @export
#'
#' @examples
  launch_GuiGui_bis <- function(){



ui <-
  shinydashboard::dashboardPage(skin = "blue",


    calipR::dashboardHeader(disable = TRUE),


    shinydashboard::dashboardSidebar(

      shiny::tags$head(
        shiny::tags$style(shiny::HTML(".sidebar {
                      height: 100%;
                    }"
        )
        )
      ),

      shiny::img(src = "logo/calipR_logo.png", width = "70%", height = "70%",
      style = "position: relative; top: -30px; left: 35px;"),

      #shiny::tags$style(".left-side, .main-sidebar {padding-top: 180px}"),
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
                  shinydashboard::menuSubItem("Visualize Results", tabName = "viz_res"),



                  shiny::conditionalPanel( 'input.sidebarid == "prep"',
                                   shiny::selectInput("stim_num", label ="Stimuli number (/cell)", c("1" = "1", "2" = "2", "3" = "3",
                                                                                                                     "4" = "4", "5" = "5", "6"="6", "7"="7",
                                                                                                                      "8"="8", "9"="9","10"="10")),
                                   shiny::textInput("frame_rate", label = "Enter your frame rate (Hz)", placeholder = "e.g. 0.5" ),
                                   shiny::textInput("folder", label = NULL, placeholder = "Write folder's name (where all the files are)"),
                                   shiny::textInput("mark_thresh", label = "if you have a cellular marker, enter your threshold", placeholder = "e.g. 30"),
                                   shiny::checkboxInput("trackbox", label = "Check if you did ROI detection with Trackmate"),

                                   shiny::actionButton("launch", "Load & Tidy Data", align = "center")),

                 shiny::conditionalPanel( 'input.sidebarid == "viz"',
                                   ),


                 shiny::conditionalPanel( 'input.sidebarid == "ana_full"',
                                   shiny::textInput("peak_thresh_full", label = "Peak Threshold", placeholder = "Peak Threshold"),
                                   shiny::textInput("rise_full", label = "Borders Range", placeholder = "Frame before peak"),
                                   shiny::textInput("lambda_full", label = "Lambda"),
                                   shiny::textInput("gam_full", label = "gam"),
                                   shiny::checkboxInput("false_pos_full", label = "False Positives Estimation"),
                                   shiny::checkboxInput("patMatch", label = "Background Estimation with Pattern Matching"),
                                   shiny::uiOutput('posBank_field_full'),
                                   shiny::uiOutput('negBank_field_full'),
                                   shiny::uiOutput('window_field_full'),


                                   shiny::checkboxInput("groups", label = "Compare groups"),
                                   shiny::actionButton("ana_full_button", "Launch Full Analysis", align = "center"))

                            )),



    shinydashboard::dashboardBody(


      shiny::tags$head(shiny::tags$style(shiny::HTML("

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #172330;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #5499c7
                              }

                                    "))),

      shinydashboard::tabItems(

        shinydashboard::tabItem("des",

        shiny::fluidRow(

          shiny::img(src = "logo/calipR_logo.png",
                     style = "border: 1px;
    color: white;
    position: absolute;
    top : 100px;
    left: 300px;
    right: 0;
    margin: 0 auto;
    max-width: 250px;"), shiny::div(style = "height:1000px;")),


        shiny::fluidRow(

        shiny::HTML( "<center> <p style = 'font-size: 20px; word-wrap: break-word;
        width: 1000px; align: justify; color: black; padding-top: 50px;
    position: absolute;
    top: 400px;
    left: 250px;
    right: 0;
    margin: 0 auto;
    max-width: 600px;'> <b> calipR </b> is an open source software designed to facilitate calcium imaging data analysis.
                      It is fully written in the R programming language.
                      With this Graphical User Interface (GUI) you can analyze your data
                      without any programming skills.<br> <br>
                      Two workflows are available: one that allows you to automatize
                     ROIs detection with the Trackmate imageJ plugin and another
                     one that takes the output from manual ROI detection in ImageJ.
                     To start with your first analysis you can go on to the Tutorial page.</p> </center>"),


        ),
        shiny::fluidRow(shiny::tags$br()),
        shiny::fluidRow(shiny::tags$br()),
        shiny::fluidRow(shiny::tags$br()),

        ),



        shinydashboard::tabItem("ana", shiny::fluidRow(shiny::div(style = "height:1000px;"))),

        shinydashboard::tabItem("tuto", shiny::fluidRow(shiny::div(style = "height:1000px;"))),

        shinydashboard::tabItem("prep",

        shiny::tags$style(shiny::HTML("


.box.box-solid.box-primary>.box-header {
  color:#fff;
  background:#5499c7
                    }

.box.box-solid.box-primary{
border-bottom-color:#5499c7  ;
border-left-color:#5499c7  ;
border-right-color:#5499c7  ;
border-top-color:#5499c7  ;
}

.box.box-primary>.box-header {
  color:#000000;
  background:#fff
                    }

.box.box-primary{
border-bottom-color:#5499c7  ;
border-left-color:#5499c7  ;
border-right-color:#5499c7  ;
border-top-color:#5499c7  ;
}

                                    ")),

      shiny::fluidRow(class = "myRow1",
                 shiny::column(12,
        shinydashboard::box(title = "Dataset Prepared", width = 12, solidHeader = TRUE, status = "primary", collapsible = T,
            DT::dataTableOutput("df_sql")),

        shiny::textInput("db_name", label = NULL, placeholder = "Enter the name of the database you want to use to create your bank"),
        shiny::actionButton("start_creation", "Create Bank", align = "center"),
        textInput("bankName", "How do you want to call this bank ?"),


        actionButton("viewPattern", "View Selected Pattern"),
        actionButton("submitPattern", "Submit Pattern to Bank"),

        actionButton("printList", "Print Bank"),
        actionButton("saveNewBank", "Save Bank"),
        actionButton("start_newbank", "Start New Bank"),

        numericInput("cell",label = "cell_number", value = 1),
        selectInput("displayType", "Type of data display", choices = list("points" = "markers", "line" = "line",
                                                                          "both" = "lines+markers"),selected = "points" ),
        plotlyOutput("myPlot"),
        plotlyOutput("pattern"),

        verbatimTextOutput("click"),

        shiny::div(style = "height:1000px;")))),


      shinydashboard::tabItem("viz",
      shiny::fluidRow(
        shinydashboard::box(title = "Plotting Cells", width = 6, solidHeader = TRUE, status = "primary",
            shiny::textInput("cell_viz", label = NULL, placeholder = "Enter the name of the cell you want to plot"),
            shiny::actionButton("cell_click", "Plot Cell", align = "center"),
            shiny::plotOutput(outputId = "plot_cell")), shiny::div(style = "height:1000px"))),

      shinydashboard::tabItem("opt",

      shiny::fluidRow(
      shinydashboard::box(title = "Optimize Parameters", width = 6, solidHeader = TRUE, status = "primary",
            shiny::textInput("peak_thresh", label = "Peak Threshold", placeholder = "Enter the peak threshold you want to try (z score)"),
            shiny::textInput("rise_range", label = "Range", placeholder = "Enter the range taken to find a peak start (integer between 5 and 60)"),
            shiny::textInput("lambda", label = "Lambda", placeholder = "Enter the Lambda parameter for the Deconvolution (integer)"),
            shiny::textInput("gam", label = "Gam", placeholder = "Enter the Gam parameter for the Deconvolution (double between 0-1)"),
            shiny::textInput("n_cells", label = "Number of cells", placeholder = "Enter the number of cells you want to run the stimulation on"),

            shiny::checkboxInput("false_pos", label = "Estimate False Positives"),
            shiny::checkboxInput("patMatch_opt", label = "Background Estimation with Pattern Matchhing"),

            shiny::uiOutput('posBank_field'),
            shiny::uiOutput('negBank_field'),
            shiny::uiOutput('window_field'),





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
            shiny::checkboxInput("patMatch_opt_bis", label = "Background Estimation with Pattern Matchhing"),

            shiny::uiOutput('posBank_field_bis'),
            shiny::uiOutput('negBank_field_bis'),
            shiny::uiOutput('window_field_bis'),

            shiny::checkboxInput("show_peak_bis", label = "Show Peaks"),


            shiny::actionButton("sim_bis", "Simulate Analysis", align = "center"),
            shiny::actionButton("plot_simulation_bis", "Plot Cell", align = "center"))),

      shiny::fluidRow(
      shinydashboard::box(title = "Plot Window", width = 6, solidHeader = TRUE, status = "primary",
                          shiny::selectInput(inputId = "cell_plot_var", label = NULL,
                                             list("Mean_Grey" = "Mean_Grey", "Delta_F/F" = "delta_f_f", "z_score" = "z_score",
                                                  "Smooth_Delta_F/F" = "smooth_delta", "First_Derivative" = "first_derivative",
                                                  "Smooth_First_Derivative" = "smooth_Diff", "Deconvolved_trace" = "deconvolved_trace")),
                          shiny::plotOutput(outputId = "plot_cell_sim")),
      shinydashboard::box(title = "Plot Window", width = 6, solidHeader = TRUE, status = "primary",
                          shiny::plotOutput(outputId = "plot_cell_sim_bis"))),

      shiny::fluidRow(
        shinydashboard::box(title = "Statistics", width = 6, solidHeader = TRUE, status = "primary",
                            DT::dataTableOutput("stats_opt")))),


      shinydashboard::tabItem("ana_full",

      shiny::fluidRow(
        shinydashboard::box(title = "Description of Responses", width = 12, solidHeader = TRUE, status = "primary",
            shinycssloaders::withSpinner(DT::dataTableOutput("resp_count"), type = 6),

            DT::dataTableOutput("resp_group_stim"),
            DT::dataTableOutput("peaks_by_class"),
            DT::dataTableOutput("overall_q"),
            DT::dataTableOutput("post_hoc_mcnemar"),
            shiny::selectInput(inputId = "grouping_var", label = NULL,
                               list("Group" = "group", "Coverslip" = "coverslip", "Stimulus" = "stimulus", "Marker" = "marker_positive"),
                               multiple = TRUE),
            shiny::actionButton("update_button", "Update", align = "right"),
            shiny::checkboxInput("base_resp", label = "Remove Baseline Responders"),



            )),

        shiny::fluidRow(
          shinydashboard::box(title = "Dual Proportions", width = 12, solidHeader = TRUE, status = "primary",
              shiny::uiOutput("stim_list_1"),
              shiny::uiOutput("stim_list_2"),

              shiny::actionButton("dual_button", "Compute Dual Proportions", align = "center" ),
              shiny::br(),
              shiny::checkboxInput("base_resp_dual", label = "Remove Baseline Responders"),

              shiny::br(),
              DT::dataTableOutput("dual_prop")),shiny::div(style = "height:1000px;")),


      ### Clustering module :

      shiny::fluidRow(
        shinydashboard::box(title = "Clustering", width = 12, solidHeader = TRUE, status = "primary",
                            shiny::uiOutput('clustering_var'),
                            shiny::checkboxInput("normclust", label = "Z Normalize Values"),
                            shiny::textInput("nclust", "Number of Clusters"),
                            shiny::checkboxInput("set_seed", "Set Seed"),
                            shiny::uiOutput('seed_field'),

                            shiny::selectInput("dist_type", "Distance Measure to Use",
                                               list("Euclidean" = "Euclidean",
                                                    "Dynamic Time Warping" = "dtw",
                                                    "Global Alignment Kernels" = "gak",
                                                    "Manhattan" = "Manhattan",
                                                    "Shape Based Distance" = "sbd")),
                            shiny::actionButton("clustplot_button", "Launch Clustering", align = "right"),

                            shiny::plotOutput(outputId = "clustplot")),shiny::div(style = "height:1000px;"))),




      shinydashboard::tabItem("viz_res",
                              shiny::fluidRow(
                                shinydashboard::box(title = "Plotting Results", width = 12, solidHeader = TRUE, status = "primary",
                                                    shiny::selectInput(inputId = "x", label = NULL, list("Group" = "group", "Coverslip" = "coverslip", "Stimulus" = "stimulus", "Marker" = "marker_positive")),
                                                    shiny::selectInput(inputId = "y", label = NULL, list("Proportion Responders" = "Prop_marker_resp", "Responses" = "Responses", "Proportion Totale" = "Prop_tot", "Proportion Marker" = "Prop_marker" )),
                                                    shiny::selectInput(inputId = "z", label = NULL, list("Group" = "group", "Coverslip" = "coverslip", "Stimulus" = "stimulus", "Marker" = "marker_positive")),
                                                    plotly::plotlyOutput(outputId = "viz")),shiny::div(style = "height:1000px;")),

                              shiny::fluidRow(
                                shinydashboard::box(title = "Plotting Cells", width = 12, solidHeader = TRUE, status = "primary",
                                                    shiny::selectInput(inputId = "x", label = NULL, list("Group" = "group", "Coverslip" = "coverslip", "Stimulus" = "stimulus", "Marker" = "marker_positive")),
                                                    shiny::selectInput(inputId = "y", label = NULL, list("Proportion Responders" = "Prop_marker_resp", "Responses" = "Responses", "Proportion Totale" = "Prop_tot", "Proportion Marker" = "Prop_marker" )),
                                                    shiny::selectInput(inputId = "z", label = NULL, list("Group" = "group", "Coverslip" = "coverslip", "Stimulus" = "stimulus", "Marker" = "marker_positive")),

                                                    shiny::uiOutput('resp_viz'),
                                                    shiny::uiOutput('non_resp_viz'),
                                                    shiny::actionButton("plot_button", "Plot Responder", align = "right"),
                                                    shiny::actionButton("plot_button_bis", "Plot Non Responder", align = "right"),
                                                    shiny::checkboxInput("show_peaks_box", label = "Show Peaks"),




                                                    shiny::plotOutput(outputId = "plot_resp_viz")),shiny::div(style = "height:1000px;"))
                              )


      )))



server <- function(input, output, session){

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

    if(input$trackbox == FALSE){
      df <- prepareData(folder(), stim_numb(), as.numeric(input$frame_rate),
                        compare_groups = TRUE, marker_thresh = as.numeric(input$mark_thresh))

    }

    if(input$trackbox == TRUE){
      print(("TRUE"))
      df <- calipR::prepareData_track(folder(), stim_numb(), as.numeric(input$frame_rate),
                                compare_groups = FALSE, marker_thresh = as.numeric(input$mark_thresh))

    }

      calipR::saveData(df, "db_cq.sqlite", "df_full")

      df
    })




  output$df_sql <- DT::renderDataTable({

  if(is.null(df_final())) {return()}

  else{
  df <- calipR::loading100("db_cq.sqlite", "df_full")
  df <- DT::datatable(df)
  df
  }
  })


############### Module to create the banks ######################


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

  db_name <- reactiveValues(name = "yo")

  shiny::observeEvent(input$start_creation, {

    db_name$name <- input$db_name

  })



  df_full <- shiny::eventReactive(input$start_creation, {

    df_full <- calipR::get_full_df(paste0(db_name$name, ".sqlite"), "df_full")
    df_full
  })


  print("yaaa")


  new_DF <- reactiveValues(data = data.frame(x = seq(1,10), y = seq(1,10)))
  shiny::observeEvent(input$start_creation, {

    new_DF$data <- df_full()
  })

  print("yii")

  coordinates <- list()

  observeEvent(input$clickposition, {
    new_DF$data <- rbind(new_DF$data, input$clickposition)



  })

  pattern_list <- reactiveValues('1' =  c(1,1,1))


  observeEvent(input$saveNewBank, {

    final_bank <- reactiveValuesToList(pattern_list)
    saveRDS(final_bank, file = paste(input$bankName))

  })

  counter <- reactiveValues(value = 0)

  observeEvent(input$submitPattern, {
    counter$value <- counter$value + 1
    start <- as.integer(new_DF$data[['x']][length(new_DF$data[['x']]) -1])
    end <-  as.integer(new_DF$data[['x']][length(new_DF$data[['x']])])

    pattern <- new_DF$data[['y']][start:end]
    print(pattern)
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
    print(test)
  })



  output$myPlot <- renderPlotly({
    print("new_DF")
    print(new_DF$data)
    plot_ly(new_DF$data, x = ~x, y = ~y, type = "scatter", mode = input$displayType) %>%
      onRender(js, data = "clickposition")
  })

  output$pattern <- renderPlotly({
    end <-  as.integer(new_DF$data[['x']][length(new_DF$data[['x']])])
    pattern_len <- length(pattern_viewer[['1']])
    to_add <- end - pattern_len
    fluo <- pattern_viewer[['1']]
    time <- seq(1,length(fluo))
    df <- as.data.frame(y = fluo, x = time)
    plot_ly(df, x = ~time, y = ~fluo, type = "scatter", mode = "line")
  })


  observeEvent(input$cell, {
    print(new_DF$data)
    new_DF$data <- data.frame(x = data.table::setDT(df_full())[Cell_id == unique(df_full()$Cell_id)[[input$cell]]]$time_frame,
                      y = data.table::setDT(df_full())[Cell_id == unique(df_full()$Cell_id)[[input$cell]]]$Mean_Grey )
  })

  myPlotProxy <- plotlyProxy("myPlot", session)

  observe({
    print("yuu")
    print(new_DF$data)
    plotlyProxyInvoke(myPlotProxy, "restyle", list(x = list(new_DF$data[['x']]), y = list(new_DF$data[['y']])))
  })

  output$click <- renderPrint({
    #clickposition_history()
    new_DF$data
  })


### End of the module to create the banks

    df_plot <- shiny::eventReactive(input$cell_click, {

      df <- calipR::get_cell(input$cell_viz, "db_cq.sqlite", "df_full")

      df
    })


    output$plot_cell <- shiny::renderPlot({

      p <- cell_plot_shiny(df_plot())
      p


  })



    res_sim <- shiny::reactiveValues(res = NULL)

    shiny::observeEvent(input$patMatch_opt, {

      if(input$patMatch_opt == TRUE){
      output$posBank_field <- shiny::renderUI({
        shiny::textInput(inputId = "posBank", "Positive Bank", placeholder = "Enter the name of the bank you want to use")

      })

      output$negBank_field <- shiny::renderUI({
        shiny::textInput(inputId = "negBank", "Negative Bank", placeholder = "Enter the name of the bank you want to use")

      })


      output$window_field <- shiny::renderUI({
        shiny::selectInput(inputId = "windows", "Windows", list("5" = 5,"10" = 10, "15" = 15,
                                                                "20" = 20, "25"=25,
                                                                "30"=30,"35"=35,
                                                                "40"=40,"45"=45,
                                                                "50"=50,"55"=55,
                                                                "60"=60,"65"=65,
                                                                "70"=70,"75"=75,
                                                                "80"=80,"85"=85,
                                                                "90"=90,"95"=35,
                                                                "100"=100,"105"=105,
                                                                "110"=110,"115"=115,
                                                                "120"=120,"125"=125),multiple=TRUE      )})


    }
      if(input$patMatch_opt == FALSE){

        output$posBank_field <- NULL
        output$negBank_field <- NULL
        output$window_field <- NULL
      }
      })


   shiny::observeEvent(input$sim, {

      print("Simulation started")
      df_sub <- calipR::get_sub_df("db_cq.sqlite", "df_full", input$n_cells)


      if(input$patMatch_opt == TRUE){
      posBank <- readRDS(input$posBank)
      negBank <- readRDS(input$negBank)

      posBank <- Filter(Negate(is.null), posBank)
      negBank <- Filter(Negate(is.null), negBank)
      }

      else{
        posBank <- list()
        negBank <- list()
      }


      res_sim$res <- downstream_analysis(df_sub, threshold = input$peak_thresh,
                                         borders_range = input$rise_range, lambda = input$lambda, gam = input$gam,
                                         false_pos = input$false_pos, simulation = TRUE, pattern_matching = input$patMatch_opt,
                                         posBank = posBank, negBank = negBank, windows = as.integer(input$windows))

   })


    if(is.null(res_sim)){

    }
   else{
      output$responders <- shiny::renderUI({
        data <- res_sim$res
        responders <- unique(data[[1]]$Cell_id)
        shiny::selectInput(inputId = "responders", "Responders", responders)
      })
   }

   if(is.null(res_sim)){

   }
   else{
    output$non_responders <- shiny::renderUI({
      data <- res_sim$res
      cells <- unique(data[[2]]$Cell_id)
      responders <- unique(data[[1]]$Cell_id)
      non_responders <- cells %in% responders
      non_responders <- unlist(purrr::map2(cells, non_responders, function(x,y) if(y == FALSE){x}))
      shiny::selectInput(inputId = "non_responders", "Non Responders", non_responders)
    })

}

   if(is.null(res_sim)){

   }
   else{
    output$stats_opt <- DT::renderDataTable({
      data <- res_sim$res
      res2 <- data[[3]][[2]]
      res2

    })
}




    shiny::observeEvent(input$plot_responders, {

      data <- res_sim$res


      output$plot_cell_sim <- shiny::renderPlot({

        print(data[[1]])
        print(data[[2]])

        input$patMatch_opt
        if(input$patMatch_opt == TRUE){
        p <- cell_plot(data[[2]], data[[1]], var = input$cell_plot_var, cell = input$responders, line = "back", show_peak = input$show_peak)
        p
        }

        if(input$patMatch_opt == FALSE){
          p <- cell_plot(data[[2]], data[[1]], var = input$cell_plot_var, cell = input$responders, line = "gam", show_peak = input$show_peak)
          p
        }


    })
    })

      shiny::observeEvent(input$plot_non_responders, {

        data <- res_sim$res

      output$plot_cell_sim <- shiny::renderPlot({

        if(input$patMatch_opt == TRUE){
          p <- cell_plot(data[[2]], data[[1]], var = input$cell_plot_var, cell = input$non_responders, line = "back", show_peak = input$show_peak)
          p
        }

        if(input$patMatch_opt == FALSE){
          p <- cell_plot(data[[2]], data[[1]], var = input$cell_plot_var, cell = input$non_responders, line = "gam", show_peak = input$show_peak)
          p
        }


      })
    })

      shiny::observeEvent(input$patMatch_opt_bis, {

        if(input$patMatch_opt_bis == TRUE){
          output$posBank_field_bis <- shiny::renderUI({
            shiny::textInput(inputId = "posBank_bis", "Positive Bank", placeholder = "Enter the name of the bank you want to use")

          })

          output$negBank_field_bis <- shiny::renderUI({
            shiny::textInput(inputId = "negBank_bis", "Negative Bank", placeholder = "Enter the name of the bank you want to use")

          })


          output$window_field_bis <- shiny::renderUI({
            shiny::selectInput(inputId = "windows_bis", "Windows", list("5" = 5,"10" = 10, "15" = 15,
                                                                        "20" = 20, "25"=25,
                                                                        "30"=30,"35"=35,
                                                                        "40"=40,"45"=45,
                                                                        "50"=50,"55"=55,
                                                                        "60"=60,"65"=65,
                                                                        "70"=70,"75"=75,
                                                                        "80"=80,"85"=85,
                                                                        "90"=90,"95"=35,
                                                                        "100"=100,"105"=105,
                                                                        "110"=110,"115"=115,
                                                                        "120"=120,"125"=125),multiple=TRUE      )})


        }
        if(input$patMatch_opt_bis == FALSE){

          output$posBank_field_bis <- NULL
          output$negBank_field_bis <- NULL
          output$window_field_bis <- NULL
        }


      })




      shiny::observeEvent(input$sim_bis, {

        print("second stim started")
        df <- res_sim$res[[2]]

        print("df ok ")
        df_sub_bis <- df[df$Cell_id == input$cell_opt]


        if(input$patMatch_opt_bis == TRUE){
          posBank <- readRDS(input$posBank_bis)
          negBank <- readRDS(input$negBank_bis)

          posBank <- Filter(Negate(is.null), posBank)
          negBank <- Filter(Negate(is.null), negBank)
        }

        else{
          posBank <- list()
          negBank <- list()
        }
        print("df_sub ok")

        res_sim$res_bis <- downstream_analysis(df_sub_bis, threshold = input$peak_thresh_bis,
                                               borders_range = input$rise_range_bis, lambda = input$lambda_bis,
                                               gam = input$gam_bis, false_pos = input$false_pos_bis,simulation = TRUE, one_cell = TRUE,
                                               pattern_matching = input$patMatch_opt_bis,
                                               posBank = posBank, negBank = negBank,
                                               windows = as.integer(input$windows_bis))

      })




      shiny::observeEvent(input$plot_simulation_bis, {

        data <- res_sim$res_bis


        output$plot_cell_sim_bis <- shiny::renderPlot({

          if(input$patMatch_opt_bis == TRUE){
            p <- cell_plot(data[[2]], data[[1]], var = input$cell_plot_var, cell = input$cell_opt, line = "back", show_peak = input$show_peak_bis)
            p
          }

          if(input$patMatch_opt_bis == FALSE){
            p <- cell_plot(data[[2]], data[[1]], var = input$cell_plot_var, cell = input$cell_opt, line = "gam", show_peak = input$show_peak_bis)
            p
          }

        })
  })


      ### Analyze the full dataset
      res_full <- shiny::reactiveValues(res = NULL)


      # This block makes the check box "compare groups" reactive
      shiny::observe({

        group_value <- input$groups
        shiny::updateCheckboxInput(shiny::getDefaultReactiveDomain(), "groups", value = group_value)
      })


      ### This block codes alows to launch the analysis when the button analyze dataset is clicked on. It runs the whole analysis, on the whole dataset
      shiny::observeEvent(input$patMatch, {

        if(input$patMatch == TRUE){
          output$posBank_field_full <- shiny::renderUI({
            shiny::textInput(inputId = "posBank_full", "Positive Bank", placeholder = "Enter the name of the bank you want to use")

          })

          output$negBank_field_full <- shiny::renderUI({
            shiny::textInput(inputId = "negBank_full", "Negative Bank", placeholder = "Enter the name of the bank you want to use")

          })


          output$window_field_full <- shiny::renderUI({
            shiny::selectInput(inputId = "windows_full", "Windows", list("5" = 5,"10" = 10, "15" = 15,
                                                                         "20" = 20, "25"=25,
                                                                         "30"=30,"35"=35,
                                                                         "40"=40,"45"=45,
                                                                         "50"=50,"55"=55,
                                                                         "60"=60,"65"=65,
                                                                         "70"=70,"75"=75,
                                                                         "80"=80,"85"=85,
                                                                         "90"=90,"95"=35,
                                                                         "100"=100,"105"=105,
                                                                         "110"=110,"115"=115,
                                                                         "120"=120,"125"=125),multiple=TRUE      )})


        }
        if(input$patMatch == FALSE){

          output$posBank_field_full <- NULL
          output$negBank_field_full <- NULL
          output$window_field_full <- NULL
        }


      })




      res <- shiny::observeEvent(input$ana_full_button, {


        print("inside anafull")
        df_full <- calipR::get_full_df("db_cq.sqlite", "df_full")
        print("df_full ok")
        print("input$groups")

        ### Pour parallelisation : Splitter le df par coverslip :

        #cov_list <- split(df_full, df_full$coverslip)


        if(input$patMatch == TRUE){
          posBank <- readRDS(input$posBank_full)
          negBank <- readRDS(input$negBank_full)

          posBank <- Filter(Negate(is.null), posBank)
          negBank <- Filter(Negate(is.null), negBank)
        }

        else{
          posBank <- list()
          negBank <- list()
        }

        print(input$groups)
        res_full$res <- downstream_analysis(df_full, threshold = input$peak_thresh_full,
                                             borders_range = input$rise_full, lambda = input$lambda_full, gam = input$gam_full,
                                             false_pos = input$false_pos_full, compare_groups = input$groups,
                                     pattern_matching = input$patMatch, posBank = posBank, negBank = negBank,
                                     windows = as.integer(input$windows_full))


        # Extracting and saving the data table containing one row for each peak with the informations
        #about the peak
        res1 <- res_full$res[[1]]
        print(res1)
        print(str(res1))
        calipR::saveData(res1, "db_cq.sqlite", "peak_res")

        # Extracting and saving the full data table updated
        res2 <- res_full$res[[2]]
        res2 <- data.table::setDT(res2)[, peak_frames := NULL]
        print(res2)
        print(str(res2))
        calipR::saveData(res2, "db_cq.sqlite", "df_final")


        res3_1 <- data.table::setDT(res_full$res[[3]][[1]])
        print(res3_1)
        print(str(res3_1))
        calipR::saveData(res3_1, "db_cq.sqlite", "stats_desc_final")


if(input$groups == TRUE){print( "it is true")}
        else{
          print("it is not true")
          print(res_full$res[[3]][[2]][[1]])
          print(str(res_full$res[[3]][[2]][[1]]))
        res3_3_1 <- data.table::setDT(res_full$res[[3]][[2]][[1]])
        calipR::saveData(res3_3_1, "db_cq.sqlite", "overall_q")

        res3_3_2 <- data.table::setDT(res_full$res[[3]][[2]][[2]])
        calipR::saveData(res3_3_2, "db_cq.sqlite", "pairwise")
        }

      })


      # retrieving full dataset and peaks dataset :
      if(dim(calipR::checkTable("db_cq.sqlite", "'df_final'"))[1] == 0) {
        }
      else{
      full <- calipR::get_full_df("db_cq.sqlite", "df_final")
      peaks <- calipR::get_full_df("db_cq.sqlite", "peak_res")
}

      res <- shiny::eventReactive(input$update_button,{

      if(input$base_resp == TRUE){
      peaks_wo_base <- calipR::base_resp.rm(peaks, full)[[1]]
      full_wo_base <- calipR::base_resp.rm(peaks, full)[[2]]

      res <- Analyze_Responses(peaks_wo_base, full_wo_base, var_list = input$grouping_var)
      }

      else{
        full <- calipR::get_full_df("db_cq.sqlite", "df_final")
        peaks <- calipR::get_full_df("db_cq.sqlite", "peak_res")
        res <- Analyze_Responses(peaks, full, var_list = input$grouping_var)
      }
      print(res)
      res

})


      observeEvent(input$update_button, {
      output$resp_count <- DT::renderDataTable({


        if(dim(calipR::checkTable("db_cq.sqlite", "'stats_desc_final'"))[1] == 0) {}
        else{

        DT::datatable({res()[[1]]},
                      extensions = 'Buttons',
                      options = list(
                        paging = TRUE,
                        searching = TRUE,
                        fixedColumns = TRUE,
                        autoWidth = TRUE,
                        ordering = TRUE,
                        dom = 'tB',
                        pageLength = 100,
                        c("copy", "csv")),

                      class = "display"

)
}

      })



      output$overall_q <- DT::renderDataTable({

       # if(dim(calipR::checkTable("db_cq.sqlite", "'overall_q'"))[1] == 0) {}
       # else{


        DT::datatable({res()[[2]][[1]]},
                      extensions = 'Buttons',
                      options = list(
                        paging = TRUE,
                        searching = TRUE,
                        fixedColumns = TRUE,
                        autoWidth = TRUE,
                        ordering = TRUE,
                        dom = 'tB',
                        c("copy", "csv")),

                      class = "display"

        )
        #}
      }
      )

      output$post_hoc_mcnemar <- DT::renderDataTable({

#        if(dim(calipR::checkTable("db_cq.sqlite", "'pairwise'"))[1] == 0) {}
        #else{

        DT::datatable({res()[[2]][[2]]},
                      extensions = 'Buttons',
                      options = list(
                        paging = TRUE,
                        searching = TRUE,
                        fixedColumns = TRUE,
                        autoWidth = TRUE,
                        ordering = TRUE,
                        dom = 'tB',
                        c("copy", "csv")),

                      class = "display"

        )
       # }
      })



      ### Computing dual proportions (proportion of cells responding to one stimulus that also responds to another stimulus)

      output$stim_list_1 <- shiny::renderUI({

        if(dim(calipR::checkTable("db_cq.sqlite", "'peak_res'"))[1] == 0) {}
        else{

        e <- calipR::get_full_df("db_cq.sqlite", "peak_res")



        stim_list <- unique(e$Start_peak_stimulus)

        shiny::selectInput(inputId = "stim_list_1", "Stimulus 1", stim_list)
        }
      })


      output$stim_list_2 <- shiny::renderUI({

        if(dim(calipR::checkTable("db_cq.sqlite", "'peak_res'"))[1] == 0) {}
        else{

        f <- calipR::get_full_df("db_cq.sqlite", "peak_res")


        stim_list <- unique(f$Start_peak_stimulus)


        shiny::selectInput(inputId = "stim_list_2", "Stimulus 2", stim_list)
        }
      })


      t <- shiny::eventReactive(input$dual_button, {

        if(dim(calipR::checkTable("db_cq.sqlite", "'peak_res'"))[1] == 0) {}
        else{

        g <- calipR::get_full_df("db_cq.sqlite", "peak_res")

        if(input$base_resp_dual == TRUE){
        g <- calipR::base_resp.rm(peaks, full)[[1]]


        }

        t <- calipR::dual_prop(g, input$stim_list_1, input$stim_list_2)
}
      })


      output$dual_prop <- DT::renderDataTable(

        DT::datatable({t()},
       extensions = 'Buttons',
       options = list(
         paging = TRUE,
         searching = TRUE,
         fixedColumns = TRUE,
         autoWidth = TRUE,
         ordering = TRUE,
         dom = 'tB',
         c("copy", "csv")),

       class = "display"

       ))


})
      ### Ajouter des visualisations des pourcentages par groupe, par coverslip etc. :



        output$viz <- plotly::renderPlotly({

          print(res()[[1]])
          print(str(res()[[1]]))
          #res <- calipR::get_full_df("db_cq.sqlite", "stats_desc_by_cov_group")

             plotly::plot_ly(

              type = 'bar',

              x = res()[[1]][[input$x]],

              y = res()[[1]][[input$y]],
              text = paste("Group: ", res()[[1]][["group"]],

                           "<br>Stimulus:  ", res()[[1]][["stimulus"]],

                           "<br>Responders: ", res()[[1]][["Responses"]],

                           "<br>Proportion: ", res()[[1]][["Prop"]],
                           "<br> Total cells: ", res()[[1]][["n_cells_grp"]]),

              hoverinfo = 'text',

              marker = list(size = 2),


              color = res()[[1]][[input$z]],

            ) %>%
               plotly::layout(barmode ="group", yaxis = list(automargin = TRUE),
                              xaxis = list(automargin = TRUE), bargap = -2, bargroupgap = 0)




      })

        if(dim(calipR::checkTable("db_cq.sqlite", "'df_final'"))[1] == 0) {
        }
        else{

          full <- calipR::get_full_df("db_cq.sqlite", "df_final")
          peaks <- calipR::get_full_df("db_cq.sqlite", "peak_res")

          output$resp_viz <- shiny::renderUI({
            data <- res_sim$res
            responders <- unique(peaks$Cell_id)
            shiny::selectInput(inputId = "resp_viz", "Responders", responders)

          })




          output$non_resp_viz <- shiny::renderUI({
            data <- res_sim$res
            cells <- unique(full$Cell_id)
            responders <- unique(peaks$Cell_id)
            non_responders <- cells %in% responders
            non_responders <- unlist(purrr::map2(cells, non_responders, function(x,y) if(y == FALSE){x}))
            shiny::selectInput(inputId = "non_resp_viz", "Non Responders", non_responders)

          })

        }





        shiny::observeEvent(input$plot_button,{

        output$plot_resp_viz <- renderPlot({


          # Opérer un tri sur les cellules regarder comment j'ai fais pour responders

          p <- cell_plot(full, peaks, var = "Mean_Grey", cell = input$resp_viz, line = "gam", show_peak = input$show_peaks_box)
          p


        })
        })


        shiny::observeEvent(input$plot_button_bis,{



          output$plot_resp_viz <- renderPlot({

          p <- cell_plot(full, peaks, var = "Mean_Grey", cell = input$non_resp_viz, line = "gam", show_peak = input$show_peaks_box)
          p



})


        })


          if(dim(calipR::checkTable("db_cq.sqlite", "'df_final'"))[1] == 0) {
            print( "dim == 0")
          }

          else{
            print("dim ok")
          dt <- get_full_df("db_cq.sqlite", "df_final")
          print(dt)

            output$clustering_var <- shiny::renderUI({
            list_var <- names(dt)
            #list_var <- names(res_full$res[[2]])

            shiny::selectInput(inputId = "clustvar", "Variable used for clustering", list_var)})

          }


        shiny::observeEvent(input$set_seed, {

          if(input$set_seed){

            output$seed_field <- shiny::renderUI({

              shiny::textInput("seed", "Enter Seed Value")

          })
          }

          else{
            output$seed_field <- NULL
          }

          })


        shiny::observeEvent(input$clustplot_button, {

        dt <- get_full_df("db_cq.sqlite", "df_final")
        dt_peaks <- get_full_df("db_cq.sqlite", "peak_res")

        responding_cells <- unique(dt_peaks$Cell_id)


        dt <- data.table::setDT(dt)[Cell_id %in% responding_cells]

        final <- prepareClustData(dt, input$clustvar, norm = input$normclust)

        print("final")
        print(final)

        print(input$nclust)

        if(input$set_seed){

          set.seed(as.integer(input$seed))
        }

        clust_res <- dtwclust::tsclust(final, type = "partitional", k = as.integer(input$nclust), distance = input$dist_type,
                                                centroid = "dba")


        output$clustplot <- shiny::renderPlot({

          p <- plot(clust_res, type="sc")
          p

        })
        })







}
shiny::shinyApp(ui, server)

}




