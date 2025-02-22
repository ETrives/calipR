
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

                                    shiny::tags$head(
                                      shiny::tags$style(shiny::HTML("

        .sidebar {
                      height: 100%;
        }

        .launch-btn {
       position: relative;
       left : 15px;
       width: 200px;
       color: white;
       background-color:#7a3193;
       border-color: #a365b9;
        }

         .launch-btn:focus {
       position: relative;
       left : 15px;
       width: 200px;
       color: white;
       background-color:#7a3193;
       border-color: #a365b9;
        }

        .launch-btn:hover {
      background-color:#b76ed1;
      border-color: #7a3193;
      transition: 0.5s;
        }

      .warn-side {

      position: relative;
      left: 40px;
      color: red;

      }

      .warn-side-long {

      position: relative;
      left: 30px;
      color: red;

      }
       .warn-main {

      color: red;

       }

       .project-type-btn {
       top:-10px;
       left:-10px;
       border-radius:0px;
       position:relative;
       color: white;
       background-color:#1c4257;
       border : 10px;
       border-color: white;
       float: left;
        }

         .project-type-btn:focus {
       position: relative;
       color: white;
       background-color:#0d1f29;
       border-color: #a365b9;
        }

        .project-type-btn:hover {
      background-color:#0d1f29;
      color : white;
      border-color: #7a3193;
      transition: 0.5s;
        }


  "
                                      )
                                      )
                                    ),

                                    shiny::img(src = "logo/calipR_logo.png", width = "70%", height = "70%",
                                               style = "position: relative; top: -30px; left: 35px;"),

                                    #shiny::tags$style(".left-side, .main-sidebar {padding-top: 180px}"),
                                    shinydashboard::sidebarMenu(id = "sidebarid",

                                                                shinydashboard::menuItem(
                                                                  "Prepare Your Data", tabName = "prep"),
                                                                shiny::conditionalPanel( 'input.sidebarid === "prep"',
                                                                                         shiny::checkboxInput("create", label = "Create New Project"),
                                                                                         shiny::checkboxInput("load", label = "Load Existing Project")),

                                                                shinydashboard::menuItem(
                                                                  "Visualize Raw Data", tabName = "viz"),
                                                                shiny::conditionalPanel( 'input.sidebarid === "viz"',
                                                                                         shiny::sliderInput("filter",label = "Choose a frequency filter", min = 2, max = 10000, value = 2, step =1 ),
                                                                                         shiny::uiOutput("downsampling"),
                                                                                         shiny::actionButton("saveFilteredData", "Save Filtered Data")

                                                                ),



                                                                shinydashboard::menuItem(
                                                                  "Create Your Banks", tabName = "bank"),
                                                                shiny::conditionalPanel( 'input.sidebarid === "bank"',
                                                                                         shiny::textInput("db_name", label = NULL, placeholder = "Database name"),
                                                                                         shiny::uiOutput("warning_db", class = "warn-side"),
                                                                                         shiny::textInput("bankName", "How do you want to call this bank ?"),
                                                                                         shiny::actionButton("start_creation", "Load Data", align = "center")),

                                                                shinydashboard::menuItem(
                                                                  "Annotate Your Data", tabName = "annotation"),
                                                                shiny::conditionalPanel( 'input.sidebarid === "annotation"',
                                                                                         shiny::textInput("db_name_bis", label = NULL, placeholder = "Database name"),
                                                                                         shiny::uiOutput("warning_db_bis",  class = "warn-side"),
                                                                                         shiny::textInput("annotation_project", "How do you want to call this annotation project ?"),
                                                                                         shiny::actionButton("start_creation_bis", "Load Data", align = "center")),

                                                                shinydashboard::menuItem(
                                                                  "Optimize Analysis Parameters", tabName = "opt"),
                                                                shinydashboard::menuItem(
                                                                  "Launch Full Analysis", tabName = "ana_full"),
                                                                shiny::conditionalPanel( 'input.sidebarid === "ana_full"',
                                                                                         shiny::numericInput("peak_thresh_full_z", label = "Peak Threshold (z)", value = 3, min = 0),
                                                                                         shiny::numericInput("peak_thresh_full_delta", label = "Peak Threshold (deltaf/f)", value = 0, min =0),
                                                                                         shiny::textInput("lambda_full", label = "Lambda", value = "1000"),
                                                                                         shiny::textInput("gam_full", label = "gam", value = "0.9"),
                                                                                         shiny::selectInput(inputId = "norm_method_full", label = "Choose method to compute z-score",
                                                                                                            list("Baseline Period" = "baseline", "Baseline Period without Peaks" = "estimate")),
                                                                                         shiny::checkboxInput("patMatch", label = "Background Estimation with SBPC"),
                                                                                         shiny::uiOutput('posBank_field_full'),
                                                                                         shiny::uiOutput('negBank_field_full'),
                                                                                         shiny::uiOutput('warning_bank', class = "warn-side"),
                                                                                         shiny::uiOutput('warning_data', class = "warn-side-long"),
                                                                                         fluidRow(shiny::actionButton("ana_full_button", "Launch Full Analysis", class ="launch-btn"
                                                                                         ))),
                                                                shinydashboard::menuItem(
                                                                  "Visualize Results", tabName = "viz_res")


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


                                      shinydashboard::tabItem("prep",


                                                              shiny::fluidRow(shiny::column(12,
                                                                                            shinydashboard::box(title = "Project", width = 12,
                                                                                                                solidHeader = TRUE, status = "primary", collapsible = T,

                                                                                                                shiny::uiOutput("project_type"),
                                                                                                                shiny::tags$br(),
                                                                                                                shiny::tags$br(),
                                                                                                                shiny::uiOutput("project_creation"),
                                                                                                                shiny::uiOutput("project_loading"),
                                                                                                                shiny::uiOutput("warning_load",  class = "warn-main")

                                                                                            ))),

                                                              shiny::fluidRow(shiny::column(12,
                                                                                            shinydashboard::box(title = "Dataset Prepared", width = 12, solidHeader = TRUE, status = "primary", collapsible = T,
                                                                                                                shiny::dataTableOutput("df_created"),
                                                                                                                shiny::dataTableOutput("df_loaded")))),



                                                              shiny::fluidRow(shiny::column(12,
                                                              shinydashboard::box(title = "Annotate Video", width = 6,
                                                                                  solidHeader = TRUE, status = "primary", collapsible = T,

                                                                                  shiny::uiOutput("annotateVideo"),shiny::div(style = "height:1000px")

                                                              ),

                                                              shinydashboard::box(title = "Annotation Project", width = 6,
                                                                                  solidHeader = TRUE, status = "primary", collapsible = T,

                                                                                  shiny::dataTableOutput("annotation_project"),shiny::div(style = "height:1000px")

                                                              )))),

                                      shinydashboard::tabItem("bank",
                                                              shiny::fluidRow(
                                                                shinydashboard::box(title = "Select Patterns by clicking on the graph", width = 12, solidHeader = TRUE, status = "primary",
                                                                                    shiny::uiOutput('cell_selector'),
                                                                                    shiny::uiOutput('warning', class = "warn-main"),
                                                                                    shiny::selectInput("displayType", "Type of data display", choices = list("points" = "markers", "line" = "line",
                                                                                                                                                             "both" = "lines+markers"),selected = "points" ),
                                                                                    plotly::plotlyOutput("myPlot")),

                                                                shinydashboard::box(title = "View and Add Patterns to Bank", width = 12, solidHeader = TRUE, status = "primary",
                                                                                    shiny::actionButton("viewPattern", "View Selected Pattern"),
                                                                                    shiny::actionButton("submitPattern", "Submit Pattern to Bank"),
                                                                                    plotly::plotlyOutput("pattern"),
                                                                                    shiny::actionButton("printList", "Print Bank"),
                                                                                    shiny::actionButton("saveNewBank", "Save Bank"),
                                                                                    shiny::actionButton("start_newbank", "Start New Bank"),

                                                                                    #shiny::verbatimTextOutput("click"),

                                                                                    #shiny::div(style = "height:1000px;")
                                                                ))),

                                      shinydashboard::tabItem("annotation",
                                                              shiny::fluidRow(
                                                                shinydashboard::box(title = "Annotate cells by clicking on the graph", width = 12, solidHeader = TRUE, status = "primary",
                                                                                    shiny::uiOutput('cell_selector_bis'),
                                                                                    shiny::uiOutput('warning_bis'),
                                                                                    shiny::selectInput("displayType_bis", "Type of data display", choices = list("points" = "markers", "line" = "line",
                                                                                                                                                                 "both" = "lines+markers"),selected = "points" ),
                                                                                    plotly::plotlyOutput("myPlot_bis")),

                                                                shinydashboard::box(title = "View and Add annotation to project", width = 12, solidHeader = TRUE, status = "primary",
                                                                                    shiny::actionButton("viewPattern_bis", "View Selected Pattern"),
                                                                                    plotly::plotlyOutput("pattern_bis"),
                                                                                    shiny::actionButton("saveAnnotation", "Save Annotation"),
                                                                                    shiny::actionButton("newAnnotation", "Start New Annotation Project"),

                                                                                    #shiny::verbatimTextOutput("click"),

                                                                                    #shiny::div(style = "height:1000px;")
                                                                ))),



                                      shinydashboard::tabItem("viz",
                                                              shiny::fluidRow(
                                                                shinydashboard::box(title = "Plotting Cells", width = 12, solidHeader = TRUE, status = "primary",
                                                                                    shiny::numericInput("cell_num",label = "cell_number", value = 1, min = 1),
                                                                                    shiny::checkboxInput("align_behavior", label = "align behavior and fiber photometry data"),
                                                                                    shiny::checkboxInput("fit_isos", label = "fit isosbestic trace"),
                                                                                    shiny::checkboxInput("delta_isos", label = "normalize with isosbestic (f) channel (delta f/f)"),
                                                                                    plotly::plotlyOutput( "plot_cell"))),

                                                              shiny::fluidRow(
                                                                shinydashboard::box(title = "Plotting Isosbestic Trace", width = 12, solidHeader = TRUE, status = "primary",
                                                                                    plotly::plotlyOutput( "plot_isos"))),


                                                              shiny::fluidRow(
                                                                shinydashboard::box(title = "Filtering Trace", width = 12, solidHeader = TRUE, status = "primary",
                                                                                    plotly::plotlyOutput("plot_cell_filter")))),



                                      shinydashboard::tabItem("opt",

                                                              shiny::fluidRow(
                                                                shinydashboard::box(title = "Optimize Parameters", width = 12, solidHeader = TRUE, status = "primary",
                                                                                    shiny::numericInput("peak_thresh", label = "Peak Threshold (z score)", value = 3, min = 0),
                                                                                    shiny::numericInput("peak_thresh_delta", label = "Peak Threshold (delta f/f)", value = 0, min = 0),
                                                                                    shiny::textInput("lambda", label = "Lambda", placeholder = "Enter the Lambda parameter for the Deconvolution (integer)", value = "1000"),
                                                                                    shiny::textInput("gam", label = "Gam", placeholder = "Enter the Gam parameter for the Deconvolution (double between 0-1)", value = "0.9"),
                                                                                    shiny::textInput("n_cells", label = "Number of cells", placeholder = "Enter the number of cells you want to run the stimulation on", value = "10"),

                                                                                    shiny::selectInput(inputId = "norm_method", label = "Choose method to compute z-score",
                                                                                                       list("Baseline Period" = "baseline", "Baseline Period without Peaks" = "estimate")),
                                                                                    shiny::checkboxInput("patMatch_opt", label = "Background Estimation with SBPC"),

                                                                                    shiny::uiOutput('posBank_field'),
                                                                                    shiny::uiOutput('negBank_field'),

                                                                                    shiny::checkboxInput("show_peak", label = "Show Peaks"),

                                                                                    shiny::uiOutput('responders'),
                                                                                    shiny::uiOutput('non_responders'),

                                                                                    shiny::uiOutput('warning_data_sim', class = "warn-main"),
                                                                                    shiny::uiOutput('warning_bank_sim', class = "warn-main"),

                                                                                    shiny::actionButton("sim", "Simulate Analysis", align = "center"),
                                                                                    shiny::actionButton("plot_responders", "Plot Responder", align = "center"),
                                                                                    shiny::actionButton("plot_non_responders", "Plot Non Responder", align = "center"))),

                                                              shiny::fluidRow(
                                                                shinydashboard::box(title = "Plot Window", width = 12, solidHeader = TRUE, status = "primary",
                                                                                    shiny::selectInput(inputId = "cell_plot_var", label = NULL,
                                                                                                       list("Mean_Grey" = "Mean_Grey", "Delta_F/F" = "delta_f_f", "z_score" = "z_score",
                                                                                                            "Smooth_Delta_F/F" = "smooth_delta", "First_Derivative" = "first_derivative",
                                                                                                            "Smooth_First_Derivative" = "smooth_Diff", "Deconvolved_trace" = "deconvolved_trace")),
                                                                                    shiny::plotOutput(outputId = "plot_cell_sim"))),
                                                              shiny::fluidRow(
                                                                shinydashboard::box(title = "Try other parameters on a given cell", width = 12, solidHeader = TRUE, status = "primary",
                                                                                    shiny::textInput("cell_opt", label = "Cell", placeholder = "On which cell do you want to try new parameters ?"),

                                                                                    shiny::numericInput("peak_thresh_bis", label = "Threshold (z_score)", value = 3, min = 0),
                                                                                    shiny::numericInput("peak_thresh_bis_delta", label = "Threshold (delta f/f)", value = 0, min = 0),

                                                                                    shiny::textInput("lambda_bis", label = "Lambda", placeholder = "Enter the Lambda parameter for the Deconvolution (integer)", value = "1000"),
                                                                                    shiny::textInput("gam_bis", label = "Gam", placeholder = "Enter the Gam parameter for the Deconvolution (double between 0-1)", value = "0.9"),
                                                                                    shiny::selectInput(inputId = "norm_method_bis", label = "Choose method to compute z-score",
                                                                                                       list("Baseline Period" = "baseline", "Baseline Period without Peaks" = "estimate")),
                                                                                    shiny::checkboxInput("patMatch_opt_bis", label = "Background Estimation with SBPC"),

                                                                                    shiny::uiOutput('posBank_field_bis'),
                                                                                    shiny::uiOutput('negBank_field_bis'),


                                                                                    shiny::checkboxInput("show_peak_bis", label = "Show Peaks"),

                                                                                    shiny::uiOutput('warning_data_sim_bis', class = "warn-main"),
                                                                                    shiny::uiOutput('warning_bank_sim_bis', class = "warn-main"),

                                                                                    shiny::actionButton("sim_bis", "Simulate Analysis", align = "center"),
                                                                                    shiny::actionButton("plot_simulation_bis", "Plot Cell", align = "center"))),



                                                              shiny::fluidRow(
                                                                shinydashboard::box(title = "Plot Window", width = 12, solidHeader = TRUE, status = "primary",
                                                                                    shiny::plotOutput(outputId = "plot_cell_sim_bis"))),

                                                              shiny::fluidRow(
                                                                shinydashboard::box(title = "Statistics", width = 12, solidHeader = TRUE, status = "primary",
                                                                                    DT::dataTableOutput("stats_opt"))),
                                                              shiny::fluidRow(
                                                                shinydashboard::box(title = "Statistics", width = 12, solidHeader = TRUE, status = "primary",
                                                                                    DT::dataTableOutput("stats_opt_auc")))

                                      ),



                                      shinydashboard::tabItem("ana_full",

                                                              shiny::fluidRow(
                                                                shinydashboard::box(title = "Description of Responses", width = 12, solidHeader = TRUE, status = "primary",
                                                                                    shiny::uiOutput("spinner"),
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
                                                                                    DT::dataTableOutput("dual_prop"))),


                                                              ### Clustering module :

                                                              shiny::fluidRow(
                                                                shinydashboard::box(title = "Clustering", width = 12, solidHeader = TRUE, status = "primary",
                                                                                    shiny::uiOutput('clustering_var'),
                                                                                    shiny::checkboxInput("normclust", label = "Z Normalize Values"),
                                                                                    shiny::textInput("nclust", "Number of Clusters"),
                                                                                    shiny::uiOutput('cells_to_rm'),
                                                                                    shiny::checkboxInput("set_seed", "Set Seed"),
                                                                                    shiny::uiOutput('seed_field'),

                                                                                    shiny::selectInput("dist_type", "Distance Measure to Use",
                                                                                                       proxy::pr_DB$get_entry_name()),
                                                                                    shiny::actionButton("clustplot_button", "Launch Clustering", align = "right"),
                                                                                    shiny::plotOutput(outputId = "clustplot"),
                                                                                    shiny::uiOutput('export_csv')



                                                                ),shiny::div(style = "height:1000px;"))),




                                      shinydashboard::tabItem("viz_res",
                                                              shiny::fluidRow(
                                                                shinydashboard::box(title = "Plotting Results", width = 6, solidHeader = TRUE, status = "primary",
                                                                                    shiny::uiOutput("var_selector"),
                                                                                    plotly::plotlyOutput(outputId = "viz")),
                                                                shinydashboard::box(title = "Plotting Cluster Results", width = 6, solidHeader = TRUE, status = "primary",
                                                                                    shiny::uiOutput("var_selector_clust"),
                                                                                    plotly::plotlyOutput(outputId = "viz_clust_auc"))

                                                              ),




                                                              #shiny::div(style = "height:1000px;")),

                                                              shiny::fluidRow(
                                                                shinydashboard::box(title = "Plotting Cells", width = 12, solidHeader = TRUE, status = "primary",

                                                                                    shiny::uiOutput('resp_viz'),
                                                                                    shiny::uiOutput('non_resp_viz'),
                                                                                    shiny::actionButton("plot_button", "Plot Responder", align = "right"),
                                                                                    shiny::actionButton("plot_button_bis", "Plot Non Responder", align = "right"),
                                                                                    shiny::checkboxInput("show_peaks_box", label = "Show Peaks"),




                                                                                    shiny::plotOutput(outputId = "plot_resp_viz")),shiny::div(style = "height:1000px;"))
                                      )


                                    )))



  server <- function(input, output, session){

    '%notin%' <- Negate('%in%')
    'isnotnull' <- Negate('is.null')


    root_path <- paste0(getwd(),"/projects")
    sqlitePath <- getwd()

    project <- reactiveValues(dir_path = "no path")

    orig_freq <- reactiveVal(value = 1)


    reticulate::source_python(paste0(getwd(), "/inst/python_scripts/tdt_extraction.py"))
    reticulate::source_python(paste0(getwd(), "/inst/python_scripts/addEpocs.py"))
    reticulate::source_python(paste0(getwd(), "/inst/python_scripts/annotateVideo.py"))


    #### Data Preparation ##############

    shiny::observeEvent(input$create, {

      if(input$create == TRUE){

        output$project_type <- shiny::renderUI( {

          list(
            shiny::actionButton("in_vitro", "In Vitro Project", align = "left", class = "project-type-btn"),
            shiny::actionButton("fiber", "Fiber Photometry project", align = "right", class = "project-type-btn")

          )})
      }

      else{

        output$project_type <- NULL

      }

    })

    creation_tab <- reactiveValues(elements = NULL)

    shiny::observeEvent(input$in_vitro, {

      creation_tab$elements <- list(
        shiny::textInput("proj_name", label = "Project Name" ),
        shiny::textInput("frame_rate", label = "Enter your frame rate (Hz)", placeholder = "e.g. 0.5" ),
        shiny::selectInput("unit", label = "Select the unit of the times indicated in meta",
                           choices = list("minutes" = "minutes", "seconds" = "seconds") ),
        shiny::verbatimTextOutput("value"),
        shinyDirButton('folder', 'Select a folder', 'Please select a folder', FALSE),
        shiny::uiOutput("folder_warning"),
        shiny::textInput("mark_thresh", label = "if you have a cellular marker, enter your threshold", placeholder = "e.g. 30"),
        shiny::checkboxInput("trackbox", label = "Check if you did ROI detection with Trackmate"),
        shiny::actionButton("creating", "Load & Tidy Data", align = "center")
      )

      creation_tab$videoElements <- NULL
    })



    shiny::observeEvent(input$fiber, {

      creation_tab$elements <- list(
        shiny::textInput("proj_name", label = "Project Name" ),
        shiny::textInput("frame_rate", label = "Enter your frame rate (Hz)", placeholder = "e.g. 0.5" ),
        shinyDirButton('folder', 'Select a folder', 'Please select a folder', FALSE),
        shiny::uiOutput("folder_warning"),
        shiny::actionButton("creating_fiber", "Load Fiber Photometry Data", align = "center")
      )


      creation_tab$videoElements <- list(
        shiny::actionButton("loadVideoButton", "Load Video Files", align = "center"),
        shiny::tags$br(),
        shiny::textInput("videoPath", label = "path to the video you want to annotate" ),
        shiny::textInput("animal_id", label = "name the output annotation file (eg : mouse_1.csv)" ),
        shiny::actionButton("annotateVideoButton", "Annotate Video", align = "center"),
        shiny::actionButton("save_video_annotation", "Save Annotation", align = "center"),
        shiny::dataTableOutput("annotated_Video")

      )


    })



    output$annotateVideo <- shiny::renderUI({

      creation_tab$videoElements
    })


    output$project_creation <- shiny::renderUI( {
      creation_tab$elements
    })

    video_status <- shiny::reactiveValues(l = list())

    observeEvent(input$loadVideoButton, {

      if(dim(calipR::checkTable(paste(project$dir_path,project$db_file, sep ="/"), "'video_status'"))[1] == 0) {
        print("the table has not been found")

      print(db$load[["root_path"]][1])
      res <- extractAllVideoPath(db$load[["root_path"]][1])
      paths <- lapply(res, function(x) x)

      dt_paths <- data.table(paths)[, status := "not annotated"][, time_at_annotation := NA]
      dt_paths$paths <- as.character(dt_paths$paths)

      video_status$l <- dt_paths$status
      video_status$dt <- dt_paths

      calipR::saveData(video_status$dt, paste(project$dir_path, project$db_file, sep = "/"), "video_status")

      }
      else{
        print("the table has been found")
        video_status$dt <- data.table::setDT(calipR::get_full_df(paste(project$dir_path, project$db_file,sep = "/"), "video_status"))
        video_status$l <- video_status$dt[["status"]]
      }

      output$annotation_project <- shiny::renderDataTable({video_status$dt},
      options = list(scrollX = TRUE))

    })

    ### Creating a new project for In Fiber data :
    observeEvent(input$creating_fiber, {

      project$name <- input$proj_name
      project$dir_path <- paste(root_path, project$name, sep = "/")
      project$db_file <- paste0(project$name, ".sqlite")

      '%notin%' <- Negate('%in%')
      if(is.null(input$folder)) {}


      else{
        output$folder_warning <- NULL
        folder <-  abs_path$path()


        if(length(list.files(project$dir_path)) == 0){
          dir.create(project$dir_path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
        }


        extract_all_tdt_data(abs_path$path(), destination_path = getwd())
        df <- fread(paste0(getwd(), "/full_Extracted_TDT_Data.csv"))[, Cell_id := ID][, Mean_Grey := CA_TRACE]
        colnames(df)[1] <- "time_frame"

        ids <- unique(setDT(df)$Cell_id)

        if("ID" %notin% colnames(df)){
          val <- setDT(df)[Cell_id == ids[[1]] & time_seconds <= 1, .N]
        }

        if("ID" %in% colnames(df)){
          val <- setDT(df)[Cell_id == ids[[1]] & TIME_SECONDS <= 1, .N]
        }
        orig_freq(val)

        df[, root_path := abs_path$path()]

        calipR::saveData(df, paste(project$dir_path, project$db_file, sep = "/"), "df_full")

        df <- calipR::loading100(paste(project$dir_path, project$db_file, sep = "/"), "df_full")

        output$df_created <- shiny::renderDataTable({df},
                                                    options = list(scrollX = TRUE))


        if(dim(calipR::checkTable(paste(project$dir_path,project$db_file, sep ="/"), "'df_full'"))[1] == 0) {
print("yikoul")
        }
        else{
          print("yak")
        db$load <- data.table::setDT(calipR::get_full_df(paste(project$dir_path, project$db_file,sep = "/"), "df_full"))
        }

      }
    })


    shiny::observeEvent(input$annotateVideoButton, {

      # Annotating videos :

      annotateVideo(input$videoPath, paste(project$dir_path, paste0(input$animal_id,".csv"), sep= "/" ), list("f", "m", "w"), list(TRUE, TRUE, TRUE))

      annotated_video <- data.table::fread(paste0(paste(project$dir_path, input$animal_id, sep= "/" ), ".csv"))

      annotated_video[, ID := input$animal_id]

      print("annotated_video_first")
      print(annotated_video)

      ### Working on behavior alignment
      print('db$load[["root_path"]][1]')
      print(db$load[["root_path"]][1])

      addEpocs(paste(db$load[["root_path"]][1], input$animal_id, sep = "/"), annotated_video, "behavioral_data.csv", project$dir_path)
      print("yoo")
      annotated_video <- fread( paste(project$dir_path, "behavioral_data.csv", sep= "/" ))
      print("annotated_video")
      print(annotated_video)

      ###

      if(length(which(colnames(annotated_video) == "V1")) > 1){

        v1_to_rm <- which(colnames(annotated_video) == "V1")[[1]]
        annotated_video[, v1_to_rm] <- NULL

      }

      print("annotated_video_last")
      print(annotated_video)

      if(dim(calipR::checkTable(paste(project$dir_path,project$db_file, sep ="/"), "'video_annotation'"))[1] == 0) {

      calipR::saveData(annotated_video, paste(project$dir_path, project$db_file, sep = "/"), "video_annotation")

      }

      else{

      video_status$annotation <- setDT(calipR::get_full_df(paste(project$dir_path, project$db_file, sep = "/"), "video_annotation"))

      video_status$annotation <- rbind(video_status$annotation, annotated_video)
      calipR::saveData(video_status$annotation, paste(project$dir_path, project$db_file, sep = "/"), "video_annotation")

      }

      print("yish")

})

    shiny::observeEvent(input$save_video_annotation, {
      # Updating the video_status data :

      current_path <- which(video_status$dt[["paths"]] == input$videoPath)

      video_status$dt[["status"]][current_path] <- "annotated"
      video_status$dt[["time_at_annotation"]][current_path] <- Sys.time()

      calipR::saveData(video_status$dt, paste(project$dir_path, project$db_file, sep = "/"), "video_status")


      if(dim(calipR::checkTable(paste(project$dir_path,project$db_file, sep ="/"), "'video_annotation'"))[1] == 0) {
      }
      else{
      video_status$dt <- data.table::setDT(calipR::get_full_df(paste(project$dir_path, project$db_file,sep = "/"), "video_status"))
      }


    })



    output$annotated_Video <- shiny::renderDataTable({video_status$annotation},
                                                     options = list(scrollX = TRUE))

    output$annotation_status <- shiny::renderDataTable({video_status$dt},
                                                options = list(scrollX = TRUE))





    shiny::observeEvent(input$load, {

      if(input$load == FALSE){

        output$project_loading <- NULL
      }


      if(input$load == TRUE){
        output$project_loading <- shiny::renderUI({


          list(
            shiny::textInput("proj_name_load", label = "Project Name" ),
            shiny::actionButton("load_button", "Load Project", align = "center"))

        })
      }
    })


    volumes <- getVolumes()() # this makes the directory at the base of your computer.
    abs_path <- reactiveValues()
    abs_path$path <- "hey"

    observeEvent(input$folder, {
      shinyDirChoose(input, 'folder', roots=volumes, filetypes=c('', 'txt'))
      abs_path$path <- shiny::reactive({shinyFiles::parseDirPath(volumes, input$folder)})
      output$value <- renderText(abs_path$path())
    })



    ### Creating a new project for In Vitro data :
    observeEvent(input$creating, {

      project$name <- input$proj_name


      project$dir_path <- paste(root_path, project$name, sep = "/")

      project$db_file <- paste0(project$name, ".sqlite")


      '%notin%' <- Negate('%in%')
      if(is.null(input$folder)) {}

      else if("meta.csv" %notin% list.files( abs_path$path() )) {
        output$folder_warning <- shiny::renderUI({"Folder not correct. Check the provided path and/or the presence of meta.csv file"
        })
      }

      else{
        output$folder_warning <- NULL
        folder <-  abs_path$path()


        if(length(list.files(project$dir_path)) == 0){
          dir.create(project$dir_path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
        }


        if(input$trackbox == FALSE){

          df <- prepareData(folder, as.numeric(input$frame_rate),as.numeric(input$target_rate),
                            marker_thresh = as.numeric(input$mark_thresh), unit = input$unit)

        }

        if(input$trackbox == TRUE){
          df <- prepareData_track(folder, as.numeric(input$frame_rate), as.numeric(input$target_rate),
                                  marker_thresh = as.numeric(input$mark_thresh),unit = input$unit)

        }

        val <- setDT(df)[Cell_id == "A1aaa" & time_seconds <= 1, .N]
        orig_freq(val)
        calipR::saveData(df, paste(project$dir_path, project$db_file, sep = "/"), "df_full")

        df <- calipR::loading100(paste(project$dir_path, project$db_file, sep = "/"), "df_full")

        output$df_created <- shiny::renderDataTable({df},
                                                    options = list(scrollX = TRUE))

        db$load <- data.table::setDT(calipR::get_full_df(paste(project$dir_path, project$db_file,sep = "/"), "df_full"))

      }
    })

    ### Loading existing In vitro project
    observeEvent(input$load_button, {


      project$name <- input$proj_name_load

      project$dir_path <- paste(root_path, project$name, sep = "/")

      print(project$dir_path)
      project$db_file <- paste0(project$name, ".sqlite")


      if(project$db_file %in% list.files(project$dir_path)){

        output$warning_load <- NULL

        df <- calipR::loading100(paste(project$dir_path, project$db_file, sep = "/"), "df_full")

        if( "ID" %in% colnames(setDT(df))){
          ids <- unique(setDT(df)$ID)
          freq <- setDT(df)[ID == ids[[1]] & TIME_SECONDS <= 1, .N]
        }

        if("ID" %notin% colnames(setDT(df)) & "Cell_id" %in% colnames(setDT(df))){
          freq <- setDT(df)[Cell_id == "A1aaa" & time_seconds <= 1, .N]
        }
        orig_freq(freq)

        output$df_loaded <- shiny::renderDataTable({df},
                                                   options = list(scrollX = TRUE))

      }

      else{
        output$warning_load <- shiny::renderUI({"This project doesn't exist"})
      }
    })



    ############### Module to create the banks ######################

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


    ### End of the module to create the banks


    ############### Module to Annotate Data ######################

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


    ### End of the module to Annotate data

    db <- shiny::reactiveValues()

    db$load <- c(1,2)

    shiny::observeEvent(input$load_button, {

      if(project$db_file %in% list.files(project$dir_path)){
        output$warning_load <- NULL
        db$load <- data.table::setDT(calipR::get_full_df(paste(project$dir_path, project$db_file,sep = "/"), "df_full"))
      }

      else{
        output$warning_load <- shiny::renderUI({"This database doesn't exist"})

      }
    })


    ############### Visualization Module ###################

    # Visualizing a cell

    # Initializing a reactive plot :
    plot <- reactiveValues(p = NULL)
    db$aligned <- NULL
    ### Building this one :


    observeEvent(input$align_behavior, {

    if(input$align_behavior == TRUE){

    if(is.null(db$aligned) | "start_behavior" %notin% colnames(db$aligned)){

    if(dim(calipR::checkTable(paste(project$dir_path,project$db_file, sep ="/"), "'video_annotation'"))[1] == 0) {
print("youch")
    }

      else {
        print("youchi")
    behavior_data <- data.table::setDT(calipR::get_full_df(paste(project$dir_path, project$db_file,sep = "/"), "video_annotation"))
    print("behavior_data")
    print(behavior_data)
    print("db$load")
    print(db$load)

    video_status$annotation <- setDT(calipR::get_full_df(paste(project$dir_path, project$db_file, sep = "/"), "video_annotation"))

    annotated_videos <- unique(video_status$annotation[["ID"]])

    print("annotated_videos")
    print(annotated_videos)

    df <- lapply(annotated_videos, function(x)
                alignEpocs(behavior_data[ID == x],  db$load[Cell_id == x], x))

    df <- setDT(do.call(rbind,df))

    print("df")
    print(df)

    df <- df[order(ID)]
    calipR::saveData(df, paste(project$dir_path,project$db_file, sep ="/"), "aligned_behavioral_data")

    # Extracting behavioral events starts :

    db$aligned <- eventExtractR(df[VIDEO_FRAME != is.na(VIDEO_FRAME)])[[3]]

    print("db$aligned")
    print(db$aligned)
    }
    }

      else{

      }
    }
  })

#####

    observeEvent(input$fit_isos, {

      if(input$fit_isos == TRUE & length(db$load) != 2 & is.null(db$aligned)){

          if("ID" %in% colnames(db$load)){

            db$load[, fit_isos := stats::lm(CA_TRACE ~ ISOS_TRACE)$fitted.values, by = ID]
            db$load[, delta_f_f := (CA_TRACE - fit_isos) / fit_isos, by = ID]

            print(db$load)


            #calipR::saveData(df, paste(project$dir_path,project$db_file, sep ="/"), "aligned_behavioral_data")

          }
      }

      isnotnull <- Negate('is.null')
      if(input$fit_isos == TRUE & isnotnull(db$aligned)){

        print("yakoulor")
          db$aligned[, fit_isos := stats::lm(CA_TRACE ~ ISOS_TRACE)$fitted.values, by = ID]
          db$aligned[, delta_f_f := (CA_TRACE - fit_isos) / fit_isos, by = ID]

          print(db$aligned)


          #calipR::saveData(df, paste(project$dir_path,project$db_file, sep ="/"), "aligned_behavioral_data")

      }

      if(input$fit_isos == TRUE & is.null(db$aligned)){

        db$aligned <- db$load
        print("yakoulor")
        db$aligned[, fit_isos := stats::lm(CA_TRACE ~ ISOS_TRACE)$fitted.values, by = ID]
        db$aligned[, delta_f_f := (CA_TRACE - fit_isos) / fit_isos, by = ID]

        print(db$aligned)


        #calipR::saveData(df, paste(project$dir_path,project$db_file, sep ="/"), "aligned_behavioral_data")

      }

    })

    shiny::observeEvent(input$cell_num | input$align_behavior |input$fit_isos |input$delta_isos, {


      if(input$align_behavior == TRUE & length(db$load) != 2 & "start_behavior" %in% colnames(db$aligned)) {
print("yikaya")

        db$aligned <- db$aligned[order(ID)]
        df <- setDT(db$aligned[ID == unique(db$aligned[["ID"]])[[input$cell_num]]])

          output$plot_cell <- plotly::renderPlotly({

            cell <- unique(db$aligned[["ID"]])[input$cell_num]
            plot$p <- plot_aligned_fiber_data(db$aligned[ID == cell],
                                              "TIME_SECONDS",
                                              "CA_TRACE",
                                              isos = input$fit_isos,
                                              norm = input$delta_isos,
                                              behavior = input$align_behavior)

            plot$p

          })

          output$plot_isos <- plotly::renderPlotly({

            cell <- unique(db$aligned[["ID"]])[input$cell_num]
            plot$isos <- plot_aligned_fiber_data(db$aligned[ID == cell],
                                                 "TIME_SECONDS",
                                                 "ISOS_TRACE",
                                                 isos = input$fit_isos,
                                                 norm = input$delta_isos,
                                                 behavior = input$align_behavior)

            plot$isos

          })

        }

        if(input$align_behavior == FALSE  &  length(db$load) != 2 & input$fit_isos == FALSE & input$delta_isos == FALSE ){
          print("yikayaya")

          db$load <- db$load[order(Cell_id)]
          df <- setDT(db$load[Cell_id == unique(db$load[["Cell_id"]])[[input$cell_num]]])

          output$plot_cell <- plotly::renderPlotly({

            if("ID" %notin% colnames(df)){

              plot$p <- cell_plot_shiny(df)

            }

            if("ID" %in% colnames(df)){

              cell <- unique(db$load[["ID"]])[input$cell_num]

              plot$p <- plot_aligned_fiber_data(db$load[ID == cell],
                                                "TIME_SECONDS",
                                                "CA_TRACE",
                                                isos = input$fit_isos,
                                                norm = input$delta_isos,
                                                behavior = input$align_behavior)
            }

            plot$p

          })

        }

      if(input$align_behavior == FALSE  &  length(db$load) != 2 & input$fit_isos == TRUE){

        db$aligned <- db$aligned[order(ID)]
        df <- setDT(db$aligned[ID == unique(db$aligned[["ID"]])[[input$cell_num]]])

        output$plot_cell <- plotly::renderPlotly({

          cell <- unique(db$aligned[["ID"]])[input$cell_num]
          plot$p <- plot_aligned_fiber_data(db$aligned[ID == cell],
                                            "TIME_SECONDS",
                                            "CA_TRACE",
                                            isos = input$fit_isos,
                                            norm = input$delta_isos,
                                            behavior = input$align_behavior)

          plot$p

        })

      }

      #}
    })


    preprocessData <- reactiveValues(data = data.table(x = 0))
    ### Filtering this cell
    shiny::observeEvent(input$filter, {

      'isnotdt' <- Negate('is.data.table')

      if(length(db$load) != 2) {

        preprocessData$data <- db$load[Cell_id == unique(db$load[["Cell_id"]])[[input$cell_num]]]
        preprocessData$data_bis <- preprocessData$data

        if("ID" %notin% colnames(preprocessData$data)){
          setDT(preprocessData$data_bis)[,Mean_Grey := dplR::pass.filt(y = preprocessData$data[["Mean_Grey"]], W = input$filter, type = "low")]
        }
        if("ID" %in% colnames(preprocessData$data)){
          print(orig_freq())
          setDT(preprocessData$data_bis)[,CA_TRACE := dplR::pass.filt(y = preprocessData$data[["CA_TRACE"]], W = input$filter, type = "low")]
        }
        preprocessData$data_bis <-  downsampleCaData(preprocessData$data_bis, orig_freq(), input$downslider)

        output$plot_cell_filter <- plotly::renderPlotly({

          if("ID" %notin% colnames(preprocessData$data_bis)){
            p <- cell_plot_shiny(preprocessData$data_bis)


          }

          if("ID" %in% colnames(preprocessData$data_bis)){
            p <- plot_fiber_data(preprocessData$data_bis, "TIME_SECONDS", "CA_TRACE")


          }

          p


        })

      }

    })


    output$downsampling <- shiny::renderUI( {

      shiny::sliderInput("downslider",label = "Try Different Downsampling Values (Hz)", min = 0, max = 100, value = orig_freq(),step = 1 )

    })




    shiny::observeEvent(input$downslider, {


      if(length(preprocessData$data) > 1){

        preprocessData$data_bis <-  downsampleCaData(preprocessData$data, orig_freq(), input$downslider)
        #preprocessData$data_bis[,Time_frame_stim := seq(1,.N), by = .(Cell_id,stimulus)]

        output$plot_cell_filter <- plotly::renderPlotly({

          if("ID" %notin% colnames(preprocessData$data_bis)){
            p <- cell_plot_shiny(preprocessData$data_bis)


          }

          if("ID" %in% colnames(preprocessData$data_bis)){
            p <- plot_fiber_data(preprocessData$data_bis, "TIME_SECONDS", "CA_TRACE")


          }
          p

        })
      }
    })

    ### Saving the dataset with these new parameters
    shiny::observeEvent(input$saveFilteredData, {

      if(dim(calipR::checkTable(paste(project$dir_path,project$db_file, sep ="/"), "'df_full'"))[1] == 0) {
        print("yakoul")

        }
      else{
        print("yakil")
      df_full <- calipR::get_full_df(paste(project$dir_path,project$db_file,sep="/"),
                                     "df_full")


      df_full <- setDT(df_full)[, Mean_Grey := dplR::pass.filt(y = Mean_Grey, W = input$filter, type = "low"), by = Cell_id ]
      df_full <-  downsampleCaData(df_full, orig_freq(), input$downslider)

      saveData(df_full, paste(project$dir_path, project$db_file, sep = "/"), "df_full")
}
    })


    bank_path <- reactiveValues()


    shiny::observeEvent(input$patMatch_opt, {


      if(input$patMatch_opt == TRUE){

        bank_path$pos_path <- "path to positive bank"
        bank_path$neg_path <- "path to negative bank"


        ready$pos <- FALSE
        ready$neg <- FALSE
        output$posBank_field <- shiny::renderUI({

          fluidRow(
            column(4,
                   list(shinyFilesButton('posbank_file', 'Select positive bank file', 'Positive Bank', multiple = FALSE),
                        shiny::div(style = "width: 165px ;", shiny::verbatimTextOutput("bpp")))),

            column(4,
                   list(shinyFilesButton('negbank_file', 'Select negative bank file', 'Negative Bank', multiple = FALSE),
                        shiny::div(style = "width: 168px ;",  shiny::verbatimTextOutput("bpn")))))

        })

        observeEvent(input$posbank_file, {

          shinyFileChoose(input, 'posbank_file', roots=volumes, filetypes=c('', 'txt', 'RDS', 'rds'))
          bank_path$pos_path <- shiny::reactive({shinyFiles::parseFilePaths(volumes, input$posbank_file)
          })


          if(length(bank_path$pos_path()$datapath != 0)){
            output$bpp <- renderPrint(bank_path$pos_path()$datapath[[1]])
            ready$pos <- TRUE
          }

        })

        observeEvent(input$negbank_file,{
          shinyFileChoose(input, 'negbank_file', roots=volumes, filetypes=c('', 'txt', 'RDS', 'rds'))
          bank_path$neg_path <- shiny::reactive({shinyFiles::parseFilePaths(volumes, input$negbank_file)})

          if(length(bank_path$neg_path()$datapath != 0)){
            output$bpn <- renderPrint(bank_path$neg_path()$datapath[[1]])
            ready$neg <- TRUE
          }
        })

      }

      if(input$patMatch_opt == FALSE){

        output$posBank_field <- NULL
        output$negBank_field <- NULL
      }
    })


    res_sim <- shiny::reactiveValues(res = NULL)

    shiny::observeEvent(input$sim, {


      if(project$dir_path == "no path"){
        output$warning_data_sim <- renderUI({"Create or load a project first"})

      }

      else{
        df_sub <- calipR::get_sub_df(paste(project$dir_path, project$db_file,sep = "/"),
                                     "df_full", input$n_cells)
        output$warning_data_sim <- renderUI({""})
      }




      if(input$patMatch_opt == TRUE){

        deconvolve_var <- "background_detrended"
        method <- "back"

        if (sum(c(ready$pos,ready$neg)) == 2) {

          output$warning_bank_sim <- NULL

          posBank <- readRDS(bank_path$pos_path()$datapath[[1]])
          posBank <- Filter(Negate(is.null), posBank)

          negBank <- readRDS(bank_path$neg_path()$datapath[[1]])
          negBank <- Filter(Negate(is.null), negBank)
        }

        else{
          output$warning_bank_sim <- renderUI({"Please provide bank files"})

        }
      }

      else{

        posBank <- list()
        negBank <- list()
        deconvolve_var <- "gam_detrended"
        method <- "gam"
        ready$pos <- TRUE
        ready$neg <- TRUE
      }

      if (sum(c(ready$pos,ready$neg)) == 2 & project$dir_path != "no path") {

        res_sim$res <- downstream_analysis(df_sub,rate = orig_freq(),  z_thresh = input$peak_thresh, reference = input$norm_method,
                                           delta_thresh = input$peak_thresh_delta, lambda = input$lambda, gam = input$gam,
                                           simulation = TRUE, pattern_matching = input$patMatch_opt,
                                           posBank = posBank, negBank = negBank,
                                           deconvolve_var = deconvolve_var,
                                           norm_var = method, method = method)
      }
    })

    observe({
      if(is.null(res_sim$res)){

      }

      else if (isnotnull(res_sim$res)) {

        output$responders <- shiny::renderUI({
          data <- res_sim$res
          responders <- unique(data[[1]]$Cell_id)
          shiny::selectInput(inputId = "responders", "Responders", responders)
        })


        output$non_responders <- shiny::renderUI({
          data <- res_sim$res
          cells <- unique(data[[2]]$Cell_id)
          responders <- unique(data[[1]]$Cell_id)
          non_responders <- cells %in% responders
          non_responders <- unlist(purrr::map2(cells, non_responders, function(x,y) if(y == FALSE){x}))
          shiny::selectInput(inputId = "non_responders", "Non Responders", non_responders)
        })


        output$stats_opt <- DT::renderDataTable({
          data <- res_sim$res

          res2_1 <- data[[3]][[1]]


        })

        output$stats_opt_auc <- DT::renderDataTable({
          data <- res_sim$res

          res2_2 <- data[[1]][, .(mean_auc = mean(auc, na.rm=TRUE),
                                  mean_max_peak = mean(peak_max, na.rm=TRUE)
          ), by = stimulus]


        })
      }
    })



    shiny::observeEvent(input$plot_responders, {

      data <- res_sim$res


      output$plot_cell_sim <- shiny::renderPlot({

        cnames <- colnames(data[[2]])
        back_estim_opt <- c("gam_fit", "background")

        cnames_check <- back_estim_opt %in% cnames
        back_var <- back_estim_opt[[which(cnames_check == TRUE)]]


        p <- cell_plot(data[[2]], data[[1]], var = input$cell_plot_var,
                       cell = input$responders, line = back_var,
                       show_peak = input$show_peak)
        p

      })
    })

    shiny::observeEvent(input$plot_non_responders, {

      data <- res_sim$res




      output$plot_cell_sim <- shiny::renderPlot({

        cnames <- colnames(data[[2]])
        back_estim_opt <- c("gam_fit", "background")

        cnames_check <- back_estim_opt %in% cnames
        back_var <- back_estim_opt[[which(cnames_check == TRUE)]]

        if(input$patMatch_opt == TRUE){
          p <- cell_plot(data[[2]], data[[1]], var = input$cell_plot_var,
                         cell = input$non_responders, line = back_var,
                         show_peak = input$show_peak)
          p
        }

        if(input$patMatch_opt == FALSE){
          p <- cell_plot(data[[2]], data[[1]], var = input$cell_plot_var, cell = input$non_responders, line = back_var, show_peak = input$show_peak)
          p
        }


      })
    })

    bank_path_bis <- reactiveValues()

    shiny::observeEvent(input$patMatch_opt_bis, {


      if(input$patMatch_opt_bis == TRUE){

        bank_path_bis$pos_path <- "path to positive bank"
        bank_path_bis$neg_path <- "path to negative bank"

        ready$pos <- FALSE
        ready$neg <- FALSE

        output$posBank_field_bis <- shiny::renderUI({

          fluidRow(
            column(4,
                   list(shinyFilesButton('posbank_file_bis', 'Select positive bank file', 'Positive Bank bis', multiple = FALSE),
                        shiny::div(style = "width: 165px ;",shiny::verbatimTextOutput("bpp_bis")))),

            column(4,
                   list(
                     shinyFilesButton('negbank_file_bis', 'Select negative bank file', 'Negative Bank bis', multiple = FALSE),
                     shiny::div(style = "width: 168px ;",shiny::verbatimTextOutput("bpn_bis")))))

        })

        observeEvent(input$posbank_file_bis,{

          shinyFileChoose(input, 'posbank_file_bis', roots=volumes, filetypes=c('', 'txt', 'RDS', 'rds'))

          bank_path_bis$pos_path <- shiny::reactive({shinyFiles::parseFilePaths(volumes, input$posbank_file_bis)
          })


          if(length(bank_path_bis$pos_path()$datapath != 0)){
            output$bpp_bis <- renderPrint(bank_path_bis$pos_path()$datapath[[1]], width = 10)
            ready$pos <- TRUE
          }

        })

        observeEvent(input$negbank_file_bis,{
          shinyFileChoose(input, 'negbank_file_bis', roots=volumes, filetypes=c('', 'txt', 'RDS', 'rds'))
          bank_path_bis$neg_path <- shiny::reactive({shinyFiles::parseFilePaths(volumes, input$negbank_file_bis)})

          if(length(bank_path_bis$neg_path()$datapath != 0)){
            output$bpn_bis <- renderPrint(bank_path_bis$neg_path()$datapath[[1]], width = 10)
            ready$neg <- TRUE
          }
        })
      }



      if(input$patMatch_opt_bis == FALSE){

        output$posBank_field_bis <- NULL
        output$negBank_field_bis <- NULL
      }
    })





    shiny::observeEvent(input$sim_bis, {

      if (project$dir_path != "no path") {

        df_sub_bis <- get_cell(input$cell_opt, paste(project$dir_path,project$db_file,sep = "/"),
                               "df_full")
        output$warning_data_sim_bis <- renderUI({""})
      }

      else{
        output$warning_data_sim_bis <- renderUI({"Create or load a project first"})

      }

      if(input$patMatch_opt_bis == TRUE){

        deconvolve_var <- "background_detrended"
        method <- "back"

        if (sum(c(ready$pos,ready$neg)) == 2) {

          posBank <- readRDS(bank_path_bis$pos_path()$datapath[[1]])
          posBank <- Filter(Negate(is.null), posBank)

          negBank <- readRDS(bank_path_bis$neg_path()$datapath[[1]])
          negBank <- Filter(Negate(is.null), negBank)

          output$warning_bank_sim_bis <- renderUI({""})
        }

        else{
          output$warning_bank_sim_bis <- renderUI({"Please provide bank files"})

        }

      }

      else{

        posBank <- list()
        negBank <- list()
        deconvolve_var <- "gam_detrended"
        method <- "gam"
        ready$pos <- TRUE
        ready$neg <- TRUE
      }

      if (sum(c(ready$pos,ready$neg)) == 2 & project$dir_path != "no path") {

        res_sim$res_bis <- downstream_analysis(df_sub_bis, rate = orig_freq(), z_thresh = input$peak_thresh_bis,reference = input$norm_method_bis,
                                               delta_thresh = input$peak_thresh_bis_delta, lambda = input$lambda_bis,
                                               gam = input$gam_bis,simulation = TRUE, one_cell = TRUE,
                                               pattern_matching = input$patMatch_opt_bis,
                                               posBank = posBank, negBank = negBank,
                                               deconvolve_var = deconvolve_var,
                                               method = method,
                                               norm_var = method)
      }
    })


    shiny::observeEvent(input$plot_simulation_bis, {

      data <- res_sim$res_bis



      output$plot_cell_sim_bis <- shiny::renderPlot({

        cnames <- colnames(data[[2]])
        back_estim_opt <- c("gam_fit", "background")

        cnames_check <- back_estim_opt %in% cnames
        back_var <- back_estim_opt[[which(cnames_check == TRUE)]]


        if(input$patMatch_opt_bis == TRUE){
          p <- cell_plot(data[[2]], data[[1]], var = input$cell_plot_var, cell = input$cell_opt, line = back_var, show_peak = input$show_peak_bis)
          p
        }

        if(input$patMatch_opt_bis == FALSE){
          p <- cell_plot(data[[2]], data[[1]], var = input$cell_plot_var, cell = input$cell_opt, line = back_var, show_peak = input$show_peak_bis)
          p
        }

      })
    })


    ### Analyze the full dataset
    res_full <- shiny::reactiveValues(res = NULL)


    bank_path_full <- reactiveValues()
    ### This block codes alows to launch the analysis when the button analyze dataset is clicked on. It runs the whole analysis, on the whole dataset

    ready <- reactiveValues()
    ready$pos <- FALSE
    ready$neg <- FALSE

    shiny::observeEvent(input$patMatch, {

      if(input$patMatch == TRUE){

        bank_path_full$pos_path <- "path to positive bank"
        bank_path_full$neg_path <- "path to negative bank"

        ready$pos <- FALSE
        ready$neg <- FALSE

        output$posBank_field_full <- shiny::renderUI({

          list(
            tags$h6(shiny::div(style= "color: white; padding-left: 15px;", "Select the positive and negative banks")),
            fluidRow(
              column(4, shiny::div(style= "width: 250px;",
                                   shinyFilesButton('posbank_file_full', 'positive', 'Positive Bank', multiple = FALSE))),
              column(4, shiny::div(style= "width: 250px;margin-left: 35px;",
                                   shinyFilesButton('negbank_file_full', 'negative', 'Negative Bank', multiple = FALSE)))),

            fluidRow(column(4, shiny::div(style= "width: 87px; padding-left: 15px;",
                                          shiny::verbatimTextOutput("bpp_full"))),
                     column(4, shiny::div(style= "width: 77px; margin-left: 50px;",
                                          shiny::verbatimTextOutput("bpn_full"))))
          )


        })


        observeEvent(input$posbank_file_full, {

          shinyFileChoose(input, 'posbank_file_full', roots=volumes, filetypes=c('', 'txt', 'RDS', 'rds'))
          bank_path_full$pos_path <- shiny::reactive({shinyFiles::parseFilePaths(volumes, input$posbank_file_full)
          })

          if(length(bank_path_full$pos_path()$datapath != 0)){
            output$bpp_full <- renderPrint(bank_path_full$pos_path()$datapath[[1]])
            ready$pos <- TRUE
          }

        })

        observeEvent(input$negbank_file_full,{
          shinyFileChoose(input, 'negbank_file_full', roots=volumes, filetypes=c('', 'txt', 'RDS', 'rds'))
          bank_path_full$neg_path <- shiny::reactive({shinyFiles::parseFilePaths(volumes, input$negbank_file_full)})

          if(length(bank_path_full$neg_path()$datapath != 0)){
            output$bpn_full <- renderPrint(bank_path_full$neg_path()$datapath[[1]])
            ready$neg <- TRUE
          }
        })

      }

      if(input$patMatch == FALSE){

        output$posBank_field_full <- NULL
        output$negBank_field_full <- NULL
      }


    })




    res <- shiny::observeEvent(input$ana_full_button, {

      if(project$dir_path == "no path"){
        output$warning_data <- renderUI({"Create or load a project first"})

      }

      else{

        df_full <- calipR::get_full_df(paste(project$dir_path,project$db_file,sep="/"),
                                       "df_full")

        output$warning_data <- renderUI({""})

      }

      if(input$patMatch == TRUE){



        deconvolve_var <- "background_detrended"
        method <- "back"


        if (sum(c(ready$pos,ready$neg)) == 2) {

          output$warning_bank <- NULL

          posBank <- readRDS(bank_path_full$pos_path()$datapath[[1]])
          posBank <- Filter(Negate(is.null), posBank)

          negBank <- readRDS(bank_path_full$neg_path()$datapath[[1]])
          negBank <- Filter(Negate(is.null), negBank)

        }

        else{

          output$warning_bank <- renderUI({"Please provide bank files"})

        }
      }

      else{

        posBank <- list()
        negBank <- list()
        deconvolve_var <- "gam_detrended"
        method <- "gam"
        ready$pos <- TRUE
        ready$neg <- TRUE

      }


      if (sum(c(ready$pos,ready$neg)) == 2 & project$dir_path != "no path") {

        res_full$res <- downstream_analysis(df_full,rate = orig_freq(), z_thresh = input$peak_thresh_full_z,reference = input$norm_method_full,
                                            delta_thresh = input$peak_thresh_full_delta, lambda = input$lambda_full, gam = input$gam_full,
                                            pattern_matching = input$patMatch, posBank = posBank, negBank = negBank,
                                            deconvolve_var = deconvolve_var,
                                            method=method,
                                            norm_var = method)


        # Extracting and saving the data table containing one row for each peak with the informations
        #about the peak
        res1 <- res_full$res[[1]]

        data.table::setDT(res1)[, peak_frames := NULL]


        saveData(res1, paste(project$dir_path,project$db_file, sep ="/"), "peak_res",
                 over = TRUE, append = FALSE)



        # Extracting and saving the full data table updated
        res2 <- res_full$res[[2]]
        res2 <- data.table::setDT(res2)[, peak_frames := NULL]

        calipR::saveData(res2, paste(project$dir_path,project$db_file, sep ="/"), "df_final")


        res3_1 <- data.table::setDT(res_full$res[[3]][[1]])

        calipR::saveData(res3_1, paste(project$dir_path,project$db_file, sep ="/"), "stats_desc_final")



        res3_3_1 <- data.table::setDT(res_full$res[[3]][[2]][[1]])
        calipR::saveData(res3_3_1, paste(project$dir_path,project$db_file, sep ="/"), "overall_q")

        res3_3_2 <- data.table::setDT(res_full$res[[3]][[2]][[2]])
        calipR::saveData(res3_3_2, paste(project$dir_path,project$db_file, sep ="/"), "pairwise")

      }
    })


    # retrieving full dataset and peaks dataset :


    result <- reactiveValues()

    res <- shiny::eventReactive(input$update_button,{

      if(dim(calipR::checkTable(paste(project$dir_path,project$db_file, sep ="/"), "'df_final'"))[1] == 0) {
      }
      else{

        path <- paste(project$dir_path,project$db_file, sep ="/")

        full <- calipR::get_full_df(path, "df_final")
        peaks <- calipR::get_full_df(path, "peak_res")

        result$full <- data.table::setDT(full)
        result$peaks <- data.table::setDT(peaks)

      }

      if(input$base_resp == TRUE){
        peaks_wo_base <- data.table::setDT(calipR::base_resp.rm(result$peaks, result$full)[[1]])
        full_wo_base <- data.table::setDT(calipR::base_resp.rm(peaks, full)[[2]])

        res <- Analyze_Responses(peaks_wo_base, full_wo_base, var_list = input$grouping_var)
      }

      else{
        res <- Analyze_Responses(result$peaks, full, var_list = input$grouping_var)
      }
      res



    })


    observeEvent(input$update_button, {

      output$spinner <- shiny::renderUI({shinycssloaders::withSpinner(DT::dataTableOutput("resp_count"),
                                                                      type = 6)})

      output$resp_count <- DT::renderDataTable({


        if(dim(calipR::checkTable(paste(project$dir_path,project$db_file, sep ="/"), "'stats_desc_final'"))[1] == 0) {}
        else{

          result$resp_desc <- res()[[1]]

          DT::datatable({result$resp_desc},
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

        result$general_model <- res()[[2]][[1]]

        DT::datatable({result$general_model},
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

      }
      )

      output$post_hoc_mcnemar <- DT::renderDataTable({


        result$pw_mcnemar <- res()[[2]][[2]]

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

      })



      ### Computing dual proportions (proportion of cells responding to one stimulus that also responds to another stimulus)

      output$stim_list_1 <- shiny::renderUI({

        if(dim(calipR::checkTable(paste(project$dir_path,project$db_file, sep ="/"), "'peak_res'"))[1] == 0) {}
        else{

          stim_list <- unique(result$peaks[["spike_stimulus"]])


          shiny::selectInput(inputId = "stim_list_1", "Stimulus 1", stim_list)


        }
      })


      output$stim_list_2 <- shiny::renderUI({

        if(dim(calipR::checkTable(paste(project$dir_path,project$db_file, sep ="/"), "'peak_res'"))[1] == 0) {}
        else{


          stim_list <- unique(result$peaks[["spike_stimulus"]])

          shiny::selectInput(inputId = "stim_list_2", "Stimulus 2", stim_list)
        }
      })


      t <- shiny::eventReactive(input$dual_button, {

        if(dim(calipR::checkTable(paste(project$dir_path,project$db_file, sep ="/"), "'peak_res'"))[1] == 0) {}
        else{

          g <- result$peaks

          if(input$base_resp_dual == TRUE){
            g <- calipR::base_resp.rm(result$peaks, result$full)[[1]]


          }

          t <- dual_prop(g, input$stim_list_1, input$stim_list_2)
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



    output$clustering_var <- shiny::renderUI({

      if(length(result) == 0) {

      }

      else{

        list_var <- names(result$full)

        shiny::selectInput(inputId = "clustvar", "Variable used for clustering", list_var)

      }
    })




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

    output$cells_to_rm <- shiny::renderUI({

      if(length(result$peaks[["Cell_id"]]) > 1){

        selectInput("c_to_rm",label = "If you want to exclude specific cells from the analysis",
                    choices = result$peaks[["Cell_id"]], multiple = TRUE)
      }
    })

    shiny::observeEvent(input$clustplot_button, {

      responding_cells <- unique(result$peaks[["Cell_id"]])

      dt <- result$full[Cell_id %in% responding_cells]

      '%notin%' <- Negate('%in%')
      final <- prepareClustData(dt[Cell_id %notin% input$c_to_rm], input$clustvar, norm = input$normclust)


      if(input$set_seed){

        set.seed(as.integer(input$seed))
      }

      clust_res <- dtwclust::tsclust(final, type = "partitional", k = as.integer(input$nclust), distance = input$dist_type,
                                     centroid = "dba")


      output$clustplot <- shiny::renderPlot({

        p <- plot(clust_res, type="sc")
        p

      })

      ### Identifying cells in each cluster :

      result$peaks <- clustCellID(clust_res, result$peaks, input$c_to_rm)



      # Preparing data for visualization :

      peak_sum <- result$peaks[, .(mean_auc = mean(auc, na.rm = TRUE),
                                   sd_auc = sd(auc, na.rm = TRUE),
                                   mean_peak_max = mean(peak_max, na.rm = TRUE),
                                   sd_peak_max = sd(peak_max, na.rm = TRUE),
                                   mean_peak_duration = mean((peak_end - peak_start), na.rm = TRUE),
                                   sd_peak_duration = sd((peak_end - peak_start), na.rm = TRUE),
                                   n_resp = length(unique(.SD$Cell_id))), by = .(stimulus, cluster)]


      n_stim <- length(unique(result$peaks[["stimulus"]]))

      result$peaks[, n_stim_by_clust := length(unique(.SD$stimulus)), by = .(cluster)]

      n_resp_by_clust <- result$peaks[, .(n_resp_by_clust = rep(length(unique(.SD$Cell_id)), each = .SD$n_stim_by_clust)), by = .(cluster)]

      n_resp_by_clust <- n_resp_by_clust[order(cluster)]
      peak_sum <- peak_sum[order(cluster)]


      peak_sum <- cbind(peak_sum,n_resp_by_clust)

      peak_sum[, prop_by_clust := n_resp/n_resp_by_clust]


      ### Visualizing results by cluster

      output$var_selector_clust <- shiny::renderUI({

        list(
          shiny::selectInput(inputId = "x_var_bis", label = NULL, names(peak_sum)),
          shiny::selectInput(inputId = "y_var_bis", label = NULL, names(peak_sum)),
          shiny::selectInput(inputId = "z_var_bis", label = NULL, names(peak_sum))
        )
      })

      output$viz_clust_auc <- plotly::renderPlotly({


        plotly::plot_ly(

          type = 'bar',

          x = peak_sum[[input$x_var_bis]],

          y = peak_sum[[input$y_var_bis]],
          text = paste("Cluster: ", peak_sum[["cluster"]],

                       "<br>Stimulus:  ", peak_sum[["stimulus"]],

                       "<br>AUC: ", peak_sum[["mean_auc"]]),

          hoverinfo = 'text',

          marker = list(size = 2),


          color = peak_sum[[input$z_var_bis]],

        ) %>%
          plotly::layout(barmode ="group", yaxis = list(automargin = TRUE),
                         xaxis = list(automargin = TRUE), bargap = -2, bargroupgap = 0)



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



  }
  shiny::shinyApp(ui, server)

}


