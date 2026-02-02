
#' optimisationModuleUI
#'
#' UI of the optimisation module
#'
#' @param id
#'
#' @returns
#' @export
#'
#' @examples
optimisationModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
      shiny::fluidRow(
        shinydashboard::box(
          title = "Optimize Parameters",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          shiny::numericInput(
            ns("peak_thresh"),
            label = "Peak Threshold (z score)",
            value = 3,
            min = 0
          ),
          shiny::numericInput(
            ns("peak_thresh_delta"),
            label = "Peak Threshold (delta f/f)",
            value = 0,
            min = 0
          ),
          shiny::textInput(
            ns("lambda"),
            label = "Lambda",
            placeholder = "Enter the Lambda parameter for the Deconvolution (integer)",
            value = "1000"
          ),
          shiny::textInput(
            ns("gam"),
            label = "Gam",
            placeholder = "Enter the Gam parameter for the Deconvolution (double between 0-1)",
            value = "0.9"
          ),
          shiny::textInput(
            ns("n_cells"),
            label = "Number of cells",
            placeholder = "Enter the number of cells you want to run the stimulation on",
            value = "10"
          ),
          shiny::selectInput(
            inputId = ns("norm_method"),
            label = "Choose method to compute z-score",
            list(
              "Baseline Period" = "baseline",
              "Baseline Period without Peaks" = "estimate"
            )
          ),
          shiny::checkboxInput(ns("patMatch_opt"), label = "Background Estimation with SBPC"),
          shiny::uiOutput(ns('posBank_field')),
          shiny::uiOutput(ns('negBank_field')),
          shiny::checkboxInput(ns("show_peak"), label = "Show Peaks"),
          shiny::uiOutput(ns('responders')),
          shiny::uiOutput(ns('non_responders')),
          shiny::uiOutput(ns('warning_data_sim'), class = "warn-main"),
          shiny::uiOutput(ns('warning_bank_sim'), class = "warn-main"),
          shiny::actionButton(ns("sim"), "Simulate Analysis", align = "center"),
          shiny::actionButton(ns("plot_responders"), "Plot Responder", align = "center"),
          shiny::actionButton(ns("plot_non_responders"), "Plot Non Responder", align = "center")
        )
      ),

      shiny::fluidRow(
        shinydashboard::box(
          title = "Plot Window",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          shiny::selectInput(
            inputId = ns("cell_plot_var"),
            label = NULL,
            list(
              "Mean_Grey" = "Mean_Grey",
              "Delta_F/F" = "delta_f_f",
              "z_score" = "z_score",
              "Smooth_Delta_F/F" = "smooth_delta",
              "First_Derivative" = "first_derivative",
              "Smooth_First_Derivative" = "smooth_Diff",
              "Deconvolved_trace" = "deconvolved_trace"
            )
          ),
          shiny::plotOutput(outputId = ns("plot_cell_sim"))
        )
      ),

      shiny::fluidRow(
        shinydashboard::box(
          title = "Try other parameters on a given cell",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          shiny::textInput(ns("cell_opt"), label = "Cell", placeholder = "On which cell do you want to try new parameters ?"),
          shiny::numericInput(
            ns("peak_thresh_bis"),
            label = "Threshold (z_score)",
            value = 3,
            min = 0
          ),
          shiny::numericInput(
            ns("peak_thresh_bis_delta"),
            label = "Threshold (delta f/f)",
            value = 0,
            min = 0
          ),
          shiny::textInput(
            ns("lambda_bis"),
            label = "Lambda",
            placeholder = "Enter the Lambda parameter for the Deconvolution (integer)",
            value = "1000"
          ),
          shiny::textInput(
            ns("gam_bis"),
            label = "Gam",
            placeholder = "Enter the Gam parameter for the Deconvolution (double between 0-1)",
            value = "0.9"
          ),
          shiny::selectInput(
            inputId = ns("norm_method_bis"),
            label = "Choose method to compute z-score",
            list(
              "Baseline Period" = "baseline",
              "Baseline Period without Peaks" = "estimate"
            )
          ),
          shiny::checkboxInput(ns("patMatch_opt_bis"), label = "Background Estimation with SBPC"),
          shiny::uiOutput(ns('posBank_field_bis')),
          shiny::uiOutput(ns('negBank_field_bis')),
          shiny::checkboxInput(ns("show_peak_bis"), label = "Show Peaks"),
          shiny::uiOutput(ns('warning_data_sim_bis'), class = "warn-main"),
          shiny::uiOutput(ns('warning_bank_sim_bis'), class = "warn-main"),
          shiny::actionButton(ns("sim_bis"), "Simulate Analysis", align = "center"),
          shiny::actionButton(ns("plot_simulation_bis"), "Plot Cell", align = "center")
        )
      ),


      shiny::fluidRow(
        shinydashboard::box(
          title = "Plot Window",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          shiny::plotOutput(outputId = ns("plot_cell_sim_bis"))
        )
      ),

      shiny::fluidRow(
        shinydashboard::box(
          title = "Statistics",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          DT::dataTableOutput(ns("stats_opt"))
        )
      ),

      shiny::fluidRow(
        shinydashboard::box(
          title = "Statistics",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          DT::dataTableOutput(ns("stats_opt_auc"))
        )
      )
  )
}

#' optimisationModuleServer
#'
#' Server of the optimisation module
#'
#' @param id
#'
#' @returns
#' @export
#'
#' @examples
optimisationModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {

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
  })
}
