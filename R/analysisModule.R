
#' anaFullModuleUI
#'
#' UI of the in vitro analysis module
#'
#' @param id
#'
#' @returns
#' @export
#'
#' @examples
anaFullModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      shinydashboard::box(
        title = "Description of Responses",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        shiny::uiOutput(ns("spinner")),
        DT::dataTableOutput(ns("resp_group_stim")),
        DT::dataTableOutput(ns("peaks_by_class")),
        DT::dataTableOutput(ns("overall_q")),
        DT::dataTableOutput(ns("post_hoc_mcnemar")),
        shiny::selectInput(
          inputId = ns("grouping_var"),
          label = NULL,
          list(
            "Group" = "group",
            "Coverslip" = "coverslip",
            "Stimulus" = "stimulus",
            "Marker" = "marker_positive"
          ),
          multiple = TRUE
        ),
        shiny::actionButton(ns("update_button"), "Update", align = "right"),
        shiny::checkboxInput(ns("base_resp"), label = "Remove Baseline Responders")
      )
    ), shiny::fluidRow(
      shinydashboard::box(
        title = "Dual Proportions",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        shiny::uiOutput(ns("stim_list_1")),
        shiny::uiOutput(ns("stim_list_2")),
        shiny::actionButton(ns("dual_button"), "Compute Dual Proportions", align = "center"),
        shiny::br(),
        shiny::checkboxInput(ns("base_resp_dual"), label = "Remove Baseline Responders"),
        shiny::br(),
        DT::dataTableOutput(ns("dual_prop"))
      )
    ),
    ### Clustering module :
    shiny::fluidRow(
      shinydashboard::box(
        title = "Clustering",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        shiny::uiOutput('clustering_var'),
        shiny::checkboxInput("normclust", label = "Z Normalize Values"),
        shiny::textInput("nclust", "Number of Clusters"),
        shiny::uiOutput('cells_to_rm'),
        shiny::checkboxInput("set_seed", "Set Seed"),
        shiny::uiOutput('seed_field'),
        shiny::selectInput(
          "dist_type",
          "Distance Measure to Use",
          proxy::pr_DB$get_entry_name()
        ),
        shiny::actionButton("clustplot_button", "Launch Clustering", align = "right"),
        shiny::plotOutput(outputId = "clustplot"),
        shiny::uiOutput('export_csv')
      ),
      shiny::div(style = "height:1000px;")
    )
  )
}

#' anaFullModuleServer
#'
#' Server of the invitro analysis module
#'
#' @param id
#'
#' @returns
#' @export
#'
#' @examples
anaFullModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
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
  })
}
