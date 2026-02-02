#' vizRawModuleUI
#'
#' UI of the visualization module
#'
#' @param id
#'
#' @returns
#' @export
#'
#' @examples
vizRawModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
      shiny::fluidRow(
        shinydashboard::box(
          title = "Plotting Cells",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          shiny::numericInput(
            ns("cell_num"),
            label = "cell_number",
            value = 1,
            min = 1
          ),
          shiny::checkboxInput(ns("align_behavior"), label = "align behavior and fiber photometry data"),
          shiny::checkboxInput(ns("fit_isos"), label = "fit isosbestic trace"),
          shiny::checkboxInput(ns("delta_isos"), label = "normalize with isosbestic (f) channel (delta f/f)"),
          shiny::checkboxInput(ns("update_database"), label = "update database"),

          shiny::textInput(
            ns("linesToRm"),
            label = "Number of points to remove at the beginning",
            placeholder = "Enter the number of lines you want to remove at the start of the trace",
            value = "10"
          ),
          plotly::plotlyOutput(ns("plot_cell"))
        )
      ),

      shiny::fluidRow(
        shinydashboard::box(
          title = "Plotting Isosbestic Trace",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          plotly::plotlyOutput(ns("plot_isos"))
        )
      ),

      shiny::fluidRow(
        shinydashboard::box(
          title = "Filtering Trace",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          plotly::plotlyOutput(ns("plot_cell_filter"))
        )
      )
    )
}


#' vizRawModuleServer
#'
#' Server of the visualization module
#'
#' @param id
#' @param db
#' @param project
#' @param orig_freq
#' @param filter
#' @param downslider
#' @param video_status
#'
#' @returns
#' @export
#'
#' @examples
vizRawModuleServer <- function(id, db, project, orig_freq, filter, downslider, video_status) {
  moduleServer(id, function(input, output, session) {
    # Visualizing a cell

    # Initializing a reactive plot :
    plot <- reactiveValues(p = NULL)

    isnotnull <- Negate("is.null")

    ### Building this one :
    print("orig_freq()")
    print(orig_freq())

    observeEvent(input$align_behavior, {

      print("db$load")
      print(db$load)

      if(input$align_behavior == TRUE){

        print("db$aligned")
        print(db$aligned)

        if(is.null(db$aligned) | "start_behavior" %notin% colnames(db$aligned) | input$update_database ==TRUE){
          print("youlabouille")
          print(project$dir_path)
          print(project$db_file)
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

            video_status$annotation <- data.table::setDT(calipR::get_full_df(paste(project$dir_path, project$db_file, sep = "/"), "video_annotation"))

            annotated_videos <- unique(video_status$annotation[["ID"]])
            annotated_videos_ids <- db$load[Cell_id %in% annotated_videos]$ID

            print("annotated_videos")
            print(annotated_videos)



            df <- lapply(annotated_videos, function(x)
              alignEpocs(behavior_data[ID == x],  db$load[Cell_id == x], db$load[Cell_id == x]$ID))

            df <- setDT(do.call(rbind,df))

            print("df")
            print(df)

            df <- df[order(ID)]


            # Extracting behavioral events starts :

            extracted_events <- eventExtractR(df[VIDEO_FRAME != is.na(VIDEO_FRAME)])

            db$aligned <- extracted_events[[length(extracted_events)]]
            #db$load <-  db$aligned
            print("db$aligned")
            print(db$aligned)

            db$aligned <- db$aligned[order(ID)]
            calipR::saveData(db$aligned, paste(project$dir_path,project$db_file, sep ="/"), "aligned_behavioral_data")

            start_idx <- as.integer(input$linesToRm)

            print("youk")
            db$aligned <- data.table::setDT(db$aligned)[, to_keep := ifelse(TIME_SECONDS > start_idx, TRUE, FALSE), by = .(ID,group)][to_keep == TRUE]


            # Extracting behavioral events starts :
            print("head(db$aligned)")
            print(head(db$aligned))

          }
        }

        else{
          start_idx <- as.integer(input$linesToRm)

          print("youko")
          db$aligned <- data.table::setDT(db$aligned)[, to_keep := ifelse(TIME_SECONDS > start_idx, TRUE, FALSE), by = ID][to_keep == TRUE]


        }
      }
    })

    #####






    observeEvent(input$fit_isos, {

      if(input$fit_isos == TRUE & length(db$load) != 2 & is.null(db$aligned)){

        db$load <- db$load[order(unique_ID)]

        if("ID" %in% colnames(db$load)){

          db$load[, fit_isos := stats::lm(CA_TRACE ~ ISOS_TRACE)$fitted.values, by = .(ID,group)]
          db$load[, delta_f_f := (CA_TRACE - fit_isos) / fit_isos, by = .(ID,group)]

          print(db$load)


          #calipR::saveData(df, paste(project$dir_path,project$db_file, sep ="/"), "aligned_behavioral_data")

        }
      }

      isnotnull <- Negate('is.null')
      if(input$fit_isos == TRUE & isnotnull(db$aligned)){

        print("yakoular")
        print(db$aligned)
        db$aligned <- db$aligned[order(unique_ID)]

        db$aligned[, fit_isos := stats::lm(CA_TRACE ~ ISOS_TRACE)$fitted.values, by = .(ID,group)]
        db$aligned[, delta_f_f := (CA_TRACE - fit_isos) / fit_isos, by = .(ID, group)]

        print(db$aligned)


        #calipR::saveData(df, paste(project$dir_path,project$db_file, sep ="/"), "aligned_behavioral_data")

      }

      if(input$fit_isos == TRUE & is.null(db$aligned)){

        db$load <- db$load[order(ID)]

        db$aligned <- db$load
        print("yakoulor")
        #print(db$aligned)
        print(data.table::setDT(db$aligned))
        data.table::setDT(db$aligned)[, fit_isos := stats::lm(CA_TRACE ~ ISOS_TRACE)$fitted.values, by = .(ID,group)]
        db$aligned[, delta_f_f := (CA_TRACE - fit_isos) / fit_isos, by = .(ID,group)]

        print(db$aligned)


        #calipR::saveData(df, paste(project$dir_path,project$db_file, sep ="/"), "aligned_behavioral_data")

      }

    })

    shiny::observeEvent(input$cell_num,{
    if(is.null(db$aligned) & input$fit_isos == FALSE){
    db$aligned <- db$load
    }

    else{
      db$aligned <- NULL
    }
    })

    shiny::observeEvent(input$cell_num | input$align_behavior |input$fit_isos |input$delta_isos, {

      if(isnotnull(db$aligned)){
        print("something_changed2")
      if(input$align_behavior == TRUE & length(db$load) != 2 & "start_behavior" %in% colnames(db$aligned)) {
        print("yikaya")

       # db$load <- db$load[order(unique_ID)]


        #db$aligned <- db$aligned[order(ID)]


        #df <- setDT(db$aligned[ID == unique(db$aligned[["ID"]])[[input$cell_num]]])


  print("youk")
        output$plot_cell <- plotly::renderPlotly({

          print("db$aligned")
          print(db$aligned)

          print("unique(db$aligned[['ID'']])")
          print(unique(db$aligned[["ID"]]))

          #id <- unique(db$aligned[["ID"]])[input$cell_num]
          #db$aligned <- db$aligned[order(unique_ID)]
          cell <- unique(db$load[["unique_ID"]])[input$cell_num]

          print("cell")
          print(cell)


          #cell <- unique(db$aligned[["ID"]])[input$cell_num]
          plot$p <- plot_aligned_fiber_data(db$aligned[unique_ID == cell],
                                            "TIME_SECONDS",
                                            "Mean_Grey",
                                            isos = input$fit_isos,
                                            norm = input$delta_isos,
                                            behavior = input$align_behavior)

          plot$p

        })

        output$plot_isos <- plotly::renderPlotly({

          cell <- unique(db$aligned[["unique_ID"]])[input$cell_num]

          plot$isos <- plot_aligned_fiber_data(db$aligned[unique_ID == id],
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

        db$load <- db$load[order(unique_ID)]
        df <- setDT(db$load[unique_ID == unique(db$load[["unique_ID"]])[[input$cell_num]]])

        df_bis <- df
        View(df_bis)

        output$plot_cell <- plotly::renderPlotly({

          if("ID" %notin% colnames(df)){

            plot$p <- cell_plot_shiny(df)

          }

          if("ID" %in% colnames(df)){

            cell <- unique(db$load[["unique_ID"]])[input$cell_num]

            print("cell")
            print(cell)

            View(db$load[unique_ID == cell])

            plot$p <- plot_aligned_fiber_data(db$load[unique_ID == cell],
                                              "TIME_SECONDS",
                                              "Mean_Grey",
                                              isos = input$fit_isos,
                                              norm = input$delta_isos,
                                              behavior = input$align_behavior)
          }

          plot$p

        })

      }


      if(input$align_behavior == FALSE  &  length(db$load) != 2 & input$fit_isos == TRUE){

        if("ID" %in% colnames(db$load)){

          db$load[, fit_isos := stats::lm(CA_TRACE ~ ISOS_TRACE)$fitted.values, by = .(ID,group)]
          db$load[, delta_f_f := (CA_TRACE - fit_isos) / fit_isos, by = .(ID,group)]
        }

        db$load <- db$load[order(unique_ID)]
        print("input$cell_num")
        print(input$cell_num)

       # df <- setDT(db$load[unique_ID == unique(db$load[["unique_ID"]])[[input$cell_num]]])

        output$plot_cell <- plotly::renderPlotly({

          cell <- unique(db$load[["unique_ID"]])[input$cell_num]
          plot$p <- plot_aligned_fiber_data(db$load[unique_ID == cell],
                                            "TIME_SECONDS",
                                            "Mean_Grey",
                                            isos = input$fit_isos,
                                            norm = input$delta_isos,
                                            behavior = input$align_behavior)

          plot$p

        })

      }


      }
    })


    preprocessData <- reactiveValues(data = data.table(x = 0))
    ### Filtering this cell
    shiny::observeEvent(filter() | downslider(), {
      print("youp")
      'isnotdt' <- Negate('is.data.table')

      if(length(db$load) != 2) {

        print("yo")
        preprocessData$data <- db$load[Cell_id == unique(db$load[["Cell_id"]])[[input$cell_num]]]
        preprocessData$data_bis <- preprocessData$data
        print("yol")

        if("ID" %notin% colnames(preprocessData$data)){
          setDT(preprocessData$data_bis)[,Mean_Grey := dplR::pass.filt(y = preprocessData$data[["Mean_Grey"]], W = filter(), type = "low")]
          print("yi")

          }
        if("ID" %in% colnames(preprocessData$data)){
          print("ya")
          print("downslider()")
          print(downslider())
          setDT(preprocessData$data_bis)[,CA_TRACE := dplR::pass.filt(y = preprocessData$data[["CA_TRACE"]], W = filter(), type = "low")]
        }
        preprocessData$data_bis <-  downsampleCaData(preprocessData$data_bis, orig_freq(), downslider())

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







  #  shiny::observeEvent(downslider(), {


  #    if(length(preprocessData$data) > 1){

  #      preprocessData$data_bis <-  downsampleCaData(preprocessData$data, orig_freq(), downslider())
        #preprocessData$data_bis[,Time_frame_stim := seq(1,.N), by = .(Cell_id,stimulus)]

  #      output$plot_cell_filter <- plotly::renderPlotly({
  #
  #        if("ID" %notin% colnames(preprocessData$data_bis)){
  #          p <- cell_plot_shiny(preprocessData$data_bis)


  #        }

  #        if("ID" %in% colnames(preprocessData$data_bis)){
  #          p <- plot_fiber_data(preprocessData$data_bis, "TIME_SECONDS", "CA_TRACE")


  #        }
  #        p

  #      })
  #    }
  #  })

    ### Saving the dataset with these new parameters
    shiny::observeEvent(input$saveFilteredData, {

      if(dim(calipR::checkTable(paste(project$dir_path,project$db_file, sep ="/"), "'df_full'"))[1] == 0) {
        print("yakoul")

      }
      else{
        print("yakil")
        df_full <- calipR::get_full_df(paste(project$dir_path,project$db_file,sep="/"),
                                       "df_full")


        print("orig_freq()")
        print(orig_freq())

        print("filter()")
        print(filter())

        print("df_full before filtering")
        print(head(df_full))

        df_full <- setDT(df_full)[, CA_TRACE := dplR::pass.filt(y = CA_TRACE, W = filter(), type = "low"), by = Cell_id ]
        df_full <- setDT(df_full)[, ISOS_TRACE := dplR::pass.filt(y = ISOS_TRACE, W = filter(), type = "low"), by = Cell_id ]

        df_full <- setDT(df_full)[, Mean_Grey := dplR::pass.filt(y = Mean_Grey, W = filter(), type = "low"), by = Cell_id ]

        print("df_full after filtering")
        print(head(df_full))

        df_full <-  downsampleCaData(df_full, orig_freq(), downslider())

        print("yish")


        print("df_full before saving")
        print(head(df_full))

        saveData(df_full, paste(project$dir_path, project$db_file, sep = "/"), "df_full")

        output$dt_filt <- shiny::renderDataTable({df_full})

        print("yosh")

      }
    })

  })
}
