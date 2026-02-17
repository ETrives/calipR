#' prepModuleUI
#'
#' UI of the data preparation module
#'
#' @param id
#'
#' @returns
#' @export
#'
#' @examples
prepModuleUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
  shiny::fluidRow(shiny::column(
    12,
    shinydashboard::box(
      title = "Project",
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      collapsible = T,
      shiny::uiOutput(ns("project_type")),
      shiny::tags$br(),
      shiny::tags$br(),
      shiny::uiOutput(ns("project_creation")),
      shiny::uiOutput(ns("project_loading")),
      shiny::uiOutput(ns("warning_load"), class = "warn-main")
    )
  )),

  shiny::fluidRow(shiny::column(
    12,
    shinydashboard::box(
      title = "Dataset Prepared",
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      collapsible = T,
      shiny::dataTableOutput(ns("df_created")),
      shiny::dataTableOutput(ns("df_loaded"))
    )
  )),

  shiny::fluidRow(
    shiny::column(
      12,
      shinydashboard::box(
        title = "Annotate Video",
        width = 6,
        solidHeader = TRUE,
        status = "primary",
        collapsible = T,
        shiny::uiOutput(ns("annotateVideo")),
        shiny::div(style = "height:1000px"),
        shiny::textOutput(ns("key_status"))
      ),

      shinydashboard::box(
        title = "Annotation Project",
        width = 6,
        solidHeader = TRUE,
        status = "primary",
        collapsible = T,
        shiny::dataTableOutput(ns("annotation_project")),
        shiny::div(style = "height:1000px")
      )
    )
  )
  )
}

#' prepModuleServer
#'
#' Server of the data preparation module
#'
#' @param id
#' @param create_input
#' @param load_input
#' @param project_type
#' @param db
#' @param project
#' @param orig_freq
#' @param video_status
#'
#' @returns
#' @export
#'
#' @examples
prepModuleServer <- function(id, create_input, load_input, project_type, db,
                             project, orig_freq, video_status){
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    if ("calipR" %in% installed.packages()[, "Package"]) {
      print("yur")
      package_path <- system.file(package = "calipR")
      print("yura")

    } else {
      package_path <- getwd()
    }

    script_path <- file.path(package_path, "python_scripts")

    reticulate::source_python(paste(script_path, "tdt_extraction.py", sep = "/"))
    reticulate::source_python(paste(script_path, "addEpocs.py", sep = "/"))
    reticulate::source_python(paste(script_path, "annotateVideo.py", sep = "/"))

    #### Data Preparation ##############

    '%notin%' <- Negate('%in%')
    'isnotnull' <- Negate('is.null')


    root_path <- paste0(getwd(),"/projects")
    sqlitePath <- getwd()


        creation_tab <- reactiveValues(elements = NULL)


        shiny::observeEvent(create_input(), {
          print("project_type()")
          print(project_type)
          print(create_input())
          create_in <- create_input()

          if(project_type == "invitro" & isTRUE(create_in)) {
            print("yolabou")
          creation_tab$elements <- list(
            shiny::textInput(ns("proj_name"), label = "Project Name" ),
            shiny::textInput(ns("frame_rate"), label = "Enter your frame rate (Hz)", placeholder = "e.g. 0.5" ),
            shiny::selectInput(ns("unit"), label = "Select the unit of the times indicated in meta",
                               choices = list("minutes" = "minutes", "seconds" = "seconds") ),
            shiny::verbatimTextOutput(ns("value")),
            shinyDirButton(ns('folder'), 'Select a folder', 'Please select a folder', FALSE),
            shiny::uiOutput(ns("folder_warning")),
            shiny::textInput(ns("mark_thresh"), label = "if you have a cellular marker, enter your threshold", placeholder = "e.g. 30"),
            shiny::checkboxInput(ns("trackbox"), label = "Check if you did ROI detection with Trackmate"),
            shiny::actionButton(ns("creating"), "Load & Tidy Data", align = "center")
          )

          creation_tab$videoElements <- NULL

          }


          if(project_type == "fiber" & isTRUE(create_in)) {
            print("youkarai")

          creation_tab$elements <- list(
            shiny::textInput(ns("proj_name"), label = "Project Name" ),
            shiny::textInput(ns("downsampleRaw"), label = "Enter the frame rate you want to downsample to (an integer, in Hz)", placeholder = "e.g. 10" ),
            shiny::textInput(ns("filterRaw"), label = "Enter the width of the low pass filter you want to apply to your data", placeholder = "e.g. 10" ),
            shinyDirButton(ns('folder'), 'Select a folder', 'Please select a folder', FALSE),
            shiny::uiOutput(ns("folder_warning")),
            shiny::actionButton(ns("creating_fiber"), "Load Fiber Photometry Data", align = "center")
          )


          creation_tab$videoElements <- list(
            shiny::actionButton(ns("loadVideoButton"), "Load Video Files", align = "center"),
            shiny::tags$br(),
            shiny::textInput(ns("videoPath"), label = "path to the video you want to annotate" ),
            shiny::textInput(ns("animal_id"), label = "ID of the animal's video your annotating (ex : 2426). It needs to match the name of the folder containing fiber data for this animal." ),
            shiny::textInput(ns("keys"), label =  "keyboard keys you want to set for your annotation (comma separated) (e.g. f,m)" ),
            shiny::actionButton(ns("annotateVideoButton"), "Annotate Video", align = "center"),
            shiny::actionButton(ns("save_video_annotation"), "Save Annotation", align = "center"),
            shiny::dataTableOutput(ns("annotated_Video"))

          )
          }

        })



        output$annotateVideo <- shiny::renderUI({

          creation_tab$videoElements
        })


        output$project_creation <- shiny::renderUI( {
          print("→ renderUI project_creation déclenché")
          print("Contenu de creation_tab$elements :")
          creation_tab$elements
        })

        video_status <- shiny::reactiveValues(l = list())

        observeEvent(input$loadVideoButton, {

          groups_path <- unique(db$load[["root_path"]])

          print("groups_path")
          print(groups_path)

          paths <- lapply(groups_path, function(x)  extractAllVideoPath(x))

          dt_list <- split(db$load[order(unique_ID)], db$load[order(unique_ID)]$unique_ID)
          print("dt_list")
          print(dt_list)

          db$load <- setDT(do.call(rbind, lapply(seq(1,length(dt_list)), function(x) setDT(dt_list[[x]])[, videoPath := unlist(paths)[x]])))
         # db$load[, videoPath := unlist(paths), by = unique_ID]

          print("db$load paths")
          print(db$load)

          if(dim(calipR::checkTable(paste(project$dir_path,project$db_file, sep ="/"), "'video_status'"))[1] == 0) {
            print("the table has not been found")

            print(db$load[["root_path"]][1])

            print("path before")

            print(paths)
            #paths <- lapply(paths, function(x) lapply(x, function(y) y))

            paths <- unlist(paths, recursive = FALSE)

            print("paths after")
            print(paths)

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
          if(project_type=="fiber"){

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


            groups <- list.files(abs_path$path())

            lapply(groups, function(x) extract_all_tdt_data(paste(abs_path$path(),x,sep = "/"),
                                                            destination_path = getwd(),
                                                            file_name = paste0(x, "_Extracted_TDT_Data.csv")))


            #extract_all_tdt_data(abs_path$path(), destination_path = getwd())
            df <- list()

            for(i in groups){
            df[[i]] <- fread(paste(getwd(), paste0(i,"_Extracted_TDT_Data.csv"), sep = "/"))[, Cell_id := ID][, Mean_Grey := CA_TRACE][, group := i][, root_path := paste(abs_path$path(),i,sep = "/")]
            }

            df <- do.call(rbind, df)

            colnames(df)[1] <- "time_frame"

            ids <- unique(setDT(df)$Cell_id)

            if("ID" %notin% colnames(df)){
              val <- setDT(df)[Cell_id == ids[[1]] & time_seconds <= 1, .N]
            }

            if("ID" %in% colnames(df)){
              val <- setDT(df)[Cell_id == ids[[1]] & TIME_SECONDS <= 1, .N]
            }
            orig_freq(val)

            #df[, root_path := abs_path$path()]

            print("before filtering")
            print(orig_freq())
            print(input$filterRaw)
            print(input$downsampleRaw)

            print(head(df))
            df <- setDT(df)[, CA_TRACE := dplR::pass.filt(y = CA_TRACE, W = as.integer(input$filterRaw), type = "low"), by = .(Cell_id,group) ]
            df <- setDT(df)[, ISOS_TRACE := dplR::pass.filt(y = ISOS_TRACE, W = as.integer(input$filterRaw), type = "low"), by = .(Cell_id,group) ]

            df <- setDT(df)[, Mean_Grey := dplR::pass.filt(y = Mean_Grey, W = as.integer(input$filterRaw), type = "low"), by = .(Cell_id,group) ]
            print("after filtering")

            print(head(df))

            df[, ID := stringr::str_split(ID, "_")[[1]][1], by = Cell_id]

            View(df)

            print("orig_freq")
            print(orig_freq)

            print("input$downsampleRaw")
            print(input$downsampleRaw)

            df  <-  downsampleCaData(df, orig_freq(), as.integer(input$downsampleRaw))

            print("after downsampling")

            print(head(df))

            df[, unique_ID := rleid(ID,group)]

            print("yoo")
            print(project$dir_path)
            print(project$db_file)

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
          }
        })



        shiny::observeEvent(load_input(), {
          load_in <- load_input()

          if(load_in == FALSE){
            print("yak")
            output$project_loading <- NULL
          }


          if(load_in == TRUE){
            print("youk")
            output$project_loading <- shiny::renderUI({


              list(
                shiny::textInput(ns("proj_name_load"), label = "Project Name" ),
                shiny::actionButton(ns("load_button"), "Load Project", align = "center"))

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
            db$load <- data.table::setDT(calipR::get_full_df(paste(project$dir_path, project$db_file,sep = "/"), "df_full"))

            db$load <- db$load[order(ID)]

            print("unique(db$load[['ID'']]) video status 0")
            print(unique(db$load[["ID"]]))


            if( "ID" %in% colnames(setDT(df))){
              ids <- unique(setDT(df)$ID)
              freq <- setDT(df)[ID == ids[[1]] & TIME_SECONDS <= 1, .N]
            }

            if("ID" %notin% colnames(setDT(df)) & "Cell_id" %in% colnames(setDT(df))){
              freq <- setDT(df)[Cell_id == "A1aaa" & time_seconds <= 1, .N]
            }

            print("orig_freq()")
            print(orig_freq())

            orig_freq(freq)

            output$df_loaded <- shiny::renderDataTable({df},
                                                       options = list(scrollX = TRUE))

          }

          else{
            output$warning_load <- shiny::renderUI({"This project doesn't exist"})
          }
        })


        shiny::observeEvent(input$annotateVideoButton, {

          # Annotating videos :

          print('list.files(project$dir_path))')

          print(list.files(project$dir_path))


          if( paste0(input$animal_id, ".csv") %in% list.files(project$dir_path)){
            print("in da place")
            annotated_video <- data.table::fread(paste(project$dir_path, paste0(input$animal_id, ".csv"), sep = "/"), header = TRUE)
            annotated_video[, ID := input$animal_id][,PATH := input$videoPath]

            print(head(annotated_video))
            print(paste(db$load[Cell_id == input$animal_id & videoPath == input$videoPath]$root_path[1]))
            print(head(db$load))
            print(input$videoPath)
            print(db$load[Cell_id == input$animal_id & videoPath == input$videoPath])
            addEpocs(paste(db$load[Cell_id == input$animal_id & videoPath == input$videoPath]$root_path[1], input$animal_id, sep = "/"), annotated_video[ID == input$animal_id & PATH == input$videoPath], "behavioral_data.csv", project$dir_path)
            print("yoo")

            annotated_video <- fread( paste(project$dir_path, "behavioral_data.csv", sep= "/" ), header = TRUE)
            print("this is the nanotated video from the behavioral file")
            print(annotated_video)
            if(length(which(colnames(annotated_video) == "V1")) > 1){
              print("yis")
              v1_to_rm <- which(colnames(annotated_video) == "V1")[[1]]
              annotated_video[, v1_to_rm] <- NULL

            }

            if(dim(calipR::checkTable(paste(project$dir_path,project$db_file, sep ="/"), "'video_annotation'"))[1] == 0) {
              print("yal")

              print("this is the annotation table that is going to be saved")
              print(annotated_video)
              calipR::saveData(annotated_video, paste(project$dir_path, project$db_file, sep = "/"), "video_annotation")

            }

            else{

              video_status$annotation <- setDT(calipR::get_full_df(paste(project$dir_path, project$db_file, sep = "/"), "video_annotation"))

              print("yoli")


              print("head(annotated_video)")
              print(head(annotated_video))

              print("str(video_status$annotation)")
              print(str(video_status$annotation))

              print("video_status$annotation[ID == as.integer(input$animal_id)]")
              print(head(video_status$annotation[ID == as.integer(input$animal_id)]))

              print("video_status$annotation[ID == input$animal_id]")
              print(head(video_status$annotation[ID == input$animal_id]))

              names(video_status$annotation) <- colnames(annotated_video)

              print(dim(video_status$annotation[ID == as.integer(input$animal_id)]))
              print(dim(annotated_video))
              print(dim(video_status$annotation))


              if(input$animal_id %in% video_status$annotation[["ID"]]){
                print("yikouroutu")
                  # if the video has already been annotated, the old version will be discarded
                  video_status$annotation <- video_status$annotation[ID != input$animal_id,]
                  video_status$annotation <- rbind(video_status$annotation, annotated_video)

                }

              '%notin%' <- Negate('%in%')
              if(input$animal_id %notin% video_status$annotation[["ID"]]){
                print("yokalarou")
                video_status$annotation <- rbind(video_status$annotation,annotated_video)
              }

              #video_status$annotation <- rbind(video_status$annotation, annotated_video)
              calipR::saveData(video_status$annotation, paste(project$dir_path, project$db_file, sep = "/"), "video_annotation")

            }
          }

          else{
            keys <- as.list(stringr::str_split(input$keys, pattern = ",", simplify = TRUE))

            print("project$dir_path")
            print(project$dir_path)
            annotateVideo(input$videoPath, paste(project$dir_path, paste0(input$animal_id,".csv"), sep= "/" ), keys, rep(TRUE,length(keys)))

            annotated_video <- data.table::fread(paste0(paste(project$dir_path, input$animal_id, sep= "/" ), ".csv"), header = TRUE)

            annotated_video[, ID := input$animal_id][,PATH := input$videoPath]

            print("annotated_video_first")
            print(annotated_video)

            ### Working on behavior alignment
            print('db$load[["root_path"]][1]')
            print(db$load[["root_path"]][1])

            print("db$load[Cell_id == input$animal_id]$root_path[1]")
            print(db$load[Cell_id == input$animal_id]$root_path[1])

            print("db$load[ID == input$animal_id]$root_path[1]")
            print(db$load[ID == input$animal_id]$root_path[1])


            addEpocs(paste(db$load[Cell_id == input$animal_id]$root_path[1], input$animal_id, sep = "/"), annotated_video[ID == input$animal_id & PATH == input$videoPath], "behavioral_data.csv", project$dir_path)
            print("yoo")
            annotated_video <- fread( paste(project$dir_path, "behavioral_data.csv", sep= "/" ), header = TRUE)
            print("annotated_video")
            print(annotated_video)
          }
          ###

          if(length(which(colnames(annotated_video) == "V1")) > 1){
            print("yis")
            v1_to_rm <- which(colnames(annotated_video) == "V1")[[1]]
            annotated_video[, v1_to_rm] <- NULL

          }

          print("annotated_video_last")
          print(annotated_video)

          if(dim(calipR::checkTable(paste(project$dir_path,project$db_file, sep ="/"), "'video_annotation'"))[1] == 0) {
            print("yal")

            calipR::saveData(annotated_video[ID == input$animal_id & PATH == input$videoPath], paste(project$dir_path, project$db_file, sep = "/"), "video_annotation")

          }

          else{

            video_status$annotation <- setDT(calipR::get_full_df(paste(project$dir_path, project$db_file, sep = "/"), "video_annotation"))

            print("yol")

            print("head(annotated_video)")
            print(head(annotated_video))

            print("str(video_status$annotation)")
            print(str(video_status$annotation))

            print("video_status$annotation[ID == as.integer(input$animal_id)]")
            print(head(video_status$annotation[ID == as.integer(input$animal_id)]))

            print("video_status$annotation[ID == input$animal_id]")
            print(head(video_status$annotation[ID == input$animal_id]))


            print("input$animal_id")
            print(input$animal_id)

            print("unique(video_status$annotation[['ID']])")
            print(unique(video_status$annotation[["ID"]]))

            names(video_status$annotation) <- colnames(annotated_video)

            if(input$animal_id %in% video_status$annotation[["ID"]]){
              print("yikouroutuille")

              video_status$annotation <- video_status$annotation[ID != input$animal_id,]
              video_status$annotation <- rbind(video_status$annotation, annotated_video)

            }

            '%notin%' <- Negate('%in%')
            if(input$animal_id %notin% video_status$annotation[["ID"]]){
              print("yokalarouille")
              video_status$annotation <- rbind(video_status$annotation,annotated_video)
            }
            print(video_status$annotation)
            print("unique(video_status$annotation[['ID']]) BIS")
            print(unique(video_status$annotation[["ID"]]))


            #video_status$annotation <- rbind(video_status$annotation, annotated_video)
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

})
}

