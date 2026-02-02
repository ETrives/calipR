
#' analyzeFiberModuleUI
#'
#' Function containing the UI components for the analysis part of the fiber tab
#'
#' @param id
#'
#' @returns
#' @export
#'
#' @examples
analyzeFiberModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      shinydashboard::box(
        title = "PeriEvents",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        shiny::uiOutput(ns("var_selector")),
        shiny::actionButton(ns("plot"), "Plot"),
        shiny::actionButton(ns("plot_auc"), "Plot AUC"),
        shiny::checkboxInput(ns("indiv_box"), "Show indivdiual traces"),
        shiny::uiOutput(ns("ind_selector")),
        plotly::plotlyOutput(ns("peri_plot")),
        plotly::plotlyOutput(ns("peri_plot_auc"))


      )
    )

  )
}

#' analyzeFiberModuleServer
#'
#' Function containing the server elements related to the analyzeFiberModuleUI
#'
#' @param id
#' @param db
#'
#' @returns
#' @export
#'
#' @examples
analyzeFiberModuleServer <- function(id, db) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$var_selector <- shiny::renderUI({

      shiny::selectInput(inputId = ns("x_var"), label = NULL, names(db$aligned))

    })


    observeEvent(input$plot, {
    print("youka")
    output$peri_plot <- plotly::renderPlotly({
      print("youki")


        db$aligned[, fit_isos := stats::lm(CA_TRACE ~ ISOS_TRACE)$fitted.values, by = .(ID, group)]
        db$aligned[, delta_f_f := (CA_TRACE - fit_isos) / fit_isos, by = .(ID, group)]


      var <- paste0("status_",input$x_var)

      if(input$indiv_box == TRUE){

        peri_extracted <- PeriEventExtractR(db$aligned[ID == input$indiv_selector], var, 20,20)

        lapply(seq(1,length(peri_extracted[[var]])),
               function(x) peri_extracted[[var]][[x]][, event_id := x])

        data <- do.call(rbind,peri_extracted[[var]])

      }

      if(input$indiv_box == FALSE){

        peri_extracted <- lapply(unique(db$aligned[["group"]]), function(x) PeriEventExtractR(db$aligned[group == x], var, 20,20) )

        lapply(peri_extracted, function(x) lapply(seq(1,length(x[[var]])),
               function(y) x[[var]][[y]][, event_id := y]))

        data <- lapply(peri_extracted, function(x) do.call(rbind,x[[var]]))

        print("data rbind")
        print(data)

        data <- do.call(rbind, data)

        print("data rbind after")
        print(data)
      }
      # Add an id to each repetition of a given type of event to be able
      # to then plot it as a variable


      data.table::setDT(data)[, period := ifelse(TIME_FROM_EVENT <= 0,
                                                 "before","after")]

      print("before_after")
      print(head(data))

      data[, auc := flux::auc(TIME_SECONDS, delta_f_f), by = .(event_id,period, group)]

      print(data)
      data$period <- factor(data$period, levels = c("before", "after"))
      data$group <- factor(data$group, levels = unique(data$group))

      db$auc <- data

      print("auc")
      #mean_auc <- data[, .(mean(auc, na.rm = TRUE)), by = .(period)]


      p <- ggplot2::ggplot(data)+
        ggplot2::geom_line(ggplot2::aes(x = TIME_FROM_EVENT, y = CA_TRACE, color = group))+
        ggplot2::geom_line(ggplot2::aes(x = TIME_FROM_EVENT, y = fit_isos, color = group))+
        ggplot2::facet_wrap(~event_id+group)+
        ggplot2::theme_classic()

      p

    })

    })


    observeEvent(input$plot_auc, {

    output$peri_plot_auc <- plotly::renderPlotly({

      print("before mean_auc")

      #mean_auc <- db$aligned[, .(auc = mean(auc, na.rm = TRUE)), by = .(period)]
      print("db$auc")
      print(db$auc)

      if(input$indiv_box == TRUE){
      p_auc <- ggplot2::ggplot(data = db$auc[ID == input$indiv_selector], ggplot2::aes(x = event_id, y = auc,
                                 fill = period))+
        ggplot2::geom_bar(stat = "identity", position = "dodge")+
        ggplot2::theme_classic()
      }

      if(input$indiv_box == FALSE){
        p_auc <- ggplot2::ggplot(data = db$auc, ggplot2::aes(x = event_id, y = auc,
                                                            fill = period))+
          ggplot2::geom_bar(stat = "identity", position = "dodge")+
          ggplot2::facet_wrap(~group)+
          ggplot2::theme_classic()
      }

      p_auc
    })
    })


    observeEvent(input$indiv_box, {

      if(input$indiv_box == TRUE) {

        names_indiv <- unique(db$aligned[["ID"]])

      indiv_list <- lapply(unique(db$aligned[["ID"]]), function(x) x)

        names(indiv_list) <- names_indiv

      output$ind_selector <- shiny::renderUI({

        shiny::selectInput(ns("indiv_selector") , indiv_list, indiv_list)

      })
      }

      else{
        output$ind_selector <- NULL
      }
      })

    print("finished indiv selector")
  })
}
