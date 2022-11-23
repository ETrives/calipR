#' svg_plot
#'
#' @param full_data
#' @param peaks_data
#' @param cell
#' @param var
#' @param line
#' @param show_peak
#' @param file_name
#' @param width
#' @param height
#'
#' @return
#' @export
#'
#' @examples
svg_plot <- function(full_data, peaks_data, cell, var,
                     line = c(FALSE, "poly", "gam"), show_peak = FALSE, file_name, width, height) {

  svglite::svglite(file_name, width, height)
  plot(cell_plot(full_data, peaks_data, cell, var, line, show_peak))
  dev.off()

}


#' svg_responders
#'
#' @param full_data
#' @param peaks_data
#' @param cell
#' @param var
#' @param line
#' @param show_peak
#' @param width
#' @param height
#'
#' @return
#' @export
#'
#' @examples
svg_responders <- function(full_data, peaks_data, var,
                           line = c(FALSE, "poly", "gam"), show_peak = FALSE, width, height) {

  lapply(unique(peaks_data$Cell_id), function(x) svg_plot(full_data, peaks_data, x, var,
                                                          line, show_peak, paste(x, ".svg", sep =""), width, height))

}




#' cell_plot
#'
#' @param full_data
#' @param peaks_data
#' @param cell
#' @param var
#' @param line
#' @param show_peak
#'
#' @return
#' @export
#'
#' @examples
cell_plot <- function(full_data, peaks_data, cell, var, line = c(FALSE, "poly", "gam"), show_peak = FALSE) {

  df <- full_data[full_data$Cell_id == cell,]
  peak_info <- peaks_data[peaks_data$Cell_id == cell,]


  p <- ggplot2::ggplot(df, ggplot2::aes(x = time_frame, y = !!rlang::sym(var)))+
    ggplot2::geom_line( ggplot2::aes( color =stimulus),size = 1)+
    #geom_point(data = peak,size = 2)+
    ggplot2::facet_wrap(~df$Cell_id) +
    ggplot2::theme_classic()

  q <- ggplot2::ggplot(df, ggplot2::aes(x = time_frame, y = smooth_z))+
    ggplot2::geom_line( ggplot2::aes( color =stimulus),size = 1)+
    #geom_point(data = peak,size = 2)+
    ggplot2::facet_wrap(~df$Cell_id) +
    ggplot2::theme_classic()

  if(line == FALSE) {

  }

  if(line == "poly") {

    p <- ggplot2::ggplot(df, ggplot2::aes(x = time_frame, y = !!rlang::sym(var)))+
      ggplot2::geom_line( ggplot2::aes( color =stimulus),size = 1)+
      #geom_point(data = peak,size = 2)+
      ggplot2::geom_line( ggplot2::aes(y = poly_fit))+
      ggplot2::facet_wrap(~Cell_id) +
      ggplot2::theme_classic()
  }

  if(line == "gam") {

    p <- ggplot2::ggplot(df, ggplot2::aes(x = time_frame, y = !!rlang::sym(var)))+
      ggplot2::geom_line( ggplot2::aes( color =stimulus),size = 1)+
      #geom_point(data = peak,size = 2)+
      ggplot2::geom_line( ggplot2::aes(y = gam_fit))+
      ggplot2::facet_wrap(~Cell_id) +
      ggplot2::theme_classic()
  }




  if(show_peak == TRUE){

    if(length(peak_info$Cell_id) == 0){
      final <- gridExtra::grid.arrange(p,q, ncol = 2)

    }

    else{

      max <- peak_info$Max_peak_smooth_z
      q <- q + ggplot2::geom_vline(ggplot2::aes(xintercept = peak_info$Start_peak_frame),size = 1)+
        ggplot2::geom_vline(ggplot2::aes(xintercept = peak_info$End_peak_frame),size = 1)+
        ggplot2::geom_point(ggplot2::aes(x = peak_info$Max_peak_frame, y = max))

      max_peak <- df[df[[var]] == max(df[[var]], na.rm = TRUE), ]

      p <- p +  ggplot2::geom_vline(ggplot2::aes(xintercept = peak_info$Start_peak_frame), size = 1)+
        ggplot2::geom_vline(ggplot2::aes(xintercept = peak_info$End_peak_frame), size = 1) +
        ggplot2::geom_point(ggplot2::aes(x = peak_info$Max_peak_frame, y = max_peak[[var]]))


      final <- gridExtra::grid.arrange(p,q, ncol = 2)

    }
  }

  else{

    final <- gridExtra::grid.arrange(p,q, ncol = 2)

  }

  return(final)

}


#' simple_cell_plot
#'
#' @param data
#' @param cell
#' @param var
#' @param line
#'
#' @return
#' @export
#'
#' @examples
simple_cell_plot <- function(data, triangle_data, cell, var, line = c(FALSE, "poly", "gam", "quantile"), trianglePlot = FALSE) {

  df <- data[data$Cell_id == cell,]
  #print(df)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = time_frame, y = !!rlang::sym(var)))+
    ggplot2::geom_line( ggplot2::aes( color =stimulus),size = 1)+
    #geom_point(data = peak,size = 2)+
    ggplot2::facet_wrap(~df$Cell_id) +
    ggplot2::theme_classic()

  #q <- ggplot(df, aes(x = time_frame, y = smooth_z))+
  #geom_line( aes( color =stimulus),size = 1)+
  #geom_point(data = peak,size = 2)+
  #facet_wrap(~df$Cell_id) +
  #theme_classic()

  if(line == FALSE){

  }

  if(line == "poly") {

    p <- ggplot2::ggplot(df, ggplot2::aes(x = time_seconds, y = !!rlang::sym(var)))+
      ggplot2::geom_line( ggplot2::aes( color =stimulus),size = 1)+
      #geom_point(data = peak,size = 2)+
      ggplot2::geom_line( ggplot2::aes(y = poly_fit))+
      ggplot2::facet_wrap(~Cell_id) +
      ggplot2::theme_classic()
  }

  if(line == "gam") {

    p <- ggplot2::ggplot(df, ggplot2::aes(x = time_seconds, y = !!rlang::sym(var)))+
      ggplot2::geom_line( ggplot2::aes( color =stimulus),size = 1)+
      #geom_point(data = peak,size = 2)+
      ggplot2::geom_line( ggplot2::aes(y = gam_fit))+
      ggplot2::facet_wrap(~Cell_id) +
      ggplot2::theme_classic()
  }

  if(line == "quantile") {

    p <- ggplot2::ggplot(df, ggplot2::aes(x = time_seconds, y = !!rlang::sym(var)))+
      ggplot2::geom_line( ggplot2::aes( color =stimulus),size = 1)+
      #geom_point(data = peak,size = 2)+
      ggplot2::geom_line( ggplot2::aes(y = local_quantile))+
      ggplot2::facet_wrap(~Cell_id) +
      ggplot2::theme_classic()
  }

  if(trianglePlot == TRUE) {

  triangle_data <- triangle_data[triangle_data$Cell_id == cell,]
  print(triangle_data)
  #print(p)
  print(unlist(triangle_data$trianglePoints[[1]][1]))
  p <- p + ggplot2::geom_point( aes(x = unlist(triangle_data$trianglePoints[[1]][1]), y = 0)) +
     geom_point(aes(x = unlist(triangle_data$trianglePoints[[1]][2]), y = 0)) +
    geom_point(aes(x = unlist(triangle_data$trianglePoints[[1]][3]), y = 0)) +
    geom_point(aes(x = unlist(triangle_data$trianglePoints[[1]][4]), y = 0)) +
    geom_point(aes(x = unlist(triangle_data$trianglePoints[[1]][5]), y = 0))




  }
  #final <- grid.arrange(p,q, ncol = 2)

  return(p)

}

