

#' example_data_1
#'
#' @return
#' @export
#'
#' @examples
example_data_1 <- function() {

  data <- prepareData(system.file("extdata/VNO", package = "calipR"), 5,0.25, compare_groups = TRUE)

  return(data)

}

#' example_data_2
#'
#' @return
#' @export
#'
#' @examples
example_data_2 <- function() {

  data <- prepareData(system.file("extdata/test1", package = "calipR"), 2,1, compare_groups = FALSE)

  return(data)

}

