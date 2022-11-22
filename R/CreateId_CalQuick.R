#' createId
#'
#' @param df
#' @param coverslip_id
#'
#' @return
#' @export
#'
#' @examples
createId <- function(df, coverslip_id){

  cells <- names(table(df$Cell_id))

  # initializing an empty list
  id_list <- vector(mode = "list", length = length(cells))


  # splitting the cells in chunks of 26 (letters of the alphabet)
  # to be able to iterate over the chunks
  cells <- split(cells, ceiling(seq_along(cells)/26))

  # there are more than 26 chunks so resplitting the chunks
  cells <- split(cells, ceiling(seq_along(cells)/26))

  list_index = 1
  index1 = 1
  w <- letters[coverslip_id]

  for(i in cells){

    x <- letters[index1]

    index1 = index1 + 1

    index2 = 1
    for(j in i) {
      y <- letters[index2]

      index2 = index2 + 1
      index3 = 1

      for(h in j){



        z <- letters[index3]


        id <- paste0(w,x,y,z)
        id_list[[list_index]] <- id

        list_index = list_index + 1
        index3 = index3 + 1

      }

    }

  }

  return(id_list)
}
