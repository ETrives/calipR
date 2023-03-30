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

  if(is.na(coverslip_id)) {
    a <- LETTERS[sample.int(26,1)]
    b <- LETTERS[sample.int(26,1)]
    c <- LETTERS[sample.int(26,1)]

    w <- paste0(paste0(a,b),c)
  }

    else{
  w <- df$coverslip[[1]]
    }


  if(length(w[which(is.na(w))]) != 0){
    na_cov <- length(w[which(is.na(w))])
    second_letter_list <- LETTERS[seq(from = 1, to = length(na_cov))]
    third_letter_list <- LETTERS[seq(from = 1, to = length(na_cov))]
    dbl <- paste0(second_letter_list, third_letter_list)
    w[which(is.na(w))] <- dbl
  }

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


        #id <- paste0(w,x,y,z)
        id <- paste0(w,x,y,z)

        id_list[[list_index]] <- id

        list_index = list_index + 1
        index3 = index3 + 1

      }

    }

  }

  return(id_list)
}
