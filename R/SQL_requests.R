

#' saveData
#'
#' @param data
#' @param db_name
#' @param tab_name
#'
#'
#' @return
#'
#' @export
#'
#' @examples
saveData <- function(data, db_name, tab_name ) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_name)
  RSQLite::dbWriteTable(con, tab_name, data, overwrite = TRUE)
  RSQLite::dbDisconnect(con)
}


#' checkTable
#'
#' @param db_name
#' @param tab_name
#'
#' @return
#' @export
#'
#' @examples
checkTable <- function(db_name, tab_name){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_name)
  t <- RSQLite::dbGetQuery(con, paste("SELECT name FROM sqlite_master WHERE type='table' AND name=", tab_name, sep = "" ))
  RSQLite::dbDisconnect(con)
return(t)
}



# Exemple d'une requête pour récupérer les 100 premières lignes du df :

#' loading100
#'
#' @param db_name
#' @param tab_name
#'
#'
#' @return
#'
#' @export
#'
#' @examples
loading100 <- function(db_name, tab_name){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_name)
  df_top_100 <- RSQLite::dbGetQuery(con, paste("SELECT * FROM", tab_name, "LIMIT 100", sep = " " ))
  RSQLite::dbDisconnect(con)
  return(df_top_100)
}


# Fonction pour extraire une cellule de la base de donnée :

#' get_cell
#'
#' @param cell
#' @param db_name
#' @param tab_name
#'
#'
#' @return
#'
#' @export
#'
#' @examples
get_cell <- function(cell, db_name, tab_name) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_name)
  query <- paste(paste("SELECT * FROM", tab_name, "WHERE Cell_id LIKE", sep = " "),
                 paste("'%", cell, "%'", sep = ""), sep = " ")
  x <- RSQLite::dbGetQuery(con, query)
  RSQLite::dbDisconnect(con)
  return(x)
}


#' get_full_df
#'
#' @param db_name
#' @param tab_name
#'
#'
#' @return
#'
#' @export
#'
#' @examples
get_full_df <- function(db_name, tab_name) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_name)
  df_full <- RSQLite::dbGetQuery(con, paste("SELECT * FROM", tab_name, sep = " " ))
  RSQLite::dbDisconnect(con)
  return(df_full)

}



# Fonction pour extraire un petit nombre de cellules (défini par l'utilisateur) du dataset full :

#' get_sub_df
#'
#' @param db_name
#' @param tab_name
#' @param n_cells
#'
#' @return
#' @export
#'
#' @examples
get_sub_df <- function(db_name, tab_name, n_cells) {

  seq <- seq_along(as.integer(n_cells))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_name)

  cell_id <- unique(RSQLite::dbGetQuery(con, paste("SELECT Cell_id FROM", tab_name, sep = " " )))

  cells <- sample(cell_id$Cell_id, n_cells)

  df <- RSQLite::dbGetQuery(
             con, paste(paste("SELECT * FROM", tab_name,sep = " "), "WHERE Cell_id IN (",
             paste(rep("?", length(cells)), collapse = ","), ")"),
  	     params = as.list(cells))

  RSQLite::dbDisconnect(con)
  
return(df)

}


