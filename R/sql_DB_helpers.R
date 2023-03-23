#' This function will look inside a database table and sample data from the
#' table. This is a slower sample using SQL's `ORDER BY RANDOM()`.
#'
#' @param conn (`DBI` database connection object) This is the connection object
#' to a database. Refer to `DBI::dbConnect()` for details.
#' @param samples (integer) Number of samples to randomly sample.
#' @param table_name (character) Name of the table in the database to extract
#' data from.
#'
#' @return (tibble) Returns a tibble that is randomly sampled from the chosen
#' table in the databse.
#' @export
#'
#' @examples \dontrun{
#'
#' local_db_path <- "data/joined_lm_trig_data/mydb.db"
#' conn <- DBI::dbConnect(RSQLite::SQLite(), local_db_path)
#' tables <- RSQLite::dbListTables(conn) %>%
#'                  pluck(1)
#'
#' sampled_data <-
#'         sample_db(
#'         conn = conn,
#'         samples = 1000,
#'         table_name = tables
#'         )
#'
#' }
sample_db <- function(conn,
                      samples,
                      table_name){

  DBI::dbGetQuery(conn,statement = glue::glue("SELECT *
                                              FROM {table_name}
                                              ORDER BY RANDOM()
                                              LIMIT {samples};"))

}

#' This function will look inside a database table and counts the number of
#' rows inside that table.
#'
#' @param conn (`DBI` database connection object) This is the connection object
#' to a database. Refer to `DBI::dbConnect()` for details.
#' @param table_name (character) Name of the table in the database to extract
#' data from.
#'
#' @return (tibble) Returns a tibble that is randomly sampled from the chosen
#' table in the databse.
#' @export
#'
#' @examples \dontrun{
#'
#' local_db_path <- "data/joined_lm_trig_data/mydb.db"
#' conn <- DBI::dbConnect(RSQLite::SQLite(), local_db_path)
#' tables <- RSQLite::dbListTables(conn) %>%
#'                  pluck(1)
#'
#' sampled_data <-
#'         find_db_size(
#'         conn = conn,
#'         table_name = tables
#'         )
#'
#' }
find_db_size <- function(conn,
                         table_name){

 dplyr::tbl(conn,table_name) %>%
    dplyr::count() %>%
    dplyr::collect()

}

fast_db_sample <- function(conn,samples,
                           max_value,
                           filter_statement = NULL,
                           return_query = FALSE){

  tables <- RSQLite::dbListTables(conn)

  max_value <- ifelse(
    class(max_value) == "numeric", max_value,  max_value$n[1]
  )

  random_values <- floor(stats::runif(1,1, max_value - samples - 1))

  if(is.null(filter_statement)){

    if(return_query){

      test <- glue::glue("SELECT * FROM {tables}
                          LIMIT {random_values},{samples};")

    }else{

      test <- DBI::dbGetQuery(conn,statement = glue::glue(
        "SELECT * FROM {tables}
                              LIMIT {random_values},{samples};"))
    }


  }else{

    if(return_query){

      test <- glue::glue("SELECT * FROM {tables}
                         {filter_statement}
                          LIMIT {random_values},{samples};")

    }else{

      test <- DBI::dbGetQuery(conn,statement = glue::glue(
        "SELECT * FROM {tables}
                              {filter_statement}
                              LIMIT {random_values},{samples};"))


    }

  }


  return(test)

}

#' This function will create a local/connect to a local sql lite file. The 
#' path specified will be where the sql lite database is created to the 
#' local database that the function will connect you to. 
#'
#' @param path (file path character) File path for where you want the sql lite
#' database file to be created or the path containing the sql lite database
#' you would like to connect to.  
#'
#' @return (Database connection object)
#' @export
#'
#' @examples \dontrun{
#' local_db_path <- "data/joined_lm_trig_data/mydb.db"
#' db_con <- connect_db_local(local_db_path)
#' 
#' }
connect_db_local <- function(path){

  conn <- DBI::dbConnect(RSQLite::SQLite(), path)

  return(conn)

}

#' This function will take a dataframe and upload that dataframe as a table
#' to a SQL lite database object. You specify the name of the table you would
#' like to create and it will create that table in the SQL lite DB file and 
#' upload the dataframe as the data. The column names of the new table will 
#' be specified by the column names in the dataframe uploaded. 
#'
#' @param .data (dataframe) The data you would like to upload to the 
#' SQL lite database. 
#' @param table_name (character) The name you would like to give the table
#' you are creating in the database. 
#' @param conn (Database connection object) Database connection see 
#' `connect_db_local`
#'
#' @return NULL
#' @export
#'
#' @examples \dontrun{
#' #' local_db_path <- "data/joined_lm_trig_data/mydb.db"
#' db_con <- connect_db_local(local_db_path)
#' 
#' table_name <- "dummy"
#' data_to_upload <- 
#'   tibble(
#'   test1 = c(1,2,3), 
#'   test2= c("a", "b", "c")
#'   )
#'   
#' write_table_sql_lite(
#'   .data = data_to_upload, 
#'   table_name = table_name, 
#'   conn = db_con
#' )   
#' 
#' }
write_table_sql_lite <- function(.data,
                                 table_name,
                                 conn){
  DBI::dbWriteTable(conn, table_name, .data)
}

#' This function will upload new rows to an existing table in a sql lite 
#' DB file. The dataframe object provided must have the same column 
#' names as the column names in the DB. 
#'
#' @param .data (dataframe) The data you would like to append to the 
#' SQL lite database. 
#' @param table_name (character) The name you would like to give the table
#' you are creating in the database. 
#' @param conn 
#'
#' @return (NULL)
#' @export
#'
#' @examples \dontrun{
#' #' local_db_path <- "data/joined_lm_trig_data/mydb.db"
#' db_con <- connect_db_local(local_db_path)
#' 
#' table_name <- "dummy"
#' data_to_upload <- 
#'   tibble(
#'   test1 = c(1,2,3), 
#'   test2= c("a", "b", "c")
#'   )
#'   
#' append_table_sql_lite(
#'   .data = data_to_upload, 
#'   table_name = table_name, 
#'   conn = db_con
#' )   
#' 
#' }
append_table_sql_lite <- function(.data,
                                  table_name,
                                  conn){

  RSQLite::dbAppendTable(conn = conn,name = table_name,value = .data)

}
