#' Get AusTender Data
#' 
#' This function read in AusTender Data stored on share hub/One Drive. The data
#' is read in from csv files kep on sharehun
#'
#' @param base_path (file path, character) This is the file path where the 
#' data is kept. By default it uses 
#' create_one_drive_path(path_extension = "raw data/AusTender_data/raw_exports"). 
#' But you may need to specify the file path if the function doest work with 
#' its default variables. 
#'
#' @return (tibble) This function will return the AusTender data into your
#' global environment
#' @export
#'
#' @examples \dontrun{
#' 
#' base_path <- 
#'     create_one_drive_path(
#'           user = NULL,
#'           path_extension = "raw data/AusTender_data/raw_exports"
#'           )
#'           
#' aus_tender_data <- 
#'     read_in_local_dashboard_exports(
#'       base_path = base_path
#'     )
#'     
#' #Other Options
#' 
#' base_path <- 
#'     create_one_drive_path(
#'           user = janet,
#'           path_extension = "raw data/AusTender_data/raw_exports"
#'           )
#'           
#' aus_tender_data <- 
#'     read_in_local_dashboard_exports(
#'       base_path = base_path
#'     )    
#' 
#' }
get_austender_local <- function(base_path = 
                                create_one_drive_path(path_extension = "raw data/AusTender_data/raw_exports")
                                ) {
  
  local_dashboard_exports <- fs::dir_info(base_path) %>%
    dplyr::filter(stringr::str_detect(path, "Overview Export_Export_data")) %>%
    dplyr::pull(path)
  
  local_csv_data <- local_dashboard_exports %>%
    purrr::map(
      ~ readr::read_csv(.x)
    )
  
  local_csv_data_joined <- local_csv_data %>%
    purrr::map_dfr(dplyr::bind_rows) %>%
    janitor::clean_names()
  
  return(local_csv_data_joined)
  
}