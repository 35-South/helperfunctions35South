#' This function goes through a specified folder within a project and sources
#' all the .R files within the folder. This functions like
#' `devtools::load_all()` for non-package repos. Please ensure the contents of
#' .R files within the folder are functions/variables that you would like in
#' your global environment before running the funtion.
#'
#' @param .path (character) The file path of the folder you would like the .R
#' files sourced from.
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#'
#' load_custom_functions(.path = "R")
#'
#' }
load_custom_functions <- function(.path = "R"){

  dir(.path,full.names = T) %>%
    purrr::walk(
      ~ source(.x)
    )

}
