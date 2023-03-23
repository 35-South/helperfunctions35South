#' This function is used to transform a project into an installable R package.
#' The function wraps a series of functions from the `usethis` package to create
#' package.
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#'
#' #Run this in your console after creating a project
#' create_package_helperR()
#'
#' }
create_package_helperR <- function(){

  usethis::create_package(".")
  usethis::use_package_doc()
  usethis::use_mit_license()
  usethis::use_news_md()
  usethis::use_testthat()
  usethis::use_spell_check()
  usethis::use_tidy_eval()
  usethis::use_pipe()
  usethis::use_tibble()
  usethis::use_tidy_description()
  usethis::use_tidy_style()
  usethis::use_roxygen_md()
  usethis::use_lifecycle()
  usethis::use_readme_md()


}
