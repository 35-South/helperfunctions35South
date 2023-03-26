#' Download and read in xlsx file
#'
#' @param url (character) The download URL for the xlsx file
#'
#' @return (tibble)
#' @export
#'
#' @examples \dontrun{
#' url <- "https://data.gov.au/data/dataset/863c394c-a26c-4340-896b-a26b18af476d/resource/66ee77e9-34b0-4215-8bc3-bfa43b7c8f11/download/2018-19-pbs-program-expense-line-items-1.xlsx"
#' data <- download_xlsx(url)
#' }
download_xlsx <- function(url) {
  
  dest <- paste0(tempdir(), "\\temp.XLSX")
  httr::GET(url, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
  readxl::read_excel(tf)
  
}
