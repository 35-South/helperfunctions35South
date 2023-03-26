#' Download, read, clean PBS line items
#' 
#' The is function will fetch data from data.gov on PBS expense items. It will 
#' then clean and join all the data and return data in long form. The data will
#' be a timeseries going back to 2014-15 Budget. 
#'
#' @return (tibble) A timeseries of Budget Data going back to 2014-15
#' @export
#'
#' @examples \dontrun{
#' 
#' pbs_data <- get_pbs_line_items()
#' 
#' }
get_pbs_line_items <- function() {
  
  url <- 
    dplyr::tibble( 
      urls = c("https://data.gov.au/data/dataset/98039365-ae78-4290-8d7c-eb0a4fcbd52b/resource/21aa5327-eca8-4319-8596-549c1080856e/download/2021-22-pbs-program-expense-line-items.csv", 
               "https://data.gov.au/data/dataset/86d7d307-92e2-48d9-b375-480685056673/resource/b8a3bd38-9662-4a00-9ffc-5c0c97d6b5bf/download/2020-21-pbs-program-expense-line-items_20201012.csv", 
               "https://data.gov.au/data/dataset/2fe0ab1a-3161-477a-9175-5c829d80afab/resource/893cbd3a-4d00-463e-b645-439afaf83337/download/2019-20-pbs-program-expense-line-items.csv", 
               "https://data.gov.au/data/dataset/d8f51107-d0de-4daf-a7d8-0fb7fbdce504/resource/d5bca8a5-f557-40da-8e5b-62047ce12802/download/2017-18-pbs-program-expenses-line-items-1.csv", 
               "https://data.gov.au/data/dataset/0c516e3a-0fb0-45a4-ac3f-0ab33c265885/resource/bd6a09a3-7687-4cc2-ae61-d03c5d03c594/download/2016-17-pbs-line-item-dataset.csv", 
               "https://data.gov.au/data/dataset/5b54386d-4b46-4736-87c5-28bd5ee38bcc/resource/365051dd-9335-4c2b-8c29-331718e079eb/download/201505181230budget1516.csv"
      ), 
      calendar_year = c(2021, 2020, 2019, 2017, 2016, 2015)
    )
  
  url_xlsx <- 
    dplyr::tibble(
      urls = 
        c("https://data.gov.au/data/dataset/863c394c-a26c-4340-896b-a26b18af476d/resource/66ee77e9-34b0-4215-8bc3-bfa43b7c8f11/download/2018-19-pbs-program-expense-line-items-1.xlsx", 
          "https://data.gov.au/data/dataset/c754efb9-f456-4417-acc7-bd96cff93cb4/resource/dacb615b-19ad-4f8f-b846-01e53c65589f/download/budget201415tablesanddata.xlsx"), 
      calendar_year = c(2018, 2014)
    )
  
  rename_function <- 
    function(read_in_data = test_data, 
             calendar_year = 2015) {
      
      names_dat <- names(read_in_data) %>% 
        purrr::map(
          ~ 
            dplyr::case_when(
              stringr::str_detect(.x, "x[0-9]+_[0-9]+") ~ 
                stringr::str_remove_all(.x, pattern = "[A-Z]|[a-z]|_[0-9]+"), 
              TRUE ~ .x
            )
        ) %>%
        purrr::map(
          ~ 
            dplyr::case_when(
              as.numeric(.x) >= calendar_year ~ glue::glue("FE_{.x}"), 
              as.numeric(.x) < calendar_year ~ glue::glue("estimated_actual_{.x}"), 
              TRUE ~ .x
            )
        ) %>% 
        unlist() %>% 
        as.character()
      
      names(read_in_data) <-  names_dat
      
      complete_data <- read_in_data %>% 
        dplyr::mutate(dplyr::across(tidyselect::contains("FE_"), ~as.numeric(.))) %>%
        dplyr::mutate(dplyr::across(tidyselect::contains("estimated_actual_"), ~as.numeric(.))) %>% 
        tidyr::pivot_longer(tidyselect::contains(c("estimated_actual_", "FE_")), 
                     names_to = "estimate_actual", values_to = "value")
      
      return(complete_data)
      
    }
  
  
  dat_csv <- url %>%
    split(.$calendar_year, drop = FALSE) %>% 
    purrr::map_dfr(
    ~ readr::read_csv(.x$urls[1]) %>% 
      janitor::clean_names() %>% 
      rename_function(calendar_year = .x$calendar_year[1]) %>% 
      dplyr::mutate(publish_year = .x$calendar_year[1])
    )
  
  xlsx_data <- url_xlsx %>%
    split(.$calendar_year, drop = FALSE) %>% 
    purrr::map_dfr(
      ~ download_xlsx(.x$urls[1]) %>% 
        janitor::clean_names() %>% 
        rename_function(calendar_year = .x$calendar_year[1])%>% 
        dplyr::mutate(publish_year = .x$calendar_year[1])
    )
  
  returned_data <- 
    dat_csv %>% 
    dplyr::bind_rows(xlsx_data)
  
  return(returned_data)
  
}