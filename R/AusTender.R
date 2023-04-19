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
#'     get_austender_local(
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
#'     get_austender_local(
#'       base_path = base_path
#'     )    
#' 
#' }
get_austender_local_raw <- function(base_path = 
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

#' Gets cleaned AusTender stored on One Drive
#'
#' @param base_path (character) Path where the data is kept. 
#'
#' @return (tibble) Returns a tibble of the read in data.
#' @export
#'
#' @examples \dontrun{
#' aus_tender_data <- get_austender_cleaned()
#' }
get_austender_cleaned <- function(
    base_path = create_one_drive_path(path_extension = "raw data/AusTender_data/joined_data/aus_tender.csv")) {
  
   readr::read_csv(base_path)
  
}

#' Cleans the description column, removes uneccessary characters
#'
#' @param .data (tibble) Use get_austender_local_raw() as the input.
#' @param descrip_col description column to clean
#' @param new_col_name new description column name (default: "descrip_removed")
#'
#' @return (tibble) Returns the input data set with the descriptions cleaned.
#' @export
#'
#' @examples \dontrun{
#' 
#' raw_tender <- get_austender_local_raw()
#' raw_data_desc_cleaned <- 
#'   clean_description(
#'   .data =raw_tender
#'   descrip_col = "x10_description", 
#'   new_col_name "descrip_removed"
#'   )
#' 
#' }
clean_description_aus_tender <- function(.data = get_austender_local_raw(),
                              descrip_col = "x10_description",
                              new_col_name = "descrip_removed") {
  
  .data %>%
    dplyr::mutate(
      !!as.name(new_col_name) := stringr::str_replace_all(string = !!as.name(descrip_col),pattern = "[[:punct:]]", replacement = " "),
      !!as.name(new_col_name) := stringr::str_replace_all(string = !!as.name(new_col_name),pattern = "  ", replacement = " "),
      !!as.name(new_col_name) := stringr::str_replace_all(string = !!as.name(new_col_name),pattern = "  ", replacement = " ")
    )
  
}

#' Convert spend to spend per year by start and end date for aus tender data.
#'
#' @param .data (tibble) Needs x07_end_date, x06_start_date and x39_value
#'
#' @return (tibble) Returns the input Aus Tender data with a cost per year 
#' column.
#' 
#' @export
#'
#' @examples \dontrun{
#' 
#' raw_tender <- get_austender_local_raw()
#' 
#' raw_data_desc_cleaned <- 
#'   clean_description(
#'   .data =raw_tender
#'   descrip_col = "x10_description", 
#'   new_col_name "descrip_removed"
#'   )
#'   
#' raw_data_with_yearly_spend <- 
#'      aus_tender_yearly_spend(
#'      .data = raw_data_desc_cleaned
#'      )
#' 
#' }
aus_tender_yearly_spend <- function(.data = get_austender_local_raw()) {
  
  .data %>%
    dplyr::mutate(
      contract_length_days = x07_end_date - x06_start_date,
      contract_length_years = as.numeric(contract_length_days/365)
    ) %>%
    dplyr::mutate(
      contract_length_years =
        ifelse(contract_length_years == 0, 1, contract_length_years)
    ) %>%
    dplyr::mutate(
      spend_per_year = x39_value/contract_length_years
    )
  
}

#' This function detects ICT words to detect various ICT spend categories inside
#' Aus Tender data. Use get_austender_local_raw() to get raw data and clean the
#' description column in the data using clean_description_aus_tender(). 
#' 
#' The function will categorise ICT spending in the main data set. 
#'
#'
#' @param .data (tibble) The data with the column that you want to have 
#' categorized. Must have x13_category_title in the columns of the tibble.
#' @param descrip_col (character; "column") The name of the column to transform
#'
#' @return (tibble) Returns a dataframe with the added columns. 
#' @export
#'
#' @examples \dontrun{
#' 
#' raw_tender <- get_austender_local_raw()
#' 
#' raw_data_desc_cleaned <- 
#'   clean_description(
#'   .data =raw_tender
#'   descrip_col = "x10_description", 
#'   new_col_name "descrip_removed"
#'   )
#'   
#' raw_data_with_yearly_spend <- 
#'      aus_tender_yearly_spend(
#'      .data = raw_data_desc_cleaned
#'      )
#'      
#' ict_categorised <- 
#'       detect_ict_descrip(.data = raw_data_with_yearly_spend, 
#'                           descrip_col = "descrip_removed")
#' 
#' }
detect_ict_descrip <- function(.data,
                               descrip_col = "descrip_removed") {
  
  category_words <-
    c(
      "Computer services","Information technology\nconsultation services",
      "Software","Software maintenance and support","Computer","computer",
      "Forensic IT Services","Hardware","Information technology consultation services",
      "Cloud","Internet services", "Network security equipment","network","Network","server",
      "Server"
    ) %>%
    paste(collapse = "|")
  
  ICT_descrip_words <-
    c("developer","agile","engineer","designer","researcher", "computer","service designer",
      "security","intelligence","threat","endpoint","end point","intrusion detection","phishing")
  
  labour_hire_words <-
    c(
      "labour", "labor", "personnel", "manager", "developer", "administrator",
      "contractor", "professional services", "staff", "specialist",
      "analyst", "engineer", "designer", "architect", "lead", "researcher"
    ) %>% paste(collapse = "|")
  
  security_words <- "security|intelligence|threat|endpoint|end point|intrusion detection|phishing"
  service_words <- c("enterprise|license|software|cloud")
  education <- c("Education|Training")
  
  .data %>%
    dplyr::mutate(
      broad_category =
        dplyr::case_when(
          
          (stringr::str_detect(tolower(!!as.name(descrip_col)), labour_hire_words) &
             stringr::str_detect((!!as.name(descrip_col)), "ICT|IT|Computer|computer")) |
            
            (stringr::str_detect(tolower(!!as.name(descrip_col)), labour_hire_words) &
               stringr::str_detect((x13_category_title), category_words)) ~ "ICT personnel",
          
          (stringr::str_detect(tolower(!!as.name(descrip_col)), labour_hire_words) &
             !stringr::str_detect((x13_category_title), category_words) &
             !stringr::str_detect((!!as.name(descrip_col)), "ICT|IT|Computer|computer") ) ~ "Personnel (unspecified)"
          
        )
    ) %>%
    dplyr::mutate(
      broad_category =
        dplyr::case_when(
          is.na(broad_category) & stringr::str_detect(tolower(!!as.name(descrip_col)), service_words) ~ "ICT services",
          is.na(broad_category) & stringr::str_detect((x13_category_title), category_words) ~ "ICT services",
          TRUE ~ broad_category
        )
    )  %>%
    dplyr::mutate(
      detected_cyber =
        dplyr::case_when(
          stringr::str_detect(broad_category, "ICT") ~ "cyber",
          stringr::str_detect(!!as.name(descrip_col), "cyber|Cyber| ICT| IT|IT |ICT |Software|software|Cloud|Amazon| IBM|Software|Microsoft") ~ "cyber",
          stringr::str_detect(!!as.name(descrip_col), category_words %>% paste(collapse = "|")) ~ "cyber",
          stringr::str_detect(x13_category_title, category_words %>% paste(collapse = "|")) ~ "cyber",
          TRUE ~ "not cyber"
        )
    ) %>%
    dplyr::mutate(subcategory =
      dplyr::case_when(
        stringr::str_detect(tolower(!!as.name(descrip_col)), security_words) ~ "security",
        stringr::str_detect(tolower(!!as.name(descrip_col)), "business analyst") ~ "business analyst",
        stringr::str_detect(tolower(!!as.name(descrip_col)), "ISD") ~ "business analyst",
        stringr::str_detect(tolower(!!as.name(descrip_col)), "researcher") ~ "researcher",
        stringr::str_detect(tolower(!!as.name(descrip_col)), "developer|developers|scrum master|test analyst|architect|development") ~ "developers, analysts, architects",
        stringr::str_detect(tolower(!!as.name(descrip_col)), "project manager") ~ "project manager",
        stringr::str_detect(tolower(!!as.name(descrip_col)),"service designer") ~ "service designer",
        stringr::str_detect(tolower(!!as.name(descrip_col)),"interaction designer") ~ "interaction designer",
        stringr::str_detect(tolower(!!as.name(descrip_col)),"data") ~ "data",
        stringr::str_detect(tolower(!!as.name(descrip_col)),"engineering|engineer") ~ "engineering",
        stringr::str_detect(tolower(!!as.name(descrip_col)),"cloud|software|license") ~ "cloud, software services",
        stringr::str_detect(tolower(!!as.name(descrip_col)),"system administrator") ~ "system administrator",
        stringr::str_detect(tolower(!!as.name(descrip_col)), education) ~ "education and training",
        stringr::str_detect(tolower(!!as.name(descrip_col)), "network|server") ~ "networking, servers etc."
             )
    ) %>%
  dplyr::mutate(
      subcategory =
   dplyr::case_when(
     stringr::str_detect(broad_category, "ICT") & is.na(subcategory) ~ "ICT - Not specified purpose",
     stringr::str_detect((x13_category_title), category_words) & is.na(subcategory) ~ "ICT - Not specified purpose",
          TRUE ~ subcategory
        )
    )
  
}