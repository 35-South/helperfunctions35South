#' Create One Drive file Path
#' 
#' This function will return a file path for your one drive path. You must provide 
#' it your user name for it to create the appropriate path. You will also need 
#' to provide the path extension which the part of the path after the root
#' 'Documents - Data Science' directory in  
#' 
#' known users:
#' 
#' sam, ryan, sonal, christian, janet
#' 
#' 
#'
#' @param user (character) This is your user name. Example: "Nikhil Chandra"
#' @param team (character) This is the Team name on sharepoint. The default is
#' "OneDrive - 35 South", it is unlikley you will have to change this. 
#' @param path_extension (character) This should be the path after 
#' "C:/Users/Nikhil Chandra/OneDrive - 35 South/Documents - Data Science/". This 
#' is the base path, if your file is in the folder raw_data/abs_anzic_income_SA2 
#' with the file name "sa2_income_industry.csv" then you would provide 
#' "raw_data/abs_anzic_income_SA2/sa2_income_industry.csv". YOUR PATH MUST 
#' NOT HAVE THE WINDOWS PATH FORM USING \. YOU MUST USE /. 
#'
#' @return (file path) This function will return the file path based on your 
#' inputs. 
#' @export
#'
#' @examples \dontrun{
#' 
#' file_path <- 
#'   create_one_drive_path(
#'   user = "Nikhil Chandra",
#'   team = "OneDrive - 35 South",
#'   path_extension = "raw_data/abs_anzic_income_SA2/sa2_income_industry.csv"
#'   )
#'   
#' data_read<- read_csv(file_path)
#' 
#' # If you want to get AusTender data: 
#' 
#' # Auto Guess Path:
#' path_for_data <- 
#'   create_one_drive_path(path_extension = "raw_data/AusTender_data")
#'   
#' files_in_folder <- 
#'   fs::dir_info(path_for_data) %>% 
#'   dplyr::pull(path) 
#'  
#' read_in_first_csv_file <- read_csv(files_in_folder[1])
#' 
#' # Dont Auto Guess Path     
#' 
#' path_for_data <- 
#'   create_one_drive_path(
#'   user = "janet",
#'   path_extension = "raw_data/AusTender_data"
#'   )
#'   
#' files_in_folder <- 
#'   fs::dir_info(path_for_data) %>% 
#'   dplyr::pull(path) 
#'  
#' read_in_first_csv_file <- read_csv(files_in_folder[1])
#' }
create_one_drive_path <- function(user = NULL,
                                  path_extension = "raw data/AusTender_data") {
  
  check_system_vars <- detect_user_onedrive()
  
  if(is.null(user)) {
    base_path <- check_system_vars[1]
  } else {
   
    if(length(check_system_vars) > 1)
      
      if(tolower(user) == "janet") {
        base_path = "C:/Users/janet/OneDrive - 35 South/Shared Documents - Data Science/"
      }
    
    if(tolower(user) == "ryan") {
      base_path = "C:/Users/61433/35 South/Data Science - Documents/"
    }
    
    if(tolower(user) == "sonal") {
      base_path = "C:/Users/sonal/OneDrive - 35 South/Shared Documents - Data Science/"
    }
    
    if(tolower(user) == "sam") {
      
      base_path <- "C:/Users/sam/OneDrive - 35 South/Shared Documents - Data Science/"
      
    }
    
    if(tolower(user) == "christian") {
      
      base_path <- "C:/Users/chris/OneDrive - 35 South/Shared Documents - Data Science/"
      
    }
     
  }
  
  normalised_path_ext <- stringr::str_split(path_extension, pattern = "/") %>% 
    unlist()
  
  returned_path <- fs::dir_info(base_path) %>%
    dplyr::filter(stringr::str_detect(path, pattern = "Documents - Data Science")) %>% 
    dplyr::pull(path)
  
  for (i in 1:length(normalised_path_ext)) {
   
    if(length(returned_path) != 0) {
      returned_path <- 
        fs::dir_info(returned_path) %>% 
        dplyr::filter(
          stringr::str_detect(path, pattern = normalised_path_ext[i])
        ) %>% 
        dplyr::pull(path) 
    } else{ break }
    
  }
  
  if(length(returned_path) == 0) {
    message("You path does not exist")
  }
  
  return(returned_path)
  
}


#' Detect the Users One Drive
#' 
#' This function will detect your one drive path.  
#'
#' @return (named character vector)
#' @export
#'
#' @examples \dontrun{
#' 
#' one_drive_path <- detect_user_onedrive()
#' 
#' }
detect_user_onedrive <- function() {
  
  Sys.getenv(x = NULL, unset = "", names = NA) %>% 
    purrr::keep(~ stringr::str_detect(.x, "OneDrive"))
  
}
