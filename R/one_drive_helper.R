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
#' "raw_data/abs_anzic_income_SA2/sa2_income_industry.csv"
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
#' }
create_one_drive_path <- function(user = "Nikhil Chandra",
                                  team = "OneDrive - 35 South",
                                  path_extension = "raw data/abs_geo_structures") {
  
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
  
  if(user == "Nikhil Chandra") {
    glue::glue("C:/Users/{user}/{team}/Documents - Data Science/")
  }
  
  glue::glue("{base_path}{path_extension}")
  
  
}