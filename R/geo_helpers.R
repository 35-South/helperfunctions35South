#' This function will take a dataframe with coordinate data (-lat,long) and
#' intersect it with ABS maps data and return a dataframe with SA2 joined on.
#'
#' @param .data (dataframe) A dataframe with coordinate data you want intersected
#' with abs maps data.
#' @param coordinate_col (character) The name of the column that contains
#' coordinate data.
#' @param abs_data (sf) An sf class data frame with a geometry column.
#'
#' @return (tibble)
#' @export
#'
#' @examples \dontrun{
#' 
#' cfps_data <- 
#'   create_one_drive_path(user = "Nikhil Chandra",
#'   path_extension = "raw data/coal_fire_power_stations/cfps_manual_web_scrape.csv") %>%
#'   read_csv()
#' sa2_geos <- absmapsdata::sa22021
#' 
#' cfps_with_sa2_2021<-
#' geo_code_aus_data_sa2(.data = cfps_data,
#'                       coordinate_col = "Coordinates",
#'                       abs_data = sa2_geos2)
#' 
#' }
geo_code_aus_data_sa2 <- function(.data,
                                  coordinate_col = "Coordinates",
                                  abs_data = sa2_geos) {
  
  dat <- cfps_data%>%
    dplyr::mutate(!!as.name(coordinate_col) := stringr::str_replace(!!as.name(coordinate_col), "\\s", "")) %>%
    tidyr::separate(!!as.name(coordinate_col), into = c("lat", "long"), sep = ",") %>%
    dplyr::mutate(lat = as.numeric(lat),
                  long = as.numeric(long)) %>%
    tidyr::drop_na(lat, long) %>%
    sf::st_as_sf(coords = c("long", "lat"), crs = "+proj=longlat +datum=WGS84")
  
  sa2_geos2 <- sf::st_set_crs(abs_data, "+proj=longlat +datum=WGS84")
  
  intersected_geos <- sf::st_intersection(sa2_geos2, dat)
  
  intersected_geos %>%
    dplyr::select(-geometry) %>%
    dplyr::as_tibble()
  
}