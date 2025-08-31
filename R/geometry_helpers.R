suppressPackageStartupMessages({ library(sf); library(tigris) })
options(tigris_use_cache = TRUE)
sf::sf_use_s2(FALSE)

get_tract_geos_2020 <- function(state, counties = NULL){
  tracts(state = state, county = counties, year = 2020, cb = TRUE, class = "sf") |>
    sf::st_transform(3857)
}

get_tract_geos_2010 <- function(state, counties = NULL){
  tracts(state = state, county = counties, year = 2010, cb = TRUE, class = "sf") |>
    sf::st_transform(3857)
}
