suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(tidycensus)
})

s1701_find <- function(year, pattern){
  vars <- load_variables(year, "acs5", cache = TRUE)
  out <- vars |> dplyr::filter(table == "S1701",
           stringr::str_detect(label, stringr::regex(pattern, ignore_case = TRUE)))
  if(nrow(out) == 0) stop(paste("No S1701 match for:", pattern, "in", year))
  out
}

get_s1701_codes <- function(year){
  v_total_u18 <- s1701_find(year, "^Estimate!!Total!!Under 18 years$")
  v_poor_u18  <- s1701_find(year, "^Estimate!!Below poverty level!!Under 18 years$")
  list(total = v_total_u18$name[1], poor = v_poor_u18$name[1])
}

pull_child_pov_counts <- function(year, state, counties = NULL, geometry = FALSE){
  codes <- get_s1701_codes(year)
  dat <- tidycensus::get_acs(
    geography = "tract",
    variables = c(u18_total = codes$total, u18_poor = codes$poor),
    year = year, survey = "acs5",
    state = state, county = counties,
    cache_table = TRUE, geometry = geometry, output = "wide"
  )
  out <- dat |>
    dplyr::transmute(
      GEOID, NAME,
      year = year,
      u18_total = u18_totalE, u18_total_moe = u18_totalM,
      u18_poor  = u18_poorE,  u18_poor_moe  = u18_poorM
    )
  if(geometry) out <- sf::st_as_sf(out)
  out
}
