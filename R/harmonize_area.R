suppressPackageStartupMessages({ library(sf); library(dplyr); library(stringr) })

area_weight_interpolate_to_2020 <- function(source_sf, target_2020, value_cols){
  stopifnot(inherits(source_sf, "sf"), inherits(target_2020, "sf"))
  src <- sf::st_transform(source_sf, sf::st_crs(target_2020))
  src$src_area <- sf::st_area(src)
  inter <- suppressWarnings(sf::st_intersection(
    dplyr::select(target_2020, GEOID20 = GEOID),
    dplyr::select(src, GEOID_src = GEOID, dplyr::all_of(value_cols), src_area)
  ))
  inter$int_area <- sf::st_area(inter)
  inter$w <- as.numeric(inter$int_area / inter$src_area)
  for(v in value_cols) inter[[paste0(v, "_alloc")]] <- inter[[v]] * inter$w
  sums <- inter |>
    sf::st_drop_geometry() |>
    dplyr::group_by(GEOID20) |>
    dplyr::summarize(dplyr::across(dplyr::ends_with("_alloc"), ~sum(.x, na.rm = TRUE)), .groups="drop")
  out <- dplyr::left_join(target_2020, sums, by = c("GEOID" = "GEOID20"))
  names(out) <- stringr::str_replace(names(out), "_alloc$", "")
  out
}
