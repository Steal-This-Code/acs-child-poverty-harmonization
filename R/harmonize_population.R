suppressPackageStartupMessages({ library(dplyr); library(readr); library(sf) })

load_geocorr_xwalk <- function(path, src_col, tgt_col, w_col){
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::transmute(
      GEOID10 = as.character(.data[[src_col]]),
      GEOID20 = as.character(.data[[tgt_col]]),
      w = as.numeric(.data[[w_col]])
    ) |>
    dplyr::filter(!is.na(GEOID10), !is.na(GEOID20), !is.na(w)) |>
    dplyr::group_by(GEOID10) |>
    dplyr::mutate(w = w / sum(w, na.rm = TRUE)) |>
    dplyr::ungroup()
}

pop_weight_interpolate_to_2020 <- function(lo2010_counts, xwalk, target_2020){
  src <- sf::st_drop_geometry(lo2010_counts) |> dplyr::select(GEOID, u18_total, u18_poor)
  alloc <- dplyr::inner_join(xwalk, src, by = c("GEOID10" = "GEOID")) |>
    dplyr::mutate(u18_total_alloc = u18_total * w,
                  u18_poor_alloc  = u18_poor  * w) |>
    dplyr::group_by(GEOID20) |>
    dplyr::summarize(
      u18_total = sum(u18_total_alloc, na.rm = TRUE),
      u18_poor  = sum(u18_poor_alloc,  na.rm = TRUE),
      .groups = "drop"
    )
  dplyr::left_join(target_2020, alloc, by = c("GEOID" = "GEOID20"))
}
