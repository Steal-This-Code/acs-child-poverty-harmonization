suppressPackageStartupMessages({
  library(dplyr); library(sf)
  source("R/s1701_helpers.R")
  source("R/geometry_helpers.R")
  source("R/harmonize_area.R")
  source("R/harmonize_population.R")
})

find_child_pov_drops <- function(
  state, counties,
  year_hi, year_lo,
  threshold = 0.30, min_drop = 0.20,
  harmonize_lo_to_2020 = TRUE,
  harmonize_weight_method = c("population","area"),
  geocorr_path = NULL, geocorr_src_col = NULL, geocorr_tgt_col = NULL, geocorr_w_col = NULL
){
  harmonize_weight_method <- match.arg(harmonize_weight_method)

  hi_counts <- pull_child_pov_counts(year_hi, state, counties, geometry = TRUE) |>
    sf::st_transform(3857) |>
    dplyr::mutate(pct_2022 = 100 * ifelse(u18_total > 0, u18_poor / u18_total, NA_real_)) |>
    dplyr::select(GEOID, pct_2022, u18_total_2022 = u18_total, u18_poor_2022 = u18_poor,
                  moe_u18_2022 = u18_poor_moe)

  if(!harmonize_lo_to_2020){
    lo_counts <- pull_child_pov_counts(year_lo, state, counties, geometry = FALSE) |>
      dplyr::mutate(pct_2017 = 100 * ifelse(u18_total > 0, u18_poor / u18_total, NA_real_)) |>
      dplyr::select(GEOID, pct_2017, u18_total_2017 = u18_total, u18_poor_2017 = u18_poor,
                    moe_u18_2017 = u18_poor_moe)
    out <- dplyr::inner_join(lo_counts, sf::st_drop_geometry(hi_counts), by = "GEOID")
  } else {
    lo_counts_geo <- pull_child_pov_counts(year_lo, state, counties, geometry = TRUE) |>
      sf::st_transform(3857)
    tr2020 <- get_tract_geos_2020(state, counties)

    if (harmonize_weight_method == "area"){
      lo_on_2020 <- area_weight_interpolate_to_2020(lo_counts_geo, tr2020, c("u18_total","u18_poor"))
    } else {
      stopifnot(!is.null(geocorr_path), !is.null(geocorr_src_col), !is.null(geocorr_tgt_col), !is.null(geocorr_w_col))
      xwalk <- load_geocorr_xwalk(geocorr_path, geocorr_src_col, geocorr_tgt_col, geocorr_w_col)
      lo2010_geo <- get_tract_geos_2010(state, counties) |>
        dplyr::select(GEOID) |>
        dplyr::left_join(sf::st_drop_geometry(lo_counts_geo), by = "GEOID")
      lo_on_2020 <- pop_weight_interpolate_to_2020(lo2010_geo, xwalk, tr2020)
    }

    lo_on_2020 <- lo_on_2020 |>
      dplyr::mutate(pct_2017 = 100 * ifelse(u18_total > 0, u18_poor / u18_total, NA_real_)) |>
      dplyr::select(GEOID, pct_2017, u18_total_2017 = u18_total, u18_poor_2017 = u18_poor)

    out <- dplyr::inner_join(lo_on_2020, sf::st_drop_geometry(hi_counts), by = "GEOID")
  }

  # Quick MOE-to-proportion (only valid for non-harmonized path)
  moe_prop <- function(x, n, moe_x){
    ifelse(is.na(moe_x) | is.na(n) | n <= 0, NA_real_, pmin(1, abs(moe_x / n)))
  }

  out2 <- out |>
    dplyr::mutate(
      p2017 = pct_2017 / 100, p2022 = pct_2022 / 100,
      drop_pp = p2017 - p2022,
      drop_pct_points = 100 * drop_pp,
      crossed_below = (p2017 >= threshold) & (p2022 < threshold),
      big_drop = drop_pp >= min_drop
    )

  if ("moe_u18_2017" %in% names(out2)) {
    out2 <- out2 |>
      dplyr::mutate(
        moe_p2017 = moe_prop(u18_poor_2017, u18_total_2017, moe_u18_2017),
        moe_p2022 = moe_prop(u18_poor_2022, u18_total_2022, moe_u18_2022),
        combined_moe_prop = sqrt(coalesce(moe_p2017, 0)^2 + coalesce(moe_p2022, 0)^2),
        drop_exceeds_combined_moe = ifelse(is.na(combined_moe_prop), NA, drop_pp > combined_moe_prop)
      )
  } else {
    out2 <- out2 |>
      dplyr::mutate(combined_moe_prop = NA_real_, drop_exceeds_combined_moe = NA)
  }

  out2 |>
    dplyr::arrange(dplyr::desc(drop_pct_points)) |>
    dplyr::mutate(flag_weird = crossed_below & (big_drop | coalesce(drop_exceeds_combined_moe, FALSE))) |>
    dplyr::select(GEOID, pct_2017, pct_2022, drop_pct_points,
                  u18_total_2017, u18_poor_2017, u18_total_2022, u18_poor_2022,
                  crossed_below, big_drop, drop_exceeds_combined_moe, combined_moe_prop, flag_weird)
}
