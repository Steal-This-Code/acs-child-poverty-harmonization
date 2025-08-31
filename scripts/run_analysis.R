#!/usr/bin/env Rscript
suppressPackageStartupMessages({ library(optparse) })
source("R/analysis.R")

opts <- list(
  make_option("--state", default = "MD"),
  make_option("--counties", default = "Baltimore City", help="Comma-separated or blank for whole state"),
  make_option("--year_lo", type="integer", default = 2017),
  make_option("--year_hi", type="integer", default = 2022),
  make_option("--threshold", type="double", default = 0.30),
  make_option("--min_drop", type="double", default = 0.20),
  make_option("--harmonize", action="store_true", default = TRUE),
  make_option("--weight_method", default = "population", help="population|area"),
  make_option("--xwalk_path", default = "data/raw/geocorr_10to20.csv"),
  make_option("--xwalk_src",  default = "geoid10"),
  make_option("--xwalk_tgt",  default = "geoid20"),
  make_option("--xwalk_w",    default = "afact"),
  make_option("--out_csv",    default = "data/derived/child_poverty_weird_drops.csv")
)

opt <- parse_args(OptionParser(option_list = opts))

counties_vec <- if (nzchar(opt$counties)) trimws(strsplit(opt$counties, ",")[[1]]) else NULL

res <- find_child_pov_drops(
  state = opt$state,
  counties = counties_vec,
  year_hi = opt$year_hi,
  year_lo = opt$year_lo,
  threshold = opt$threshold,
  min_drop = opt$min_drop,
  harmonize_lo_to_2020 = opt$harmonize,
  harmonize_weight_method = opt$weight_method,
  geocorr_path = opt$xwalk_path,
  geocorr_src_col = opt$xwalk_src,
  geocorr_tgt_col = opt$xwalk_tgt,
  geocorr_w_col   = opt$xwalk_w
)

dir.create(dirname(opt$out_csv), showWarnings = FALSE, recursive = TRUE)
readr::write_csv(res, opt$out_csv)
cat("Wrote:", opt$out_csv, "\n")
