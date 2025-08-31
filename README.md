# ACS Child Poverty Harmonization

Flag census tracts where **ACS 5-year child poverty (under 18)** dropped below a threshold across vintages. Optional harmonization of 2017 (2010 tracts) onto **2020 tract** boundaries using **area** or **population (MCDC GeoCorr)** weights.

## Quick start
```bash
# 1) Set a Census API key once on your machine:
R -q -e 'tidycensus::census_api_key("YOUR_KEY", install=TRUE); readRenviron("~/.Renviron")'

# 2) Put your GeoCorr crosswalk here (optional for population weighting):
#    data/raw/geocorr_10to20.csv

# 3) Run (population-weighted, harmonized):
Rscript scripts/run_analysis.R \
  --state "MD" --counties "Baltimore City" \
  --year_lo 2017 --year_hi 2022 \
  --threshold 0.30 --min_drop 0.20 \
  --harmonize --weight_method population \
  --xwalk_path data/raw/geocorr_10to20.csv --xwalk_src geoid10 --xwalk_tgt geoid20 --xwalk_w afact \
  --out_csv data/derived/baltimore_weird_drops.csv
