# Testing scripts
library(osiris); library(dplyr);library(tibble);library(ncdf4);library(rlang); library(data.table)

# Change dtaa folder
data_folder <- "C:/Z/models/osiris_yieldEmu4GCAM/data/"

# Tests for calculate_deltas_from_climate
osiris::calculate_deltas_from_climate(
  climate_dir = paste0(data_folder,"/climate_data"),
  write_dir = paste0(data_folder,"/yield_response_inputs"),
  esm_name = 'CanESM5',
  crops = c("Corn", "Wheat", "Rice", "Soy"),
  irrigation_rainfed = c("IRR", "RFD"),
  minlat = -87.8638,
  minlon = -179.75,
  monthly_growing_season = paste0(data_folder,"/growing_seasons/pmonth_gslength_unifWheat_smallareamask.csv"),
  monthly_harvest_season = paste0(data_folder,"/growing_seasons/p_h_months_unifWheat_smallareamask.csv"),
  rollingAvgYears = 15,
  growing_season_dir = paste0(data_folder,"/growing_season_climate_data")
)

# For Debugging/testing
climate_dir = paste0(data_folder,"/climate_data")
write_dir = paste0(data_folder,"/yield_response_inputs")
esm_name = 'CanESM5'
crops = c("Corn", "Wheat", "Rice", "Soy")
irrigs = c("IRR", "RFD")
minlat = -87.8638
minlon = -179.75
monthly_growing_season = paste0(data_folder,"/growing_seasons/pmonth_gslength_unifWheat_smallareamask.csv")
monthly_harvest_season = paste0(data_folder,"/growing_seasons/p_h_months_unifWheat_smallareamask.csv")
rollingAvgYears = 15
growing_season_dir = paste0(data_folder,"/growing_season_climate_data")
