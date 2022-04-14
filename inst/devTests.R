# Testing scripts
library(osiris); library(dplyr);library(tibble);library(ncdf4);library(rlang); library(data.table)

# Change data folder
data_folder <- "C:/Z/models/osiris_yieldEmu4GCAM/data"

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


# Tests for grid_to_basin_yield
osiris::grid_to_basin_yield(
  carbon = paste0(data_folder,"/yield_response_inputs/magicc_rcp8p5_co2.csv"),
  weight_floor_ha = 1,
  fcnpath = paste0(data_folder,"/yield_response_fcns/ggcmi_phase2"),
  inputpath = paste0(data_folder,"/yield_response_inputs"),
  areapath = paste0(data_folder,"/area_data"),
  basin_grid = paste0(data_folder,"/mapping_data/gridData.csv"),
  basin_id = paste0(data_folder,"/mapping_data/gcam_basin_ids.csv"),
  outpath = getwd(),
  crops = c("maize", "rice", 'soy', 'wheat'),
  esm_name = 'CanESM5',
  cm_name = 'LPJmL',
  scn_name = 'rcp8p5',
  N = 200
)
