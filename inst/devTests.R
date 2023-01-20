# Testing scripts
library(osiris); library(dplyr);library(tibble);library(ncdf4);library(rlang); library(data.table)

# Test for Step 0: get_example_data
# You may define your own write_dir and dir_name.
osiris::get_example_data(
  write_dir = getwd(),
  dir_name = "Osiris_Data_Test",
  data_link = "https://zenodo.org/record/7530067/files/Osiris_Data_Test.zip?download=1"
) -> data_folder; data_folder;

# Change data folder
# Run Step 0 to get the path to the data files
# data_folder = ADD_PATH_TO_YOUR_OSIRIS_DATA

# Test for wrf_to_osiris
osiris::wrf_to_osiris(
  wrf_ncdf = ADD_PATH_TO_YOUR_WRF_DATA,
  write_dir = paste0(data_folder,"/wrf_to_osiris"),
  osiris_ncdf = paste0(data_folder,"/climate_data/tas_Amon_CanESM5_historical_r1i1p1f1_orig.nc"),
  time_step = "3 hours",
  scenario = "historical"
)


# Test for Step 1: calculate_deltas_from_climate
osiris::calculate_deltas_from_climate(
  climate_dir = paste0(data_folder,"/climate_data"),
  write_dir = paste0(data_folder,"/outputs_calculate_delta_from_climate"),
  monthly_growing_season = paste0(data_folder,"/growing_seasons/pmonth_gslength_unifWheat_smallareamask.csv"),
  monthly_harvest_season = paste0(data_folder,"/growing_seasons/p_h_months_unifWheat_smallareamask.csv"),
  growing_season_dir = paste0(data_folder,"/outputs_growing_season_climate_data"),
  esm_name = "CanESM5",
  crops = c("Corn", "Wheat", "Rice", "Soy"),
  irrigation_rainfed = c("IRR", "RFD"),
  minlat = -87.8638,
  minlon = -179.75,
  rollingAvgYears = 1,
  tas_historical = "tas_Amon_CanESM5_historical_r1i1p1f1_gn_201001-201501.nc",
  tas_projected = "tas_Amon_CanESM5_ssp245_r1i1p1f1_gn_201501-202101.nc",
  pr_historical = "pr_Amon_CanESM5_historical_r1i1p1f1_gn_201001-201501.nc",
  pr_projected = "pr_Amon_CanESM5_ssp245_r1i1p1f1_gn_201501-202101.nc",
  historical_start_year = 2010,
  projection_start_year = 2015
)


# Test for Step 2: grid_to_basin_yield
osiris::grid_to_basin_yield(
  carbon = paste0(data_folder,"/yield_response_inputs/magicc_rcp8p5_co2.csv"),
  weight_floor_ha = 1,
  emulator_dir = paste0(data_folder,"/yield_response_fcns/ggcmi_phase2"),
  input_dir = paste0(data_folder,"/outputs_calculate_delta_from_climate"),
  area_dir = paste0(data_folder,"/area_data"),
  basin_grid = paste0(data_folder,"/mapping_data/gridData.csv"),
  basin_id = paste0(data_folder,"/mapping_data/gcam_basin_ids.csv"),
  write_dir = paste0(data_folder,"/outputs_grid_to_basin_yield"),
  wheat_area = paste0(data_folder,"/winter_and_spring_wheat_areas_v1_180627.nc4"),
  crops = c("maize", "rice", "soy", "wheat"),
  esm_name = "CanESM5",
  cm_name = "LPJmL",
  scn_name = "ssp245",
  N = 200
)


# Test for Step 3: yield_to_gcam_basin
osiris::yield_to_gcam_basin(
  write_dir = paste0(data_folder,"/outputs_yield_to_gcam_basin"),
  emulated_basin_yield_dir = paste0(data_folder,"/outputs_grid_to_basin_yield"),
  iso_GCAM_region_mapping = paste0(data_folder,"/mapping_data/iso_GCAM_regID.csv"),
  FAO_ag_mapping = paste0(data_folder,"/gcamdata_files/FAO_ag_items_PRODSTAT_expanded_corrected.csv"),
  iso_harvest_area_mapping = paste0(data_folder,"/gcamdata_files/L100.LDS_ag_HA_ha.csv"),
  iso_GCAM_basin_mapping = paste0(data_folder,"/mapping_data/gcam_basin_ids.csv"),
  esm_name = "CanESM5",
  scn_name = "ssp245",
  max_CCImult = 2.5,
  min_CCImult = 0.01,
  weight_floor_ha = 1,
  rolling_avg_years = 1,
  maxHistYear = 2015,
  minFutYear = 2015,
  maxFutYear = 2020,
  extrapolate_to = NULL
)


# Test for Step 4: create_AgProdChange_xml
osiris::create_AgProdChange_xml(
  write_dir = paste0(data_folder,"/outputs_create_AgProdChange_xml"),
  esm_name = 'CanESM5',
  scn_name = 'ssp245',
  ssp = 'ssp5',
  ag_irr_ref = paste0(data_folder,"/reference_agprodchange/L2052.AgProdChange_irr_high.csv"),
  bio_irr_ref = paste0(data_folder,"/reference_agprodchange/L2052.AgProdChange_bio_irr_ref.csv"),
  ag_impacts = paste0(data_folder,"/outputs_yield_to_gcam_basin/ag_impacts_CanESM5_ssp245_rcp_gcm_gcm_R_GLU_C_IRR_allyears_RA3_gridcull_allyroutlier.csv"),
  bio_impacts = paste0(data_folder,"/outputs_yield_to_gcam_basin/bio_impacts_CanESM5_ssp245_rcp_gcm_gcm_R_GLU_C_IRR_allyears_RA3_gridcull_allyroutlier.csv"),
  GCAM_region_mapping = paste0(data_folder,"/mapping_data/GCAM_region_names.csv"),
  timestep = 5,
  maxHistYear = 2010,
  minFutYear = 2015,
  appliedto = "full"
)


# Test for WRF diagnostic_plots
osiris::diagnostic_plots(
  hot_ssp3_apc = paste0(data_folder,"/outputs_create_AgProdChange_xml/ag_prodchange_rcp8p5_hot_ssp3_WRF_LPJmL.csv"),
  hot_ssp5_apc = paste0(data_folder,"/outputs_create_AgProdChange_xml/ag_prodchange_rcp8p5_hot_ssp5_WRF_LPJmL.csv"),
  cold_ssp3_apc = paste0(data_folder,"/outputs_create_AgProdChange_xml/ag_prodchange_rcp8p5_cold_ssp3_WRF_LPJmL.csv"),
  cold_ssp5_apc = paste0(data_folder,"/outputs_create_AgProdChange_xml/ag_prodchange_rcp8p5_cold_ssp5_WRF_LPJmL.csv"),
  reference_apc = paste0(data_folder,"/gcamdata_files/ag_prodchange_ref_IRR_MGMT.csv"),
  agyield = paste0(data_folder,"/gcamdata_files/L101.ag_Yield_kgm2_R_C_Y_GLU.csv"),
  iso_GCAM_basin_mapping = paste0(data_folder,"/mapping_data/gcam_basin_ids.csv"),
  write_dir = paste0(data_folder,"/outputs_diagnostic_plots")
)

