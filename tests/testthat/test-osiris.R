context("osiris Tests")
library(osiris); library(testthat); library(dplyr)

# Run tests with test data
# Test for Step 0: get_example_data
# You may define your own write_dir and dir_name.
osiris::get_example_data(
  write_dir = getwd(),
  dir_name = "Osiris_Data_Test",
  data_link = "https://zenodo.org/record/7490786/files/Osiris_Data_Test.zip?download=1"
) -> data_folder; data_folder;


# Step 1 - calculate_deltas_from_climate: Tests

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

test_that("calculate_deltas_from_climate produces expected files", {

  testthat::expect_identical( unlist(list.files(paste0(data_folder,"/outputs_calculate_delta_from_climate"))),
                              c("CanESM5_corn_irr_smooth_deltaT_deltaP.csv","CanESM5_corn_rfd_smooth_deltaT_deltaP.csv",
                             "CanESM5_rice_irr_smooth_deltaT_deltaP.csv","CanESM5_rice_rfd_smooth_deltaT_deltaP.csv",
                             "CanESM5_soy_irr_smooth_deltaT_deltaP.csv","CanESM5_soy_rfd_smooth_deltaT_deltaP.csv",
                             "CanESM5_wheat_irr_smooth_deltaT_deltaP.csv","CanESM5_wheat_rfd_smooth_deltaT_deltaP.csv"))

})

# Step 2 - grid_to_basin_yield
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

test_that("grid_to_basin_yield produces expected files", {

  testthat::expect_identical( unlist(list.files(paste0(data_folder,"/outputs_grid_to_basin_yield"))),
                              c("LPJmL_CanESM5_ssp245_maize_2010_2020.csv",
                                "LPJmL_CanESM5_ssp245_rice_2010_2020.csv",
                                "LPJmL_CanESM5_ssp245_soy_2010_2020.csv",
                                "LPJmL_CanESM5_ssp245_wheat_2010_2020.csv"))

})

# Step 3 - yield_to_gcam_basin
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

test_that("yield_to_gcam_basin produces expected files", {

  testthat::expect_identical( unlist(list.files(paste0(data_folder,"/outputs_yield_to_gcam_basin"))),
                              c("ag_impacts_CanESM5_ssp245_rcp_gcm_gcm_R_GLU_C_IRR_allyears_RA3_gridcull_allyroutlier.csv",
                                "bio_impacts_CanESM5_ssp245_rcp_gcm_gcm_R_GLU_C_IRR_allyears_RA3_gridcull_allyroutlier.csv"))

})

# Step 4 - create_AgProdChange_xml
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

test_that("create_AgProdChange_xml produces expected files", {

  testthat::expect_identical( unlist(list.files(paste0(data_folder,"/outputs_create_AgProdChange_xml"))),
                              c("ag_prodchange_ssp245_ssp5_CanESM5_LPJmL.csv",
                                "ag_prodchange_ssp245_ssp5_CanESM5_LPJmL.xml",
                                "ag_prodchange_ssp245_ssp5_CanESM5_NA.csv",
                                "ag_prodchange_ssp245_ssp5_CanESM5_NA.xml"))

})

