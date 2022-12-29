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
  growing_season_dir = paste0(data_folder,"/growing_season_climate_data"),
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
