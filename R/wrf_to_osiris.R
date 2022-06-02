#' wrf_to_osiris
#'
#' Function that uses emulated yield data aggregated to GCAM basin level to create
#' GCAM region, basin, and irrigation level data for all GCAM commodities
#'
#' @param wrd_ncdf Default = NULL
#' @keywords test
#' @return number
#' @importFrom rlang :=
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#' library(osiris)
#' osiris::yield_to_gcam_basin()
#' }

wrf_to_osiris <- function(wrf_ncdf = NULL) {



  #.........................
  # Initialize
  #.........................

  rlang::inform("Startingwrf_to_osiris...")

  # Initialize values
  NULL

  #.........................
  # Read in Files
  #.........................

  library(sf)
  library(ncdf4)
  library(dplyr)
  library(raster)


  # Read in wrf ncdf
  wrf_ncdf <- "C:/Z/projects/current/00_IM3/process_hdhcdh/wrfout_d01_2020-01-01_01%3A00%3A00"

  # Get raster brick for Temperature
  wrf_ncdf_brick <- raster::brick(wrf_ncdf, varname = 'T2', ncdf = TRUE)
  wrf_ncdf_ras <- wrf_ncdf_brick[[1]] # Base raster
  wrf_ncdf_lat <- (raster::brick(wrf_ncdf, varname = 'XLAT', ncdf = TRUE))[[1]]
  wrf_ncdf_lon <- (raster::brick(wrf_ncdf, varname = 'XLONG', ncdf = TRUE))[[1]]

  # Convert raster in to an sf object ============================================
  # Step 0: Get Lat long
  wrf_ncdf_lat_df <- as.data.frame(wrf_ncdf_lat, xy = TRUE, na.rm = TRUE) %>%
    dplyr::rename(lat = X1); wrf_ncdf_lat_df %>% head()
  wrf_ncdf_lon_df <- as.data.frame(wrf_ncdf_lon, xy = TRUE, na.rm = TRUE) %>%
    dplyr::rename(lon = X1); wrf_ncdf_lon_df %>% head()

  # Step 1: convert to a table with lat, lon, and z
  wrf_ncdf_ras_df <- as.data.frame(wrf_ncdf_ras, xy = TRUE, na.rm = TRUE) %>%
    dplyr::rename(z = X1) %>%
    dplyr::left_join(wrf_ncdf_lat_df, by=c("x","y")) %>%
    dplyr::left_join(wrf_ncdf_lon_df, by=c("x","y")) %>%
    dplyr::select(lat,lon,z); wrf_ncdf_ras_df %>% head()

  # Step 2: convert to sf object using sf::st_as_sf
  wrf_ncdf_sf <- wrf_ncdf_ras_df %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)


  #..............................
  # Read in default osiris inputs
  #...............................
  osiris_ncdf <- "C:/Z/models/osiris_yieldEmu4GCAM/data/climate_data/tas_Amon_CanESM5_historical_r1i1p1f1_orig.nc"

  # Check Params
  ncdf_check <- ncdf4::nc_open(osiris_ncdf)

  # Get raster brick for Temperature
  osiris_ncdf_brick <- raster::brick(osiris_ncdf, varname = 'tas', ncdf = TRUE)
  osiris_ncdf_ras <- osiris_ncdf_brick[[1]] # Base raster

  # Convert raster in to an sf object ============================================

  # Step 1: convert to a table with lat, lon, and z
  osiris_ncdf_ras_df <- as.data.frame(osiris_ncdf_ras, xy = TRUE, na.rm = TRUE) %>%
    dplyr::select(lat = y, lon = x, z = X1850.01.16); osiris_ncdf_ras_df %>% head()

  # Step 2: convert to sf object using sf::st_as_sf
  osiris_ncdf_sf <- osiris_ncdf_ras_df %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

  #plot(osiris_ncdf_sf$z)


  #......................
  # Convert wrf to osiris
  #.........................

  # Step 3: reproject to correct crs
  # wrf_ncdf_sf_osiris_crs <- wrf_ncdf_sf %>%
  #  sf::st_transform(crs="osiris_proj4") # TODO: NEED THE OSIRIS PROJECTION HERE

  # Step 4: Reproject to correct resolution

  # Add grid id's for the osiris_sf and wrf_sf object -- osiris_grid_id, wrf_grid_id

  # Intersect the two sf objects
  wrf_osiris_intersect <- sf::st_intersection(osiris_ncdf_sf,wrf_ncdf_sf)
  wrf_osiris_intersect <- wrf_osiris_intersect %>%
    tibble::as_tibble()
  # Then group_by (osiris_grid_id) and aggregate each parameter (be careful about sum vs mean)

  # We have wrf data by osiris_grid_ids and we have osiris x,y




  #..............................
  # Convert parameters
  #.............................

  # Convert precipitation from mm to flux
  # %>% mutate (precipitation_flux = precip * formulaXXX)

  # Temp will stay the same


  #.........................
  # Save data
  #.........................

  # Save wrf_prceipitation data as ncdf (sf to ncdf)
  # Save wrf_temperature data as ncdf (sf to ncdf)

  #.........................
  # Close Out
  #.........................

  rlang::inform("wrf_to_osiris completed.")
}
