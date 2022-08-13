#' wrf_to_osiris
#'
#' Function to extract temperature and precipitation data from WRF nc files,
#' reproject to match osiris CRS and resolution, calculate monthly mean temperature
#' and precipitation flux, and save to a new nc file.
#'
#' @param wrf_ncdf Default = NULL. Path to WRF data, which should start at the beginning
#' of a month. You can add multiple paths, just put in a list, e.g., c("/path1", "/path2")
#' @param osiris_ncdf Default = NULL. Path to osiris temperature nc file.
#' @param write_dir Default = "wrf_to_osiris". Output Folder.
#' @param time_step Default = "3 hours". Other option is "1 hour".
#' @param scenario Default = NULL. Scenario to put in output ncdf filename.
#' @keywords test
#' @return number
#' @importFrom rlang :=
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#' library(osiris)
#' osiris::wrf_to_osiris()
#' }

wrf_to_osiris <- function(wrf_ncdf = NULL,
                          osiris_ncdf = NULL,
                          write_dir = "wrf_to_osiris",
                          time_step = "3 hours",
                          scenario = NULL) {


  #.........................
  # Initialize
  #.........................

  rlang::inform("Starting wrf_to_osiris...")

  # Check write dir
  if(!dir.exists(write_dir)){dir.create(write_dir)}

  # Initialize values
  NULL -> z -> y -> x -> X1 -> .


  #.........................
  # Custom functions
  #.........................

  # Function to reproject wrf to osiris CRS and resolution. Output is a list
  # of raster layers.
  wrf_fun <- function(x) {
    # Get lat and lon from raster (first file, doesn't matter since all the same lat/lon)
    wrf_ncdf_lat <- (raster::brick(list.filepath[1], varname = 'XLAT', ncdf = TRUE))[[1]]
    wrf_ncdf_lon <- (raster::brick(list.filepath[1], varname = 'XLONG', ncdf = TRUE))[[1]]

    # Get Lat long
    wrf_ncdf_lat_df <- raster::as.data.frame(wrf_ncdf_lat, xy = TRUE, na.rm = TRUE) %>%
      dplyr::rename(lat = X1)
    wrf_ncdf_lon_df <- raster::as.data.frame(wrf_ncdf_lon, xy = TRUE, na.rm = TRUE) %>%
      dplyr::rename(lon = X1)

    # Convert to a table with lat, lon, and z
    wrf_ncdf_ras_df <- raster::as.data.frame(x, xy = TRUE, na.rm = TRUE) %>%
      dplyr::rename(z = 3) %>%
      dplyr::left_join(wrf_ncdf_lat_df, by=c("x","y")) %>%
      dplyr::left_join(wrf_ncdf_lon_df, by=c("x","y")) %>%
      dplyr::select(lat,lon,z)

    # Convert to sf object using sf::st_as_sf
    wrf_ncdf_sf <- wrf_ncdf_ras_df %>%
      sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

    # Reproject to correct crs
    sf::st_geometry(wrf_ncdf_sf) <- sf::st_geometry(wrf_ncdf_sf) + c(360, 0)

    # Reproject to correct resolution
    wrf_osiris_ras <- raster::rasterize(wrf_ncdf_sf,
                                        osiris_ncdf_ras,
                                        wrf_ncdf_sf$z,
                                        fun = mean)
  }

  #...............................
  # Read in default osiris temperature data
  #...............................

  rlang::inform("Reading osiris netcdf file and processing...")

  # Get raster brick for Temperature
  osiris_ncdf_brick <- raster::brick(osiris_ncdf, varname = 'tas', ncdf = TRUE)
  osiris_ncdf_ras <- osiris_ncdf_brick[[1]] # Base raster

  # Convert raster in to an sf object ============================================

  # Step 1: convert to a table with lat, lon, and z
  osiris_ncdf_ras_df <- raster::as.data.frame(osiris_ncdf_ras, xy = TRUE, na.rm = TRUE) %>%
    dplyr::select(lat = y, lon = x, z = 3)

  # Step 2: convert to sf object using sf::st_as_sf
  osiris_ncdf_sf <- osiris_ncdf_ras_df %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

  # Initiate list for WRF data raster bricks
  wrf_T2_brick <- list()
  wrf_RAINC_brick <- list()
  wrf_RAINNC_brick <- list()
  wrf_RAINSH_brick <- list()
  time_stamp <- list()

  # Read in wrf ncdf files using a loop
  rlang::inform("Reading WRF netcdf file and processing...")

  list.filepath <- list.files(path = wrf_ncdf, full.names = T)
  list.filepath <- list.filepath[order(gsub("[^0-9]+", "", list.filepath))] # Order files by time
  for(i in 1:length(list.filepath)){
    wrf_T2_brick[[i]] <- raster::brick(list.filepath[[i]], varname = 'T2', ncdf = TRUE)
    wrf_RAINC_brick[[i]] <- raster::brick(list.filepath[[i]], varname = 'RAINC', ncdf = TRUE)
    wrf_RAINNC_brick[[i]] <- raster::brick(list.filepath[[i]], varname = 'RAINNC', ncdf = TRUE)
    wrf_RAINSH_brick[[i]] <- raster::brick(list.filepath[[i]], varname = 'RAINSH', ncdf = TRUE)

    # Retrieve the number of layers from each file, which is equivalent to the number
    # of time steps, and define time stamp series
    layer_num <- raster::nlayers(wrf_T2_brick[[i]])
    initial_time <- as.POSIXct(paste0(substr(basename(list.filepath[i]), 12, 21), " ", substr(basename(list.filepath[i]), 23, 30)), tz = "UTC")
    time_stamp[[i]] <- as.character(seq(from = initial_time, length.out = layer_num, by = time_step))
  }

  # Make table of each time stamp
  wrf_layers <- do.call(rbind, Map(data.frame, layers = time_stamp))
  year <- substr(wrf_layers$layers, 1, 4)
  month <- substr(wrf_layers$layers, 6, 7)
  day <- substr(wrf_layers$layers, 9, 10)
  hour <- substr(wrf_layers$layers, 12, 13)
  wrf_layers <- cbind(wrf_layers, year, month, day, hour)

  # Find duplicate time stamps to remove from raster stacks (this prevents double
  # counting time stamps when finding monthly mean temperature)
  duplicate_layers <- which(duplicated(wrf_layers$layers),)
  wrf_layers <- dplyr::distinct(wrf_layers)

  # Collapse raster brick list to get single raster stack
  wrf_T2 <- raster::stack(wrf_T2_brick)

  # Remove duplicate layers
  wrf_T2 <- raster::dropLayer(wrf_T2, duplicate_layers)

  # Get unique id for each year-month combo
  wrf_layers <- wrf_layers %>%
    dplyr::group_by(year, month) %>%
    dplyr::mutate(id = dplyr::cur_group_id())

  # Create subset of raster layers based on year-month id. We drop the last year-
  # month id since we want the temperature time steps to match with precipitation,
  # which will be the delta from the beginning of the month to the next, so it won't
  # include the last time step.
  wrf_T2_sub <- list()
  for (i in 1:(length(unique(wrf_layers$id))-1)) {
    wrf_T2_sub[[i]] <- raster::subset(wrf_T2, dplyr::first(which(wrf_layers$id == i)):dplyr::last(which(wrf_layers$id == i)))
  }

  # Calculate monthly mean temperature
  wrf_T2_mean <- lapply(wrf_T2_sub, raster::mean)

  # Apply reprojection function on temperature data
  rlang::inform("Reprojecting WRF temperature data...")
  wrf_T2_ras <- lapply(wrf_T2_mean, wrf_fun)


  #.........................
  # Process precipitation data
  #.........................

  # Collapse raster brick list to get single raster stack
  wrf_RAINC <- raster::stack(wrf_RAINC_brick)
  wrf_RAINNC <- raster::stack(wrf_RAINNC_brick)
  wrf_RAINSH <- raster::stack(wrf_RAINSH_brick)

  # Remove duplicate layers
  wrf_RAINC <- raster::dropLayer(wrf_RAINC, duplicate_layers)
  wrf_RAINNC <- raster::dropLayer(wrf_RAINNC, duplicate_layers)
  wrf_RAINSH <- raster::dropLayer(wrf_RAINSH, duplicate_layers)

  # Extract layers from table corresponding to the beginning of each month
  wrf_layers_sub <- wrf_layers[match(unique(wrf_layers$id), wrf_layers$id),]

  # Extract raster layers corresponding to the beginning of each month for each
  # precipitation variable
  wrf_RAINC <- raster::subset(wrf_RAINC, match(unique(wrf_layers$id), wrf_layers$id))
  wrf_RAINNC <- raster::subset(wrf_RAINNC, match(unique(wrf_layers$id), wrf_layers$id))
  wrf_RAINSH <- raster::subset(wrf_RAINSH, match(unique(wrf_layers$id), wrf_layers$id))

  # Sum precipitation variables to get total cumulative precipitation (mm)
  wrf_RAIN <- wrf_RAINC + wrf_RAINNC + wrf_RAINSH

  # Get the monthly delta and calculate precipitation from mm to flux (just divide
  # by number of seconds in each monthly interval, since density and m to mm cancel
  # out, assuming a water density of 1000 kg/m3)
  wrf_precip <- list()
  for (i in 1:(raster::nlayers(wrf_RAIN) - 1)) {
    wrf_precip[[i]] <- wrf_RAIN[[i+1]] - wrf_RAIN[[i]]
    month_to_sec <- as.numeric(difftime(wrf_layers_sub$layers[i+1], wrf_layers_sub$layers[i], units = "secs"))
    wrf_precip[[i]] <- wrf_precip[[i]] / month_to_sec
  }

  # Apply reprojection function on precipitation flux
  rlang::inform("Reprojecting WRF precipitation data...")
  wrf_precip_ras <- lapply(wrf_precip, wrf_fun)


  #.........................
  # Save data to netcdf
  #.........................

  rlang::inform("Saving WRF to netcdf...")

  # Convert raster lists to stacks
  wrf_temperature <- raster::stack(wrf_T2_ras)
  wrf_precipitation <- raster::stack(wrf_precip_ras)

  # Longitude and Latitude data
  xvals <- unique(raster::values(raster::init(wrf_temperature, "x")))
  yvals <- unique(raster::values(raster::init(wrf_temperature, "y")))
  nx <- length(xvals)
  ny <- length(yvals)
  lon <- ncdf4::ncdim_def("lon", "degrees_east", xvals)
  lat <- ncdf4::ncdim_def("lat", "degrees_north", yvals)

  # Missing value to use
  mv <- -999

  # Time component
  time <- ncdf4::ncdim_def(name = "time",
                           units = paste0("months since ", substr(wrf_layers$layers[1], 1, 10)),
                           vals = 0:(raster::nlayers(wrf_temperature)-1),
                           unlim = TRUE,
                           longname = "time")

  # Define the temperature variables
  var_temp <- ncdf4::ncvar_def(name = "tas",
                               units = "K",
                               dim = list(lon, lat, time),
                               longname = "Near-Surface Air Temperature",
                               missval = mv,
                               compression = 9)

  # Define the precipitation flux variables
  var_prec <- ncdf4::ncvar_def(name = "pr",
                               units = "kg m-2 s-1",
                               dim = list(lon, lat, time),
                               longname = "Precipitation",
                               missval = mv,
                               compression = 9)

  # Generate the temperature file
  ncout_tas <- ncdf4::nc_create(paste0(write_dir, "/tas_wrf_", scenario, ".nc"), var_temp, force_v4 = TRUE)
  print(paste("The temperature file has", ncout_tas$nvars, "variables"))
  print(paste("The temperature file has", ncout_tas$ndim, "dimensions"))

  # Add some global attributes
  ncdf4::ncatt_put(ncout_tas, 0, "Title", "wrf_to_osiris temperature monthly output")
  ncdf4::ncatt_put(ncout_tas, 0, "Source", "WRF data")
  ncdf4::ncatt_put(ncout_tas, 0, "References", "https://jgcri.github.io/osiris/")
  ncdf4::ncatt_put(ncout_tas, 0, "Created on", date())

  # Place the temperature values in the file and loop through
  # the layers to get them to match the correct time index
  for (i in 1:raster::nlayers(wrf_temperature)) {
    ncdf4::ncvar_put(nc = ncout_tas,
                     varid = var_temp,
                     vals = raster::values(wrf_temperature[[i]]),
                     start = c(1, 1, i),
                     count = c(-1, -1, 1))
  }

  # Close the netcdf file
  ncdf4::nc_close(ncout_tas)

  # Generate the precipitation flux file
  ncout_pr <- ncdf4::nc_create(paste0(write_dir, "/pr_wrf_", scenario, ".nc"), var_prec, force_v4 = TRUE)
  print(paste("The precipitation file has", ncout_pr$nvars, "variables"))
  print(paste("The precipitation file has", ncout_pr$ndim, "dimensions"))

  # Add some global attributes
  ncdf4::ncatt_put(ncout_pr, 0, "Title", "wrf_to_osiris precipitation flux monthly output")
  ncdf4::ncatt_put(ncout_pr, 0, "Source", "WRF data")
  ncdf4::ncatt_put(ncout_pr, 0, "References", "https://jgcri.github.io/osiris/")
  ncdf4::ncatt_put(ncout_pr, 0, "Created on", date())

  # Place the precipitation values in the file and loop through
  # the layers to get them to match the correct time index
  for (i in 1:raster::nlayers(wrf_precipitation)) {
    ncdf4::ncvar_put(nc = ncout_pr,
                     varid = var_prec,
                     vals = raster::values(wrf_precipitation[[i]]),
                     start = c(1, 1, i),
                     count = c(-1, -1, 1))
  }

  # Close the netcdf file
  ncdf4::nc_close(ncout_pr)

  #.........................
  # Close Out
  #.........................

  rlang::inform("wrf_to_osiris completed.")
}
