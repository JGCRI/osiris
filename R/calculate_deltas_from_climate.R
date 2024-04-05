#' calculate_deltas_from_climate
#'
#' Function to process climate data to calculate delta P and delta T.
#'
#' @param climate_dir Default = NULL
#' @param write_dir Default = "outputs_calculate_delta_from_climate". Output Folder
#' @param esm_name Default = NULL
#' @param scn_name Default = NULL
#' @param crops Default = c("Corn", "Spring wheat", "Winter wheat", "Rice", "Soy")
#' @param irrigation_rainfed Default = c("IRR", "RFD")
#' @param minlat Default = -89.75
#' @param minlon Default = -179.75
#' @param monthly_growing_season Default = NULL. A csv file with columns latgrid, longrid, crop, irr, pmonth,	gslength, areamask
#' @param monthly_harvest_season Default = NULL. A csv file with columns latgrid, longrid, crop, irr, pmonth,	hmonth, areamask
#' @param rollingAvgYears Default = 15
#' @param growing_season_dir Default = NULL
#' @param tas_historical = NULL. Filename of historical temperature ncdf.
#' @param tas_projected = NULL. Filename of projected (hot or cold) temperature ncdf.
#' @param pr_historical = NULL. Filename of historical precipitation flux ncdf.
#' @param pr_projected = NULL. Filename of projected (hot or cold) precipitation flux ncdf.
#' @param historical_start_year = NULL. Start year of historical data.
#' @param projection_start_year = NULL. Start year of projection data.
#' @keywords test
#' @return number
#' @importFrom rlang :=
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#' library(osiris)
#' osiris::calculate_deltas_from_climate()
#' }

calculate_deltas_from_climate <- function(climate_dir = NULL,
                                          write_dir = "outputs_calculate_delta_from_climate",
                                          esm_name = NULL,
                                          scn_name = NULL,
                                          crops = c("Corn", "Spring wheat", "Winter wheat", "Rice", "Soy"),
                                          irrigation_rainfed = c("IRR", "RFD"),
                                          minlat = -89.75,
                                          minlon = -179.75,
                                          monthly_growing_season = NULL,
                                          rollingAvgYears = 15,
                                          growing_season_dir = NULL,
                                          tas_historical = NULL,
                                          tas_projected = NULL,
                                          pr_historical = NULL,
                                          pr_projected = NULL,
                                          historical_start_year = NULL,
                                          projection_start_year = NULL) {



  #.........................
  # Initialize
  #.........................

  rlang::inform("Starting calculate_deltas_from_climate...")

  # Check write dir
  if(!dir.exists(write_dir)){dir.create(write_dir)}

  # Make a directory for growing_season_dir if not there
  if(!dir.exists(growing_season_dir)){dir.create(growing_season_dir)}

  # Initialize values
  NULL -> areamask -> basePr -> baseTemp -> crop -> gslength -> hmonth ->
    irr -> keep -> lat -> latgrid -> lon -> longrid -> month -> pmonth ->
    time -> time_id -> time_index -> value -> year -> .

  # Increase memory limits
  utils::memory.limit(size=56000)

  # Check that the crops in the input argument are valid
  crops <- stringi::stri_trans_totitle(crops)
  crops <- unique(crops)
  if(!all(crops %in% c("Corn", "Spring Wheat", "Winter Wheat", "Rice", "Soy"))) {
    stop("The crops that were defined are either misspelled or not included in this package.
         The crops currently included are: Corn, Spring Wheat, Winter Wheat, Rice, Soy")
  }

  #.........................
  # Custom functions
  #.........................

  # function to convert dates
  ##TODO you'll have to adjust this depending on your calendar and units in climate data:
  # months since <start_year>-01-01
  convert_time <- function(time, start_year){
    time %>%
      dplyr::mutate(time_index = as.integer(row.names(.)) - 1,
             month = floor(time_index %% 12) + 1,
             year = floor(time_index/ 12) + start_year,
             time_id = paste0(month, '~', year )) %>%
      dplyr::select(time, time_id)
  }

  ### function to keep relevant months in growing season
  keep_months_and_avg <- function(data,crp, irrig){

    ag_practice_info %>%
      dplyr::filter(crop == crp,
             irr == irrig) ->
      crop_practice_info
    # ggplot(crop_practice_info, aes(x=longrid, y=-latgrid)) + geom_raster(aes(fill=pmonth))
    # #^ to show that the lat/lon ordering on the growing season mask is weird, which is why
    # #  we have to do the weird adjustments in the next block before joining.

    data %>%
      ##TODO this is definitely imperfect because the growing season grid is finer than
      #      the climate data grid, just picking the growing season grid cell that seems
      #      about closest to the lat/lon id of each climate grid cell
      dplyr::mutate(lat2 = -lat, # to match the crop mask grids
             latgrid = round(((lat2 - minlat) * 2) + 1, 0),
             lon2 = dplyr::if_else(lon > 180, lon - 360, lon) , # to match the crop mask grids
             longrid = round(((lon2 - minlon) * 2) + 1, 0)) %>%
      dplyr::select(-lat2, -lon2) %>%
      dplyr::left_join(crop_practice_info, by = c("latgrid", "longrid")) %>%
      dplyr::filter(pmonth != 0, gslength != 0) %>%
      dplyr::mutate(keep = dplyr::if_else(pmonth *30 + gslength < 365, # if growing season doesn't cross into new year
                            dplyr::if_else(month >= pmonth & month <= hmonth, 1, 0), # if month is between pmonth and hmonth, keep it; otherwise dump it
                            # now this is for planting seasons that DO cross into the new year, which means
                            # keep if month > pmonth OR < hmonth
                            dplyr::if_else(month >= pmonth | month <= hmonth, 1, 0))) %>%
      dplyr::filter(keep == 1) %>%
      dplyr::select(-keep) %>%
      dplyr::group_by(lon, lat, latgrid, longrid, crop, irr, year) %>%
      dplyr::summarise(value = mean(value)) %>%
      dplyr::ungroup()
  }



  #.........................
  # Read in Growing Season Info
  #.........................

  ##TODO you'll need to track down the actual ggcmi phase 2 growing season masks,
  ##     these are just ones I had.
  # read in table of planting and harvesting months for each grid cell by crop, irr
  ag_practice_info <- data.table::fread(monthly_growing_season)


  #.........................
  # Load the T and P data, process by crop-irr combos to get growing season-averaged
  # T and P values in each year
  #.........................

  for (varname in c('tas', 'pr')){

    # The names of the files you want to work with:
    # ##TODO you'll probably have different files name structure than the example hadgem data I had
    #        handy. It will probably look more like this commented block for `batch0`

    if (varname == "tas") {
      batch0 <- c(paste0(climate_dir, "/", tas_historical),
                  paste0(climate_dir, "/", tas_projected))
    } else if (varname == "pr") {
      batch0 <- c(paste0(climate_dir, "/", pr_historical),
                  paste0(climate_dir, "/", pr_projected))
    }

    # load these files in memory

    # The historical file so we have 1980-2014 baseline years
    ncfile1 <- ncdf4::nc_open(batch0[1])
    # pull off lon/lat/time info
    lat1 <- ncdf4::ncvar_get(ncfile1,"lat")
    lon1 <- ncdf4::ncvar_get(ncfile1,"lon")
    time1 <- ncdf4::ncvar_get(ncfile1, "time")

    # reshape and Assign them to a holder variable x so that we just have a matrix of data
    # each row of x is a single grid cell's time series, each column is a time slice across all grids
    x1 <- t(rbind(matrix(aperm(ncdf4::ncvar_get(ncfile1,varname),c(3,1,2)),length(time1),length(lat1)*length(lon1))))
    colnames(x1) <- convert_time(time = data.frame(time=time1),
                                 start_year = historical_start_year)$time_id

    # The projection file, which starts in 2015 in CMIP6
    ncfile2 <- ncdf4::nc_open(batch0[2])
    # pull off lon/lat/time info
    lat2 <- ncdf4::ncvar_get(ncfile2,"lat")
    lon2 <- ncdf4::ncvar_get(ncfile2,"lon")
    time2 <- ncdf4::ncvar_get(ncfile2, "time")

    # reshape and Assign them to a holder variable x so that we just have a matrix of data
    # each row of x is a single grid cell's time series, each column is a time slice across all grids
    x2 <- t(rbind(matrix(aperm(ncdf4::ncvar_get(ncfile2,varname),c(3,1,2)),length(time2),length(lat2)*length(lon2))))
    colnames(x2) <- convert_time(time = data.frame(time=time2),
                                 start_year = projection_start_year)$time_id


    # make one data frame with all years
    grid <- expand.grid(list(lon=lon1,lat=lat1))

    rlang::inform(paste0("Reading in and reshaping the following ncdf files and vars: "))
    rlang::inform(paste0("ncdf file 1: ",batch0[1],", var: ", varname, "..."))
    rlang::inform(paste0("ncdf file 2: ",batch0[2],", var: ", varname, "..."))

    x_comb <- cbind(grid, x1, x2) %>%
      dplyr::select(unique(colnames(.)))

    rm(x1, x2)

    # Use lapply here to speed up the separate() function
    x_comb_list <- lapply(names(x_comb)[-(1:2)], function(x) {
      x_comb[c("lon", "lat", x)] %>%
        tidyr::pivot_longer(cols = -(1:2), names_to = "time", values_to = "value") %>%
        tidytable::separate(col = time, into = c('month', 'year'), sep = '~')
    }
    )

    rm(x_comb)

    # Merge the list generated by lapply
    x_comb_merge <- data.table::rbindlist(x_comb_list) %>%
      dplyr::mutate(year = as.integer(year),
                    month=as.integer(month)) %>%
      dplyr::filter(year >= historical_start_year)

    rm(x_comb_list)

    rlang::inform(paste0("Reading in and reshaping ncdf files complete."))

    # pull off the climate data for each crop-irr combo
    for(crp in crops){
      for(irrig in irrigation_rainfed){

        x_comb_merge  %>%
          keep_months_and_avg(., crp=crp, irrig=irrig) %>%
          utils::write.csv(., paste0(growing_season_dir, '/', esm_name, '_', scn_name,'_', varname,
                              '_', crp, '_', irrig,
                              '_growing_season_avg.csv'),
                    row.names = F)

      }
    }
    rm(x_comb_merge)
  }

  ###############################################################################
  # Smooth T and P growing season average values so that we have the long term
  # growing season average values
  #
  # Then, calculate changes relative to 1980-2014 so that the deltas we have
  # are changes in long term growing season average values relative to baseline.
  # Here we choose 1980-2014 since this overlaps the historical period from the
  # ACCESS climate date for rest-of-the-world.
  #
  # Pretty sure the order doesn't matter for deltaT because it's additive but
  # deltaP is multiplicative so it does matter
  #
  # ## NOTE:
  # The latgrid and longrid columns are for matching up with the area masks I have.
  # They could presumably be dropped but since the masks I have for winter and
  # spring wheat are the same masks with the same latgrid and longrid, keep for now.
  ###############################################################################

  # Helper fcn
  rollAvg <-  function(x,n){
    y <- stats::filter(x,rep(1/n,n), sides=2)
    ind <- which(is.na(y))

    for(i in 1:length(ind)){
      left <- max(1,ind[i] - (n-1) /2)
      right <- min( max(ind), ind[i] + (n-1) /2)
      y[ind[i]] <- mean(x[left:right])
    }

    y
  }


  # get deltaT, deltaP for each crop-irr combo
  for(crp in crops){
    for(irrig in irrigation_rainfed){

      # get the T and P growing season average files loaded for this crop-irr combo:
      filenames <- list.files(growing_season_dir, pattern = paste0(esm_name, '_', scn_name), full.names=TRUE, recursive=FALSE)
      filenames <- filenames[grepl(paste0(crp, '_', irrig), filenames)]
      tas_file <- filenames[grepl('_tas_', filenames)]
      pr_file <- filenames[grepl('_pr_', filenames)]

      tas <- utils::read.csv(tas_file, stringsAsFactors = F)
      pr <- utils::read.csv(pr_file, stringsAsFactors = F)

      # long term average values
      if (rollingAvgYears > 0) {
        tas %>%
          dplyr::group_by(lon, lat, latgrid, longrid, crop, irr) %>%
          dplyr::mutate(value = as.numeric(rollAvg(value, n = (2 * rollingAvgYears + 1)))) %>%
          dplyr::ungroup() ->
          tas2

        pr %>%
          dplyr::group_by(lon, lat, latgrid, longrid, crop, irr) %>%
          dplyr::mutate(value = as.numeric(rollAvg(value, n = (2 * rollingAvgYears + 1)))) %>%
          dplyr::ungroup() ->
          pr2
      } else {
        tas2 <- tas
        pr2 <- pr
      }


      # baseline averages
      tas2 %>%
        dplyr::filter(year >= 1980,
               year <= 2014) %>%
        dplyr::group_by(lon, lat, latgrid, longrid, crop, irr) %>%
        dplyr::summarise(baseTemp = mean(value)) %>%
        dplyr::ungroup() ->
        baseT

      pr2 %>%
        dplyr::filter(year >= 1980,
               year <= 2014) %>%
        dplyr::group_by(lon, lat, latgrid, longrid, crop, irr) %>%
        dplyr::summarise(basePr = mean(value)) %>%
        dplyr::ungroup() ->
        baseP


      # deltas:
      tas2 %>%
        dplyr::filter(year >= 1980) %>%
        dplyr::left_join(baseT, by = c("lon", "lat", "latgrid", "longrid", "crop", "irr")) %>%
        dplyr::mutate(deltaT = value - baseTemp) %>% # deltaT is additive
        dplyr::select(-baseTemp, -value) ->
        deltaT

      pr2 %>%
        dplyr::filter(year >= 1980) %>%
        dplyr::left_join(baseP, by = c("lon", "lat", "latgrid", "longrid", "crop", "irr")) %>%
        dplyr::mutate(deltaP = 1 +((value - basePr)/basePr)) %>% #deltaP is multiplicative
        dplyr::select(-basePr, -value) ->
        deltaP

      # set longrid and latgrid to integer (to address strange error on pic where there
      # is a mismatch in column type when joining deltaT and deltaP below)
      cols <- names(deltaP)[3:4] # both the same for deltaT and deltaP
      deltaT[cols] <- lapply(deltaT[cols], as.integer)
      deltaP[cols] <- lapply(deltaP[cols], as.integer)


      # combine and save off:
      deltaT %>%
        dplyr::left_join(deltaP, by = c("lon", "lat", "latgrid", "longrid", "crop", "irr", 'year'))%>%
        utils::write.csv(., paste0(write_dir, '/', esm_name, '_', scn_name, '_', tolower(crp), '_', tolower(irrig), '_smooth_deltaT_deltaP.csv'),
                  row.names = F)

    }
  }

  # # #######
  # # some code for easy plotting for sanity checks if needed
  # # #######
  # pT <- ggplot(data = dplyr::filter(deltaT, year == 2085), aes(x = lon, y = lat)) +
  #   geom_raster(aes(fill = deltaT)) +
  #   theme_bw()
  # pT
  #
  #
  # pP <- ggplot(data = dplyr::filter(deltaP, year == 2085), aes(x = lon, y = lat)) +
  #   geom_raster(aes(fill = deltaP)) +
  #   theme_bw()
  # pP

  #.........................
  # Close out
  #.........................

  rlang::inform("calculate_deltas_from_climate complete.")

  return()
}
