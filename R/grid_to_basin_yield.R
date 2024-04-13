#' grid_to_basin_yield
#'
#' Function that uses emulators to create gridded yield files then aggregates to
#' gcam basin level via MIRCA harvested areas.
#'
#' @param carbon Default = NULL
#' @param weight_floor_ha Default = 1
#' @param emulator_dir Default = NULL
#' @param input_dir Default = NULL
#' @param area_dir Default = NULL
#' @param basin_grid Default = NULL
#' @param basin_id Default = NULL
#' @param region_id Default = NULL. Filters to specified GCAM region (1-32), otherwise no filter
#' @param gridded_yield_dir Default = NULL.
#' @param write_dir Default = "step2_grid_to_basin_yield". Output Folder
#' @param crops Default = c("Corn", "Rice", "Soy", "Spring Wheat", "Winter Wheat")
#' @param esm_name Default = 'WRF'
#' @param cm_name Default = 'LPJmL'
#' @param scn_name Default = 'rcp8p5_hotter'
#' @param N Default = 200. Assuming nothing is nitrogen limited and apply across grids
#' @keywords test
#' @return number
#' @importFrom rlang :=
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#' library(osiris)
#' osiris::grid_to_basin_yield()
#' }

grid_to_basin_yield <- function(carbon = NULL,
                                weight_floor_ha = 1,
                                emulator_dir = NULL,
                                input_dir = NULL,
                                area_dir = NULL,
                                basin_grid = NULL,
                                basin_id = NULL,
                                region_id = NULL,
                                gridded_yield_dir = NULL,
                                write_dir = "step2_grid_to_basin_yield",
                                crops = c("Corn", "Rice", "Soy", "Spring Wheat", "Winter Wheat"),
                                esm_name = 'WRF',
                                cm_name = 'LPJmL',
                                scn_name = 'rcp8p5_hotter',
                                N = 200) {



  #.........................
  # Initialize
  #.........................

  rlang::inform(paste0("Starting grid_to_basin_yield for ", cm_name, "_",
                       esm_name, "_", scn_name, ": ", paste(crops, collapse = ", ")))

  # Check write dir
  if(!dir.exists(write_dir)){dir.create(write_dir)}

  # Make a directory for gridded_yield if not there
  if(!dir.exists(gridded_yield_dir)){dir.create(gridded_yield_dir)}

  # Initialize values
  NULL -> year -> value -> vtag -> units -> ctag -> type -> Scenario ->
    model -> ID -> long -> lati -> x -> y -> countryID -> protected -> flowD ->
    elevation -> elevD -> eleD_min -> eleD_max -> regID -> inGrandELEC ->
    basinID -> GCAM_basin_ID -> gcammaptools_name -> GCAM_basin_name ->
    Basin_name -> GLU_code -> GLU_name -> ISO -> ISO_NUM -> Country_name ->
    lon -> lat -> latgrid -> longrid -> crop -> irr -> deltaT -> deltaP ->
    crop_lon -> crop_lat -> C -> yield -> gcm -> cropmodel -> HA -> param_sum ->
    orig_lon -> coarse_lon -> coarse_lat -> id -> swh_ir -> wwh_ir -> swh_rf ->
    wwh_rf -> .

  # Check that the crops in the input argument are valid
  crops <- unique(crops)
  if(!all(crops %in% c("Corn", "Rice", "Soy", "Spring Wheat", "Winter Wheat"))) {
    stop("The crops that were defined are either misspelled or not included in this package.
         The crops currently included are: Corn, Rice, Soy, Spring Wheat, Winter Wheat")
  }

  #.........................
  # Custom functions
  #.........................

  approx_fun <- function(year, value, rule = 1) {
    if(rule == 1 | rule == 2 ) {
      tryCatch(stats::approx(as.vector(year), value, rule = rule, xout = year)$y,
               error=function(e) NA)
    } else {
      stop("Not implemented yet!")
    }
  }


  # This matches yield data to harvested area data
  # ... both of which come in different files.
  match_crop <- function(CROP){
    croplist <- c("crop01" = "Wheat",
                  "crop02" = "Corn",
                  "crop03" = "Rice",
                  "crop08" = "Soy",
                  "crop11" = "cas",
                  "crop25" = "mgr",
                  "crop06" = "mil",
                  "crop16" = "nut",
                  "crop17" = "pea",
                  # "crop17" = "ben", # EPIC ben = Drybeans is equivalent to LPJmL pea = field peas; both receive HA for MIRCA crop 17
                  "crop15" = "rap",
                  "crop13" = "sgb",
                  "crop12" = "sug",
                  "crop09" = "sun",
                  "crop04" = "bar",
                  "crop07" = "sor",
                  "crop21" = "cot")

    if(CROP %in% names(croplist)) {
      croplist[CROP]
    }
  } # match_crop


  # Take ISIMIP irrtype name and match to MIRCA irrtype name
  match_irrtype <- function(IRRTYPE){
    irrtypelist <- c("rfc" = "rf",
                     "irc" = "ir")
    if(IRRTYPE %in% names(irrtypelist)) {
      irrtypelist[IRRTYPE]
    } else {
      stop("Unknown irrtype: ", IRRTYPE)
    }
  } # match_irrtype


  ## TODO when climate data on finer grid than crop emulation (0.5deg):
  ##       probably will want to use class::knn1 to figure out which crop grid every climate grid goes in,
  ##       then just apply that crop grid's coefficients to the climate grid. So basically switch
  ##       the roles of input_grid and grid in the call to the function I'm about to define :
  ## TODO: move this function outside of the loop, I just put it here so it was clearer what
  ##       arguments to switch.
  ## TODO: make sure the orientations for lon/lat are same across all data sets, also there's
  ##       probably some weird matches being returned by knn1 around where lon (or lat) = 0 -
  ##       the matched lat (or lon) may be randomly assigned the plus or minus side
  ##       latitude (or lon) matched in.
  ## NOTE: there's also probably a better way to do this
  grid_matching <- function(finer_grid, coarser_grid){
    return(do.call(rbind, apply(finer_grid, MARGIN = 1, function(row){
      match_id <- as.numeric(class::knn1(coarser_grid, c(row[1], row[2]),1:nrow(coarser_grid) ))
      match <- coarser_grid[match_id,]
      return(data.frame(lon = as.numeric(row[1]),
                        lat = as.numeric(row[2]),
                        coarse_lon = match$lon,
                        coarse_lat = match$lat))
    }))
    )
  }


  # This may take a while since it's evaluating a polynomial at every grid
  # point in every year
  eval_yield <- function(inputs, params, matched_grids, irrig=F){

    # Because my climate data (inputs) Is on a coarser grid than
    # my crop data (params), I have to do an apply over the crop
    # rows and join in the climate data that corresponds.
    ## TODO you may need to do the split on climate data instead of crop data and
    ##      reverse their roles in the below code
    params %>%
      dplyr::mutate(split_id = paste0(lon, '~', lat)) ->
      params
    params_list <- split(params, f=params$split_id)

    # For irrigated crops, yield is calculated by applying the 19 non-NA variables
    # to the 19 terms in GMD paper eqn 1 that don't have W (precipitation) terms
    # in them.
    if(irrig == T){
      yields <- do.call(rbind, lapply(params_list, function(param_row){
        # param_row <- params_list[[1]]

        matched_grids %>%
          dplyr::filter(crop_lon == param_row$lon,
                        crop_lat == param_row$lat) ->
          matched_point

        inputs %>%
          dplyr::filter(lon == matched_point$clim_lon,
                        lat == matched_point$clim_lat ) %>%
          dplyr::mutate(crop_lon = matched_point$crop_lon,
                        crop_lat = matched_point$crop_lat,
                        yield = param_row$`1` +
                          param_row$`2`*C +
                          param_row$`3`*deltaT +
                          param_row$`4`*N +
                          param_row$`5`*C^2 +
                          param_row$`6`*C*deltaT +
                          param_row$`7`*C*N +
                          param_row$`8`*deltaT^2 +
                          param_row$`9`*deltaT*N +
                          param_row$`10`*N^2 +
                          param_row$`11`*C^3 +
                          param_row$`12`*C^2*deltaT +
                          param_row$`13`*C^2*N +
                          param_row$`14`*C*deltaT^2 +
                          param_row$`15`*C*deltaT*N +
                          param_row$`16`*C*N^2 +
                          param_row$`17`*deltaT^3 +
                          param_row$`18`*deltaT^2*N +
                          param_row$`19`*deltaT*N^2
          ) ->
          yield
        return(yield)
      }))
    } else if (irrig == F) {
      yields <- do.call(rbind, lapply(params_list, function(param_row){
        # param_row <- params_list[[1]]

        matched_grids %>%
          dplyr::filter(crop_lon == param_row$lon,
                        crop_lat == param_row$lat) ->
          matched_point

        inputs %>%
          dplyr::filter(lon == matched_point$clim_lon,
                        lat == matched_point$clim_lat ) %>%
          dplyr::mutate(crop_lon = matched_point$crop_lon,
                        crop_lat = matched_point$crop_lat,
                        yield = param_row$`1` +
                          param_row$`2`*C +
                          param_row$`3`*deltaT +
                          param_row$`4`*deltaP +
                          param_row$`5`*N +
                          param_row$`6`*C^2 +
                          param_row$`7`*C*deltaT +
                          param_row$`8`*C*deltaP +
                          param_row$`9`*C*N +
                          param_row$`10`*deltaT^2 +
                          param_row$`11`*deltaT*deltaP +
                          param_row$`12`*deltaT*N +
                          param_row$`13`*deltaP^2 +
                          param_row$`14`*deltaP*N +
                          param_row$`15`*N^2 +
                          param_row$`16`*C^3 +
                          param_row$`17`*C^2*deltaT +
                          param_row$`18`*C^2*deltaP +
                          param_row$`19`*C^2*N +
                          param_row$`20`*C*deltaT^2 +
                          param_row$`21`*C*deltaT*deltaP +
                          param_row$`22`*C*deltaT*N +
                          param_row$`23`*C*deltaP^2 +
                          param_row$`24`*C*deltaP*N +
                          param_row$`25`*C*N^2 +
                          param_row$`26`*deltaT^3 +
                          param_row$`27`*deltaT^2*deltaP +
                          param_row$`28`*deltaT^2*N +
                          param_row$`29`*deltaT*deltaP^2 +
                          param_row$`30`*deltaT*deltaP*N +
                          param_row$`31`*deltaT*N^2 +
                          param_row$`32`*deltaP^3 +
                          param_row$`33`*deltaP^2*N +
                          param_row$`34`*deltaP*N^2
          ) ->
          yield
        return(yield)
      }))
    }
    row.names(yields) <- NULL
    return(yields %>%
             dplyr::select(crop, irr, crop_lon, crop_lat, year, yield, deltaT, deltaP))
  }


  ## NOTE to aggregate from gridded yields to basin yields, we will have
  ##      to use the MIRCA harvested area data. Which is only on a half
  ##      degree grid.
  ##      For the IM3 experiment where you have finer grid in the US in
  ##      weather data, I think what you'll want to do is aggregate
  ##      the yields in those 12km2 grid cells to half degree grid using
  ##      the land area in each 12km2 grid cell as the weight. That will
  ##      result in you having half degree gridded yields in all grid cells,
  ##      but the finer scale climate data will have an area weighted effect.
  ##      Then you'll do the aggregation with MIRCA harvested area data below:
  aggregate_halfdeg_yield2basin <- function(halfdeg_yield){

    if(unique(halfdeg_yield$crop) %in% c('Spring Wheat', 'Winter Wheat')){
      area_crop_name <- 'Wheat'
    }else{
      area_crop_name <- unique(halfdeg_yield$crop)
    }

    if(unique(halfdeg_yield$irr) == 'IRR'){
      area_irr_label <- 'ir'
    }else{
      area_irr_label <- 'rf'
    }

    # get corresponding area
    area.grid <- get(paste0("area.grid.", area_crop_name, ".", area_irr_label))

    # Join in the harvested area and year+yield data for each grid cell
    # in a basin.
    # Then use together to perform correctly weighted yield aggregation,
    # keeping weights = HA
    basinGrid %>%
      dplyr::rename(lon = long, lat=lati) %>%
      # area
      dplyr::left_join(area.grid, by = c("lon", "lat")) %>%
      # yield and year
      dplyr::left_join(halfdeg_yield, by = c("lon", "lat")) %>%
      stats::na.omit() %>%
      # aggregate to basin:
      dplyr::group_by(gcm, cropmodel, crop, irr, basinID, year) %>%
      dplyr::summarise(yield = stats::weighted.mean(yield, w = HA),
                       HA = sum(HA)) %>%
      dplyr::ungroup() %>%
      # calculate basin level yield
      dplyr::rename(id = basinID) %>%
      dplyr::mutate(yield = dplyr::if_else(HA == 0, 0, yield),
                    rcp = scn_name) ->
      yield_basin
    return(yield_basin)
  }

  #.........................
  # Read in Area Files
  #.........................

  # NOTE: mask applied to growing season info in step0 already included some small area mask
  # NOTE: This also means the climate data I have has been masked to land only grid cells -
  #       so some longitudes just don't show up in the climate data because they're oceans.
  #       I don't know how much that might cause an issue in the code.

  # emulator_dir sourced from https://zenodo.org/record/3592453#.Ygqzhu7MJ7M
  # too large for github.
  ## TODO download for self
  emulatorlist <- list.files(path=emulator_dir, full.names=TRUE, recursive=FALSE)
  emulatorlist <- emulatorlist[grepl('A0', emulatorlist)] # only keep non-adaptation scenarios

  inputlist <- list.files(path=input_dir, pattern = paste0(esm_name, "_", scn_name), full.names=TRUE, recursive=FALSE)

  areafilelist <- list.files(path=area_dir, full.names=TRUE, recursive=FALSE)
  # there's only 4 crops in the ggcmi phase2 emulators. Subset the area
  # files to these to save on time and memory:
  areafilelist <- areafilelist[grepl("crop01", areafilelist) |
                                 grepl("crop02", areafilelist) |
                                 grepl("crop03", areafilelist) |
                                 grepl("crop08", areafilelist)]

  # CO2
  carbon <- utils::read.csv(paste0(carbon), stringsAsFactors = F) %>%
    dplyr::mutate(value = dplyr::if_else(value > 810, 810, value))

  #.........................
  # Maps
  #.........................

  tibble::as_tibble(utils::read.csv(paste0(basin_grid), stringsAsFactors = F)) %>%
    dplyr::select(long, lati, basinID, regID) %>%
    dplyr::filter(basinID != 999) ->
    basinGrid

  # Filter region ID to GCAM region (if applicable)
  if (!is.null(region_id)) {
    basinGrid <- dplyr::filter(basinGrid, regID == region_id) %>%
      dplyr::select(-regID)
  } else {
    basinGrid <- dplyr::select(basinGrid, -regID)
  }

  basinIDs <- tibble::as_tibble(utils::read.csv(paste0(basin_id), stringsAsFactors = F))

  #.........................
  # Process all harvested area file
  #.........................

  invisible(for(a in areafilelist){
    # area .asc
    areaFname <- a

    # crop:
    area.crop <-  match_crop(paste0("crop", strsplit((strsplit(areaFname[length(areaFname)], "crop")[[1]][2]), "_")[[1]][1]))

    # irrigation:
    area.irr <- match_irrtype(strsplit((strsplit(areaFname[length(areaFname)], "_crop")[[1]][1]), "harvested_")[[1]][2])

    # file:
    try(area <- raster::raster(areaFname), silent=TRUE)

    # coordinates:
    area.coord <- raster::coordinates(area)

    # value:
    area.vals <- raster::extract(area, raster::coordinates(area))

    # gridded table to join to yield:
    assign(paste0("area.grid.", area.crop, ".", area.irr),
           area.coord %>%
             cbind(area.vals) %>%
             tibble::as_tibble() %>%
             dplyr::rename(lon = x,
                           lat = y,
                           HA   = area.vals) %>%
             # cull any grid cells with very small HA
             dplyr::mutate(HA = dplyr::if_else(HA < weight_floor_ha, 0, HA)))

    rm(area)
    rm(area.crop)
    rm(area.irr)
  })

  # clean up unneeded file
  rm(area.coord)

  #.........................
  # Yield
  #.........................

  for(crop in crops){

    rlang::inform(paste0("Generating gridded yield for ", crop))

    # yield emu netcdf
    if(crop == "Corn"){
      ncfname <- emulatorlist[grepl("maize", emulatorlist) & grepl(cm_name, emulatorlist)]
    }else{
      ncfname <- emulatorlist[grepl(sub(" ", "_", tolower(crop)), emulatorlist) & grepl(cm_name, emulatorlist)]
    }

    # get emulation parameters.
    ncin <- ncdf4::nc_open(ncfname)

    nc_lon <- ncdf4::ncvar_get(ncin,'lon')
    nc_lat <- ncdf4::ncvar_get(ncin, 'lat')
    grid <- expand.grid(list(lon=nc_lon,lat=nc_lat))

    # pull and reshape rainfed params
    rf_params_3d_array <- ncdf4::ncvar_get(ncin, 'K_rf')
    indlon <- which(dim(rf_params_3d_array)==length(nc_lon))
    indlat <- which(dim(rf_params_3d_array)==length(nc_lat))
    indpoly <- which(dim(rf_params_3d_array)==ncin$dim$poly$len)

    rf_params <-cbind(grid,
                      t(rbind(matrix(aperm(rf_params_3d_array, c(indpoly,indlon,indlat)),
                                     ncin$dim$poly$len,length(nc_lat)*length(nc_lon))
                      ))
    )
    rm(rf_params_3d_array)


    # pull and reshape irrigated params
    ir_params_3d_array <- ncdf4::ncvar_get(ncin, 'K_ir')
    indlon <- which(dim(ir_params_3d_array)==length(nc_lon))
    indlat <- which(dim(ir_params_3d_array)==length(nc_lat))
    indpoly <- which(dim(ir_params_3d_array)==ncin$dim$poly$len)

    ir_params <-cbind(grid,
                      t(rbind(matrix(aperm(ir_params_3d_array, c(indpoly,indlon,indlat)),
                                     ncin$dim$poly$len,length(nc_lat)*length(nc_lon))
                      ))
    )
    rm(ir_params_3d_array)
    ncdf4::nc_close(ncin)

    # The crop responses include every grid cells, even ones with 0 response.
    # Drop the cells with 0 response to speed things up.

    ir_params[is.na(ir_params)] <- 0
    ir_params$param_sum <- rowSums(abs(ir_params[3:36]))
    ir_params %>%
      dplyr::filter(param_sum != 0 )  %>%
      dplyr::select(-param_sum) ->
      ir_params

    rf_params$param_sum <- rowSums(abs(rf_params[3:36]))
    rf_params %>%
      dplyr::filter(param_sum != 0 )  %>%
      dplyr::select(-param_sum) ->
      rf_params


    # read in climate data
    rf_tp <- utils::read.csv(inputlist[grepl(tolower(crop), inputlist) & grepl('rfd', inputlist)], stringsAsFactors = F) %>%
      dplyr::mutate(deltaP = dplyr::if_else(deltaP > 1.3, 1.3, deltaP))
    ir_tp <- utils::read.csv(inputlist[grepl(tolower(crop), inputlist) & grepl('irr', inputlist)], stringsAsFactors = F) %>%
      dplyr::mutate(deltaP = dplyr::if_else(deltaP > 1.3, 1.3, deltaP))

    # Combine C, N, T and P data with ir and rfd params to get ir, rf yields in each
    # grid cell in each year

    # get the grid of the climate inputs
    rf_tp %>%
      tidyr::drop_na() %>%
      dplyr::select(lon, lat) %>%
      dplyr::distinct() %>%
      dplyr::mutate(orig_lon = lon,
                    lon = dplyr::if_else(lon > 179.75, lon -360, lon)) ->
      rfinput_grid

    ir_tp %>%
      tidyr::drop_na() %>%
      dplyr::select(lon, lat) %>%
      dplyr::distinct() %>%
      dplyr::mutate(orig_lon = lon,
                    lon = dplyr::if_else(lon > 179.75, lon -360, lon)) ->
      irinput_grid

    # and the grid of the crop responses
    ir_params %>%
      dplyr::select(lon, lat) %>%
      dplyr::distinct() ->
      ircrop_grid

    rf_params %>%
      dplyr::select(lon, lat) %>%
      dplyr::distinct() ->
      rfcrop_grid


    # now call the grid_matching function so that we can have
    # the table of whch climate grids pair with which crop grids.
    ## TODO: this is where to switch which grid is finer and coarser:
    ir_matched_grids <- grid_matching(finer_grid = irinput_grid%>%
                                        dplyr::select(-orig_lon),
                                      coarser_grid = ircrop_grid )%>%
      dplyr::rename(crop_lon = lon, crop_lat = lat,
                    clim_lon = coarse_lon, clim_lat = coarse_lat)

    rf_matched_grids <- grid_matching(finer_grid = rfinput_grid%>%
                                        dplyr::select(-orig_lon),
                                      coarser_grid = rfcrop_grid )%>%
      dplyr::rename(crop_lon = lon, crop_lat = lat,
                    clim_lon = coarse_lon, clim_lat = coarse_lat)


    # Ok now we have all the info we need for different grids.
    # Make the main tables of inputs for ir, rf
    ir_tp %>%
      # TODO: update this with your specific info for nitrogen and CO2 in
      #       your scenario:
      dplyr::mutate(N=N) %>%
      dplyr::left_join(carbon %>%
                         dplyr::select(year, value), by = 'year') %>%
      dplyr::rename(C = value) %>%
      dplyr::select(-longrid, -latgrid) %>%
      dplyr::mutate(orig_lon = lon,
                    lon = dplyr::if_else(lon > 179.75, lon -360, lon)) ->
      ir_inputs

    rf_tp %>%
      tidyr::drop_na() %>%
      # TODO: update this with your specific info for nitrogen and CO2 in
      #       your scenario:
      dplyr::mutate(N=N) %>%
      dplyr::left_join(carbon %>%
                         dplyr::select(year, value), by = 'year') %>%
      dplyr::rename(C = value) %>%
      dplyr::select(-longrid, -latgrid) %>%
      dplyr::mutate(orig_lon = lon,
                    lon = dplyr::if_else(lon > 179.75, lon -360, lon)) ->
      rf_inputs

    # Filter the crop parameters to only include grid cells for which we have climate data,
    # since we only want to estimate yield in the grid cells where we have the growing season
    # info. This will also speed up the code.
    ir_params_filtered <- dplyr::left_join(irinput_grid, ir_params, by = c("lat", "lon")) %>%
      dplyr::select(-"orig_lon")

    rf_params_filtered <- dplyr::left_join(rfinput_grid, rf_params, by = c("lat", "lon")) %>%
      dplyr::select(-"orig_lon")

    # Test the inputs
    # rf_inputs %>%
    #   dplyr::select(lon, lat, crop, irr) %>%
    #   dplyr::distinct() %>%
    #   dplyr::mutate(deltaT=0,
    #                 deltaP=1,
    #                 N=200,
    #                 C=360,
    #                 year = 1985) -> # year shouldn't matter
    #   test_inputs1
    #
    # test_inputs <- test_inputs1[3:4,]
    #
    # test_yields <- eval_yield(inputs=test_inputs, params=rf_params, matched_grids = rf_matched_grids)


    ir_yields <- eval_yield(inputs=ir_inputs, params=ir_params_filtered, matched_grids = ir_matched_grids, irrig=T) %>%
      dplyr::mutate(gcm = esm_name,
                    cropmodel = cm_name) %>%
      dplyr::rename(lon = crop_lon,
                    lat = crop_lat)

    rf_yields <- eval_yield(inputs=rf_inputs, params=rf_params_filtered, matched_grids = rf_matched_grids, irrig=F) %>%
      dplyr::mutate(gcm = esm_name,
                    cropmodel = cm_name) %>%
      dplyr::rename(lon = crop_lon,
                    lat = crop_lat)

    # Replace negative yields with zero
    ir_yields$yield[ir_yields$yield < 0] <- 0
    rf_yields$yield[rf_yields$yield < 0] <- 0

    # Save gridded yield data (as well as deltaT and delatP) by crop and irr
    ir_yields  %>%
      utils::write.csv(., paste0(gridded_yield_dir, '/', cm_name, "_", esm_name, '_', scn_name,'_', tolower(crop), '_irr_gridded_yield_deltaT_deltaP.csv'),
                       row.names = F)

    rf_yields  %>%
      utils::write.csv(., paste0(gridded_yield_dir, '/', cm_name, "_", esm_name, '_', scn_name,'_', tolower(crop), '_rfd_gridded_yield_deltaT_deltaP.csv'),
                       row.names = F)
  }


  # So now we have a data frame of yields for each grid cell in each year, which
  # we load from file. This helps to deal with the spring and winter wheat aggregation.
  # We deal with wheat separately first. Then the rest of the crops are done in a loop.

  if (any(grepl("Wheat", crops))) {

    crop <- "Wheat"
    rlang::inform(paste0("Generating basin yield for ", crop))

    griddedlist <- list.files(path=gridded_yield_dir, pattern = paste0(esm_name, "_", scn_name), full.names=TRUE, recursive=FALSE)

    # Read in spring and winter wheat gridded yield outputs
    ir_yield_swheat <- utils::read.csv(griddedlist[grepl('spring wheat', griddedlist) & grepl('irr', griddedlist)], stringsAsFactors = F) %>%
      dplyr::select(-c(deltaT, deltaP)) %>%
      dplyr::rename(swheat_yield = yield) %>%
      dplyr::mutate(crop = "Wheat")
    rf_yield_swheat <- utils::read.csv(griddedlist[grepl('spring wheat', griddedlist) & grepl('rfd', griddedlist)], stringsAsFactors = F)%>%
      dplyr::select(-c(deltaT, deltaP)) %>%
      dplyr::rename(swheat_yield = yield) %>%
      dplyr::mutate(crop = "Wheat")

    ir_yield_wwheat <- utils::read.csv(griddedlist[grepl('winter wheat', griddedlist) & grepl('irr', griddedlist)], stringsAsFactors = F)%>%
      dplyr::select(-c(deltaT, deltaP)) %>%
      dplyr::rename(wwheat_yield = yield) %>%
      dplyr::mutate(crop = "Wheat")
    rf_yield_wwheat <- utils::read.csv(griddedlist[grepl('winter wheat', griddedlist) & grepl('rfd', griddedlist)], stringsAsFactors = F)%>%
      dplyr::select(-c(deltaT, deltaP)) %>%
      dplyr::rename(wwheat_yield = yield) %>%
      dplyr::mutate(crop = "Wheat")


    # Function for adding rows with NA (we want to keep NA if summing two NA, otherwise
    # summing two NA will yield 0)
    sum_na <- function(x) {
      if(all(is.na(x))) NA else sum(x, na.rm = TRUE)
    }

    ir_yield_wheat <- dplyr::full_join(ir_yield_swheat,
                                       ir_yield_wwheat,
                                       by = c("crop", "irr", "lon", "lat", "year", "gcm", "cropmodel")) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(yield = sum_na(c(swheat_yield, wwheat_yield)))


    rf_yield_wheat <- dplyr::full_join(rf_yield_swheat,
                                       rf_yield_wwheat,
                                       by = c("crop", "irr", "lon", "lat", "year", "gcm", "cropmodel")) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(yield = sum_na(c(swheat_yield, wwheat_yield)))


    # Check that ir and rf summed correctly
    if(all.equal(sum(ir_yield_swheat$swheat_yield, ir_yield_wwheat$wwheat_yield, na.rm = TRUE), sum(ir_yield_wheat$yield, na.rm = TRUE))) {
      rlang::inform("Sum of individual irrigated spring and winter wheat yields correctly sum to total irrigated wheat yield.")
    } else{rlang::inform(paste0("Warning: Sum of individual irrigated spring and winter wheat yields do not sum to total irrigated wheat yield. The difference is ", sum(ir_yield_swheat$swheat_yield, ir_yield_wwheat$wwheat_yield, na.rm = TRUE) - sum(ir_yield_wheat$yield, na.rm = TRUE)))}

    if(all.equal(sum(rf_yield_swheat$swheat_yield, rf_yield_wwheat$wwheat_yield, na.rm = TRUE), sum(rf_yield_wheat$yield, na.rm = TRUE))) {
      rlang::inform("Sum of individual rainfed spring and winter wheat yields correctly sum to total rainfed wheat yield")
    } else{rlang::inform(paste0("Warning: Sum of individual rainfed spring and winter wheat yields do not sum to total rainfed wheat yield. The difference is ", sum(rf_yield_swheat$swheat_yield, rf_yield_wwheat$wwheat_yield, na.rm = TRUE) - sum(rf_yield_wheat$yield, na.rm = TRUE)))}


    # Generate basin yield
    ir_yields_basin  <- aggregate_halfdeg_yield2basin(ir_yield_wheat %>%
                                                        dplyr::select(-c(swheat_yield, wwheat_yield)))
    rf_yields_basin  <- aggregate_halfdeg_yield2basin(rf_yield_wheat %>%
                                                        dplyr::select(-c(swheat_yield, wwheat_yield)))

    dplyr::bind_rows(rf_yields_basin,
                     ir_yields_basin) ->
      yield.basin

    years <- min(yield.basin$year):max(yield.basin$year)

    # note that only 231 basins have data
    # we need to create new rows in the data frame for missing basins
    # (otherwise gcammapdata::plot_GCAM() will fail)

    tibble::tibble(id = which(basinIDs$GCAM_basin_ID %in% yield.basin$id == F)) %>%
      dplyr::mutate(year = years[1]) %>%
      tidyr::complete(id, year = years) %>%
      dplyr::mutate(yield = 0,
                    HA = 0,
                    cropmodel = cm_name,
                    gcm = esm_name,
                    rcp = scn_name,
                    crop = crop) ->
      tmp

    dplyr::bind_rows(tmp %>% dplyr::mutate(irr = 'IRR'),
                     tmp %>% dplyr::mutate(irr = 'RFD')) ->
      mb
    rm(tmp)

    # bind the missing basins
    dplyr::bind_rows(yield.basin, mb) %>%
      dplyr::ungroup() ->
      yieldByBasin


    utils::write.csv(yieldByBasin, paste0(write_dir, "/", cm_name, "_",
                                          esm_name, "_", scn_name, "_", tolower(crop), "_",
                                          min(years), "_", max(years),".csv"),
                     row.names = F)

  }


# Run basin yield aggregation for remaining crops (not including wheat)
  for(crop in crops[!grepl("Wheat", crops)]){

    rlang::inform(paste0("Generating basin yield for ", crop))

# Read in gridded yield outputs by crop
    ir_yields <- utils::read.csv(griddedlist[grepl(tolower(crop), griddedlist) & grepl('irr', griddedlist)], stringsAsFactors = F) %>%
      dplyr::select(-c(deltaT, deltaP))
    rf_yields <- utils::read.csv(griddedlist[grepl(tolower(crop), griddedlist) & grepl('rfd', griddedlist)], stringsAsFactors = F)%>%
      dplyr::select(-c(deltaT, deltaP))


    ir_yields_basin  <- aggregate_halfdeg_yield2basin(ir_yields)
    rf_yields_basin  <- aggregate_halfdeg_yield2basin(rf_yields)

    dplyr::bind_rows(rf_yields_basin,
                     ir_yields_basin) ->
      yield.basin

    years <- min(yield.basin$year):max(yield.basin$year)

    # note that only 231 basins have data
    # we need to create new rows in the data frame for missing basins
    # (otherwise gcammapdata::plot_GCAM() will fail)

    tibble::tibble(id = which(basinIDs$GCAM_basin_ID %in% yield.basin$id == F)) %>%
      dplyr::mutate(year = years[1]) %>%
      tidyr::complete(id, year = years) %>%
      dplyr::mutate(yield = 0,
                    HA = 0,
                    cropmodel = cm_name,
                    gcm = esm_name,
                    rcp = scn_name,
                    crop = crop) ->
      tmp

    dplyr::bind_rows(tmp %>% dplyr::mutate(irr = 'IRR'),
                     tmp %>% dplyr::mutate(irr = 'RFD')) ->
      mb
    rm(tmp)

    # bind the missing basins
    dplyr::bind_rows(yield.basin, mb) %>%
      dplyr::ungroup() ->
      yieldByBasin


    utils::write.csv(yieldByBasin, paste0(write_dir, "/", cm_name, "_",
                                          esm_name, "_", scn_name, "_", tolower(crop), "_",
                                          min(years), "_", max(years),".csv"),
                     row.names = F)

  }
  rlang::inform("grid_to_basin_yield complete.")
}
