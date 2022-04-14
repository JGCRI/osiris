#' grid_to_basin_yield
#'
#' Function that uses emulators to create gridded yield files then aggregates to
#' gcam basin level via MIRCA harvested areas.
#'
#' @param carbon Default = NULL
#' @param weight_floor_ha Default = 1
#' @param fcnpath Default = NULL
#' @param inputpath Default = NULL
#' @param areapath Default = NULL
#' @param basin_grid Default = NULL
#' @param basin_id Default = NULL
#' @param outpath Default = getwd(). Output Folder
#' @param crops Default = c("maize", "rice", 'soy', 'wheat')
#' @param esm_name Default = 'CanESM5'
#' @param cm_name Default = 'LPJmL'
#' @param scn_name Default = 'rcp8p5'
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
                                fcnpath = NULL,
                                inputpath = NULL,
                                areapath = NULL,
                                basin_grid = NULL,
                                basin_id = NULL,
                                outpath = getwd(),
                                crops = c("maize", "rice", 'soy', 'wheat'),
                                esm_name = 'CanESM5',
                                cm_name = 'LPJmL',
                                scn_name = 'rcp8p5',
                                N = 200) {



  #.........................
  # Initialize
  #.........................

  rlang::inform("Starting grid_to_basin_yield...")

  # Initialize values
  NULL -> year -> value -> vtag -> units -> ctag -> type -> Scenario ->
    model -> ID -> long -> lati -> x -> y -> countryID -> protected -> flowD ->
    elevation -> elevD -> eleD_min -> eleD_max -> regID -> inGrandELEC ->
    basinID -> GCAM_basin_ID -> gcammaptools_name -> GCAM_basin_name ->
    Basin_name -> GLU_code -> GLU_name -> ISO -> ISO_NUM -> Country_name ->
    lon -> lat -> latgrid -> longrid -> crop -> irr -> deltaT -> deltaP ->
    crop_lon -> crop_lat -> C -> yield -> gcm -> cropmodel -> HA -> param_sum ->
    orig_lon -> coarse_lon -> coarse_lat -> id -> .


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
    croplist <- c("crop01" = "wheat",
                  "crop02" = "maize",
                  "crop03" = "rice",
                  "crop08" = "soy",
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


  ## TODO: double check that the inputs are formed correctly
  ##       for what the paper Franke et al needs.
  # This is not fast, it's evaluating a polynomial at every grid
  # point in every year
  eval_yield <- function(inputs, params, matched_grids){

    # Because my climate data (inputs) Is on a coarser grid than
    # my crop data (params), I have to do an apply over the crop
    # rows and join in the climate data that corresponds.
    ## TODO you may need to do the split on climate data instead of crop data and
    ##      reverse their roles in the below code
    params %>%
      dplyr::mutate(split_id = paste0(lon, '~', lat)) ->
      params
    params_list <- split(params, f=params$split_id)

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
                      ## TODO: double check this with eqn 1 in franke et al
                      yield = param_row$`1`+
                        param_row$`2`*C +
                        param_row$`3`*deltaT +
                        param_row$`4`*deltaP +
                        param_row$`5`*N +
                        param_row$`6`*C*C +
                        param_row$`7`*C*deltaT +
                        param_row$`8`*C*deltaP +
                        param_row$`9`*C*N +
                        param_row$`10`*deltaT*deltaT +
                        param_row$`11`*deltaT*deltaP +
                        param_row$`12`*deltaT*N +
                        param_row$`13`*deltaP*deltaP +
                        param_row$`14`*deltaP*N +
                        param_row$`15`*N*N +
                        param_row$`16`*C*C*C +
                        param_row$`17`*C*C*deltaT +
                        param_row$`18`*C*C*deltaP +
                        param_row$`19`*C*C*N +
                        param_row$`20`*C*deltaT*deltaT +
                        param_row$`21`*C*deltaT*deltaP +
                        param_row$`22`*C*deltaT*N +
                        param_row$`23`*C*deltaP*deltaP +
                        param_row$`24`*C*deltaP*N +
                        param_row$`25`*C*N*N +
                        param_row$`26`*deltaT*deltaT*deltaT +
                        param_row$`27`*deltaT*deltaT*deltaP +
                        param_row$`28`*deltaT*deltaT*N +
                        param_row$`29`*deltaT*deltaP*deltaP +
                        param_row$`30`*deltaT*deltaP*N +
                        param_row$`31`*deltaT*N*N +
                        param_row$`32`*deltaP*deltaP*deltaP +
                        param_row$`33`*deltaP*deltaP*N +
                        param_row$`34`*deltaP*N*N
        ) ->
        yield
      return(yield)
    }))
    row.names(yields) <- NULL

    return(yields %>%
             dplyr::select(crop, irr, crop_lon, crop_lat, year, yield))
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

    if(unique(halfdeg_yield$crop) == 'Corn'){
      area_crop_name <- 'maize'
    }else{
      area_crop_name <- tolower(unique(halfdeg_yield$crop))
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
      # aggregate to basin:
      dplyr::group_by(gcm, cropmodel, crop, irr, basinID, year) %>%
      dplyr::summarise(yield = stats::weighted.mean(yield, w = HA),
                       HA = sum(HA)) %>%
      dplyr::ungroup() %>%
      # calculate basin level yield
      dplyr::rename(id = basinID) %>%
      dplyr::mutate(yield = dplyr::if_else(HA == 0, 0, yield),
                    rcp = scn_name) %>%
      tidyr::replace_na(list(yield=0)) %>%
      stats::na.omit() ->
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

  # fcnpath sourced from https://zenodo.org/record/3592453#.Ygqzhu7MJ7M
  # too large for github.
  ## TODO download for self
  fcnlist <- list.files(path=fcnpath, full.names=TRUE, recursive=FALSE)
  fcnlist <- fcnlist[grepl('A0', fcnlist)] # only keep non-adaptation scenarios

  inputlist <- list.files(path=inputpath, full.names=TRUE, recursive=FALSE)

  areafilelist <- list.files(path=areapath, full.names=TRUE, recursive=FALSE)
  # there's only 4 crops in the ggcmi phase2 emulators. Subset the area
  # files to these to save on time and memory:
  areafilelist <- areafilelist[grepl("crop01", areafilelist) |
                                 grepl("crop02", areafilelist) |
                                 grepl("crop03", areafilelist) |
                                 grepl("crop08", areafilelist)]

  # CO2
  # TODO update as you need
  carbon <- utils::read.csv(paste0(carbon), stringsAsFactors = F) #cmip5 co2

  #.........................
  # Maps
  #.........................

  tibble::as_tibble(utils::read.csv(paste0(basin_grid), stringsAsFactors = F)) %>%
    dplyr::select(long, lati, basinID) %>%
    dplyr::filter(basinID != 999) ->
    basinGrid

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

  for(crop in crops[1:1]){

    rlang::inform(paste0("Generating basin yield for ", crop))

    # yield emu netcdf
    ncfname <- fcnlist[grepl(crop, fcnlist) & grepl(cm_name, fcnlist)]

    # Wheat is weird
    # TODO:
    if(crop == 'wheat' | crop == 'Wheat'){
      # open up both winter and spring wheat response functions:

      # apply a unifying mask so you just have one df for each of
      # irr, rfd wheat params:

    }else{
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

    }
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
    if(crop == 'maize'){
      rf_tp <- utils::read.csv(inputlist[grepl('corn', inputlist) & grepl('rfd', inputlist)], stringsAsFactors = F)
      ir_tp <- utils::read.csv(inputlist[grepl('corn', inputlist) & grepl('irr', inputlist)], stringsAsFactors = F)
    }else{
      rf_tp <- utils::read.csv(inputlist[grepl(crop, inputlist) & grepl('rfd', inputlist)], stringsAsFactors = F)
      ir_tp <- utils::read.csv(inputlist[grepl(crop, inputlist) & grepl('irr', inputlist)], stringsAsFactors = F)
    }


    # Combine C, N, T and P data with ir and rfd params to get ir, rf yields in each
    # grid cell in each year

    # get the grid of the climate inputs
    rf_tp %>%
      dplyr::select(lon, lat) %>%
      dplyr::distinct() %>%
      dplyr::mutate(orig_lon = lon,
                    lon = dplyr::if_else(lon > 179.75, lon -360, lon)) ->
      rfinput_grid

    ir_tp %>%
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
    ir_matched_grids <- grid_matching(finer_grid = ircrop_grid,
                                      coarser_grid = irinput_grid %>%
                                        dplyr::select(-orig_lon)) %>%
      dplyr::rename(crop_lon = lon, crop_lat = lat,
                    clim_lon = coarse_lon, clim_lat = coarse_lat)

    rf_matched_grids <- grid_matching(finer_grid = rfcrop_grid,
                                      coarser_grid = rfinput_grid %>%
                                        dplyr::select(-orig_lon))%>%
      dplyr::rename(crop_lon = lon, crop_lat = lat,
                    clim_lon = coarse_lon, clim_lat = coarse_lat)


    # Ok now we have all the info we need for different grids.
    # Make the main tables of inputs for ir, rf
    ir_tp %>%
      dplyr::mutate(deltaP = (deltaP-1)) %>%
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
      dplyr::mutate(deltaP = (deltaP-1)) %>%
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


    # Test the inputs
    rf_inputs %>%
      dplyr::select(lon, lat, crop, irr) %>%
      dplyr::distinct() %>%
      dplyr::mutate(deltaT=0,
                    deltaP=1,
                    N=200,
                    C=360,
                    year = 1985) -> # year shouldn't matter
      test_inputs1

    test_inputs <- test_inputs1[3:4,]

    test_yields <- eval_yield(inputs=test_inputs, params=rf_params, matched_grids = rf_matched_grids)


    ir_yields <- eval_yield(inputs=ir_inputs, params=ir_params, matched_grids = ir_matched_grids) %>%
      dplyr::mutate(gcm = esm_name,
                    cropmodel = cm_name) %>%
      dplyr::rename(lon = crop_lon,
                    lat = crop_lat)

    rf_yields <- eval_yield(inputs=rf_inputs, params=rf_params, matched_grids = rf_matched_grids) %>%
      dplyr::mutate(gcm = esm_name,
                    cropmodel = cm_name) %>%
      dplyr::rename(lon = crop_lon,
                    lat = crop_lat)



    # So now we have a data frame of yields for each grid cell in each year
    # for the grid cells from the finest mesh.
    # In this case, that's the crop grid cells.

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
      dplyr::ungroup() %>%
      dplyr::filter(year > 2005) ->
      yieldByBasin


    utils::write.csv(yieldByBasin, paste0(outpath, "/", cm_name, "_",
                                          esm_name, "_", scn_name, "_", crop, "_",
                                          min(years), "_", max(years),".csv"),
                     row.names = F)

  }

  rlang::inform("grid_to_basin_yield complete.")
}