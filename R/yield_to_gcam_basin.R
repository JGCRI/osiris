#' yield_to_gcam_basin
#'
#' Function that uses emulated yield data aggregated to GCAM basin level to create
#' GCAM region, basin, and irrigation level data for all GCAM commodities
#'
#' @param write_dir Default = "outputs_yield_to_gcam_basin". Output Folder
#' @param emulated_basin_yield_dir Default = NULL
#' @param iso_GCAM_region_mapping Default = NULL
#' @param FAO_ag_mapping Default = NULL
#' @param iso_harvest_area_mapping Default = NULL
#' @param iso_GCAM_basin_mapping Default = NULL
#' @param esm_name Default = 'WRF'
#' @param scn_name Default = 'rcp8p5_hot'
#' @param max_CCImult Default = 2.5 Upper limit on positive climate impacts (multiplier)
#' @param min_CCImult Default = 0.01 Lower limit on negative climate impacts (multiplier)
#' @param weight_floor_ha Default = 1 Floor on area weights, in hectares. Below this climate impacts will be ignored. These are more likely than others to be problematic. 1 hectare = 0.01 km^2  = 1e-5 thou km^2, GCAM land units.
#' @param rolling_avg_years Default = 15 Set the number of years to define the range for rolling averages (range = 2 times this plus 1)
#' @param maxHistYear Default = 2010 Historical year for which to apply rolling averages
#' @param minFutYear Default = 2015 Min future year for which to apply rolling averages
#' @param maxFutYear Default = 2100 Max future year for which to apply rolling averages
#' @param extrapolate_to Default = NULL. Note that this extrapolates to only one future year (eg, 2099 to 2100)
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

yield_to_gcam_basin <- function(write_dir = "outputs_yield_to_gcam_basin",
                                emulated_basin_yield_dir = NULL,
                                iso_GCAM_region_mapping = NULL,
                                FAO_ag_mapping = NULL,
                                iso_harvest_area_mapping = NULL,
                                iso_GCAM_basin_mapping = NULL,
                                esm_name = "WRF",
                                scn_name = "rcp8p5_hot",
                                max_CCImult = 2.5,
                                min_CCImult = 0.01,
                                weight_floor_ha = 1,
                                rolling_avg_years = 15,
                                maxHistYear = 2010,
                                minFutYear = 2015,
                                maxFutYear = 2100,
                                extrapolate_to = NULL) {



  #.........................
  # Initialize
  #.........................

  rlang::inform("Starting yield_to_gcam_basin...")

  # Check write dir
  if(!dir.exists(write_dir)){dir.create(write_dir)}

  # Initialize values
  NULL -> AgMIPAbbrev -> C3avg_incl -> GCAM_commodity -> GCAM_region_ID -> GLU ->
    GTAP_crop -> SAGE_crop -> UNIQUE_JOIN_FIELD -> base -> cm_crop -> cm_cropID ->
    ctry_iso -> epic_crop -> gepic_crop -> glu_code -> image_crop -> impact ->
    iso -> lpjguess_crop -> lpjml_crop -> maximp -> maxyr -> median -> outlier ->
    pdssat_crop -> pegasus_crop -> rcp -> weight -> GCAM_basin_ID -> GLU_code ->
    GLU_name -> HA -> ID -> ISO -> crop -> cropmodel -> gcm -> id -> irr ->
    value -> year -> yield -> .


  #.........................
  # Custom functions
  #.........................

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

  repeat_add_columns <- function(x, y) {
    x %>%
      dplyr::mutate(UNIQUE_JOIN_FIELD = 1) %>%
      dplyr::full_join(dplyr::mutate(y, UNIQUE_JOIN_FIELD = 1), by = "UNIQUE_JOIN_FIELD") %>%
      dplyr::select(-UNIQUE_JOIN_FIELD)
  }

  # Identify outliers.
  # See: Davies, P.L. and Gather, U. (1993), "The identification of multiple outliers"
  # (with discussion), J. Amer. Statist. Assoc., 88, 782-801.
  is_outlier <- function(x, devs = 3.2) {
    x <- stats::na.omit(x)
    lims <- stats::median(x) + c(-1, 1) * devs * stats::mad(x, constant = 1)
    # x < lims[1] | x > lims[2]
    x > lims[2]
  } # is_outlier

  # Remove outliers
  remove_outlier <- function( DF, col, devs = 3.2 ) {

    FINAL_DF <- subset( DF, !is_outlier( DF[[col]]))

    return( FINAL_DF )
  }

  #.........................
  # Read in Mapping Data Files
  #.........................

  iso_GCAM_regID <- tibble::as_tibble(utils::read.csv(file=iso_GCAM_region_mapping,head = TRUE, comment.char = "#", sep = ",") )
  FAO_ag_items_PRODSTAT <- tibble::as_tibble(utils::read.csv(file=FAO_ag_mapping,head = TRUE, sep = ",") )
  L100.LDS_ag_HA_ha <- tibble::as_tibble(utils::read.csv(file=iso_harvest_area_mapping,head = TRUE, sep = ",", comment.char = "#") )
  iso_GCAM_basinID <- tibble::as_tibble(utils::read.csv(file=iso_GCAM_basin_mapping,head = TRUE, sep = ",") )

  # correct abbreviations
  FAO_ag_items_PRODSTAT %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::select(GTAP_crop, GCAM_commodity, lpjml_crop, epic_crop, gepic_crop, image_crop, lpjguess_crop, pdssat_crop, pegasus_crop) ->
    crops_gtap_gcam_allCMs

  FAO_ag_items_PRODSTAT %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::select(AgMIPAbbrev, C3avg_incl) %>%
    dplyr::rename(cropname = AgMIPAbbrev)->
    crops_masterlist_c3avgincl

  # switch factors to characters for remaining inputs
  iso_GCAM_regID %>%
    dplyr::mutate_if(is.factor, as.character) ->
    iso_GCAM_regID

  L100.LDS_ag_HA_ha %>%
    dplyr::mutate_if(is.factor, as.character) ->
    L100.LDS_ag_HA_ha

  iso_GCAM_basinID %>%
    dplyr::mutate_if(is.factor, as.character) ->
    iso_GCAM_basinID

  # reading emulated basin yield files in a loop
  emufiles.list <- list.files(path=emulated_basin_yield_dir, pattern=paste0(esm_name, "_", scn_name), full.names=TRUE, recursive=FALSE)
  # emufiles.list <- sub( ".csv", "", emufiles.list )
  emufiles <- list()

  for( i in emufiles.list){ #[c(20, 37, 173, 255, 400, 593,672,731,788)]
    index <- which( emufiles.list == i )
    emufiles[[ index ]] <- utils::read.csv(file=paste0( i),head = TRUE, sep = ",")
  }
  #names( emufiles ) <- emufiles.list
  emu_data1 <- do.call(rbind, emufiles)
  tibble::as_tibble(emu_data1) %>%
    dplyr::mutate_if(is.factor, as.character) ->
    emu_data1



  #.........................
  # Extrapolate
  #.........................

  # Example of extrapolating from 2099 to 2100:

  if (!is.null(extrapolate_to)) {
    rlang::inform(paste0("Extrapolating basin yield data to ", extrapolate_to))

    # Change "maize" to "corn"
    emu_data1$crop[emu_data1$crop == 'maize'] <- 'corn'

    # Capitalize first letter of crop column (not all capitalized before)
    emu_data1$crop <- gsub("(?<!\\w)(.)","\\U\\1", emu_data1$crop, perl = TRUE)

    # Store years as numeric
    emu_data1$year <- as.numeric(emu_data1$year)

    # Run extrapolation function based on Local Polynomial Regression Fitting and add future year
    emu_data1 <- emu_data1 %>%
      dplyr::group_by(crop, irr, id) %>%
      dplyr::do(stats::loess( yield ~ year , control = stats::loess.control(surface = "direct"), data = .) %>%
                  stats::predict(., tibble::tibble(year = extrapolate_to)) %>%
                  tibble::tibble(year = extrapolate_to, yield = .)) %>%
      dplyr::bind_rows(emu_data1) %>%
      tidyr::fill(gcm, cropmodel, HA, rcp, .direction = "downup") %>%
      dplyr::select(names(emu_data1)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(crop, irr, id, year) %>%
      dplyr::ungroup()

    # Store years as numeric
    emu_data1$yield <- as.numeric(emu_data1$yield)
  }

  # emu_data1 %>%
  #   dplyr::filter(cropmodel != "lpj-guess") ->
  #   emu_data1

  # emu_data1 %>%
  #   dplyr::filter(rcp == "rcp8p5") ->
  #   emu_data1

  #.........................
  # Perform computations
  #.........................

  rlang::inform("Performing computations")

  emu_data1 %>%
    dplyr::arrange(rcp, gcm, cropmodel, id, crop, irr, year) %>%
    # Drop inland water and any rows where the area weight is less than the exogenous floor
    dplyr::rename(ID = id,
                  weight = HA) %>%
    # dplyr::filter(weight >= weight_floor_ha) %>%
    dplyr::filter( ID != 0) ->
    emu_yield_HA_AllYears_glu_irr_isicrop


  #Create data frames with the appropriate averages
  # For the base years, calculate the average
  # For the future years, calculate rolling averages

  # get base average for each unique ID from historical years
  emu_yield_HA_AllYears_glu_irr_isicrop %>%
    dplyr::filter(year <= (maxHistYear + rolling_avg_years),
                  year >= (maxHistYear - rolling_avg_years)) %>%
    dplyr::group_by(ID, weight,rcp, gcm, cropmodel,crop, irr) %>%
    dplyr::mutate(base = mean(yield)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-year, -yield) %>%
    dplyr::distinct() ->
    emu_yield_HA_base_glu_irr_isicrop


  # rolling average of future years; append column of historical years base average for each unique id
  # Then calculate impacts
  emu_yield_HA_AllYears_glu_irr_isicrop %>%
    dplyr::filter(year > maxHistYear) %>%
    dplyr::group_by(ID, weight,rcp, gcm, cropmodel,crop, irr) %>%
    dplyr::mutate(yield = as.numeric(rollAvg(yield, n= (2*rolling_avg_years+1) ) ) ) %>%
    dplyr::ungroup() %>%
    # append base values
    dplyr::left_join(emu_yield_HA_base_glu_irr_isicrop,
                     by = c("ID", "weight","rcp", "gcm", "cropmodel","crop", "irr")) %>%
    # "dropping irrelevant columns" - mostly done already, just restrict down to every 5 years in future
    dplyr::filter(year %in% seq(minFutYear, maxFutYear, 5)) %>%
    # calculate the multipliers/impacts by doing Value/base for each year
    dplyr::group_by(ID, weight, rcp, gcm, cropmodel,crop, irr) %>%
    dplyr::mutate(impact = yield/base,
                  base = base/base) %>%
    dplyr::ungroup() %>%
    dplyr::select(rcp, gcm, cropmodel, ID, crop, irr, weight, base, year, impact) ->
    emu_impacts_AllYears_base_glu_irr_isicrop

  rm(emu_yield_HA_base_glu_irr_isicrop)
  rm(emu_yield_HA_AllYears_glu_irr_isicrop)


  # #2b. Calculate the C3 average in emulated basin yield and append this to the table with emulated basin yield crops differentiated
  # printlog( "Calculating average C3 crop impacts to be applied to C3 crops not included in the emulated basin yield data set")
  # printlog( "NOTE: Using area-weighted impacts to avoid bias of computing averages from crops with different base yields" )
  # # NOTE: This step works for LPJmL's crop list. not sure how to modify for crop models with different crop lists
  ### just need a master column for the full range of crops

  # #Aggregate crops with weighted average
  emu_impacts_AllYears_base_glu_irr_isicrop %>%
    dplyr::left_join(crops_masterlist_c3avgincl, by = c("crop" = "cropname")) %>%
    dplyr::filter(C3avg_incl == 1) %>%
    dplyr::select(-C3avg_incl) %>%
    # dplyr::filter(crop %in% FAO_ag_items_PRODSTAT$LPJmL_crop[FAO_ag_items_PRODSTAT$C3avg_include == 1]) %>%
    dplyr::group_by(ID, rcp, gcm, cropmodel, irr, year) %>%
    dplyr::summarise(impact = stats::weighted.mean(impact, weight),
                     base = stats::weighted.mean(base, weight)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(crop="C3avg") %>%
    dplyr::bind_rows(emu_impacts_AllYears_base_glu_irr_isicrop,.) %>%
    # add in iso info for each basin so that we can use it to go from basin -> basin + region id
    dplyr::left_join(dplyr::select(iso_GCAM_basinID, GCAM_basin_ID, ISO), by = c("ID" = "GCAM_basin_ID")) %>%
    dplyr::mutate(ISO = tolower(as.character((ISO)))) %>%
    dplyr::rename(iso = ISO) %>%
    # add region id
    dplyr::left_join(dplyr::select(iso_GCAM_regID, iso, GCAM_region_ID), by ="iso")  ->
    emu_impacts_AllYears_base_glu_irr_isicrop_c3avg


  # Join crop model and gcam commodity identifying information to the LDS harvested area set (which is HA by glu and gtap crop)
  L100.LDS_ag_HA_ha$glu_code <- sprintf("GLU%03d",L100.LDS_ag_HA_ha$glu_code) # modify glu_code format
  L100.LDS_ag_HA_ha %>%
    dplyr::rename(HA = value,
                  GTAP_crop = SAGE_crop,
                  GLU = glu_code,
                  iso = ctry_iso) %>%
    dplyr::left_join(crops_gtap_gcam_allCMs, by = "GTAP_crop") %>%
    stats::na.omit() %>%
    dplyr::filter(HA != 0) %>%
    dplyr::mutate(ID = as.numeric(substr(GLU, 4,6))) %>%
    # repeat for all model run info combos
    repeat_add_columns(tibble::tibble(cropmodel = unique(emu_impacts_AllYears_base_glu_irr_isicrop_c3avg$cropmodel))) %>%
    # dplyr::rename(lpjml_crop = LPJmL_crop,
    #        gepic_crop = GEPIC_crop) %>%
    tidyr::gather(cm_cropID, cm_crop, -iso, -GLU, -GTAP_crop, -HA, -ID, -cropmodel) %>% #, -GCAM_commodity) %>%  # may need to kill GCAM commodity here and join separately in next pipeline to avoid errors
    #dplyr::filter(paste0(cropmodel, "_crop") == cm_cropID) %>%
    dplyr::select(-cm_cropID) ->
    LDS_HA_iso_glu_gtap_cm_cmcrop


  # join the HA and use to aggregate to GCAM commodity for each region-glu-irr
  emu_impacts_AllYears_base_glu_irr_isicrop_c3avg %>%
    dplyr::left_join(LDS_HA_iso_glu_gtap_cm_cmcrop, by = c("iso", "ID", "cropmodel", "crop" = "cm_crop")) %>%
    dplyr::select(-weight, -ID, -iso) %>%  # drop mirca harvested area weights from grid processing and other unneeded info
    stats::na.omit() %>%
    dplyr::left_join(dplyr::select(FAO_ag_items_PRODSTAT, GTAP_crop, GCAM_commodity), by = "GTAP_crop") %>%
    dplyr::group_by(rcp, gcm, cropmodel, irr, GCAM_region_ID, GLU, GCAM_commodity, year) %>%
    dplyr::summarise(impact = stats::weighted.mean(impact, HA),
                     base = stats::weighted.mean(base, HA)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(impact = dplyr::if_else(impact > max_CCImult, max_CCImult, impact)) %>%
    dplyr::mutate(impact = dplyr::if_else(impact < min_CCImult, min_CCImult, impact)) ->
    ag_impacts_rcp_gcm_gcm_R_GLU_C_IRR_allyears


  # Remove FodderGrass for consistency across crop models (only epic and lpjguess cover it)
  ag_impacts_rcp_gcm_gcm_R_GLU_C_IRR_allyears %>%
    dplyr::filter(GCAM_commodity != "FodderGrass") ->
    ag_impacts_rcp_gcm_gcm_R_GLU_C_IRR_allyears


  # some labeling for ag
  ag_impacts_rcp_gcm_gcm_R_GLU_C_IRR_allyears %>%
    dplyr::left_join(dplyr::select(iso_GCAM_basinID, GLU_code, GLU_name),
                     by = c("GLU" = "GLU_code")) ->
    ag_impacts_rcp_gcm_gcm_R_GLU_C_IRR_allyears


  # # If not removing outliers
  # ag_impacts_rcp_gcm_gcm_R_GLU_C_IRR_allyears ->
  #   ag_impacts_rcp_gcm_gcm_R_GLU_C_IRR_allyears1

  # if removing outliers
  # identify outliers for ag:
  ag_impacts_rcp_gcm_gcm_R_GLU_C_IRR_allyears %>%
    dplyr::group_by(rcp, gcm, cropmodel, GCAM_commodity, irr) %>%
    dplyr::mutate(outlier = is_outlier(impact)) %>%
    dplyr::ungroup() ->
    ag_tmp

  # Get last non-outlier year and impact for each crop-irr-basin to replace outlier years in the crop-irr-basin
  ag_tmp %>%
    dplyr::filter(outlier == FALSE) %>%
    dplyr::group_by(rcp, gcm, cropmodel, GCAM_region_ID, GLU, GLU_name, GCAM_commodity, irr) %>%
    dplyr::mutate(maxyr = max(year)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == maxyr) %>%
    dplyr::select(-outlier, -year, -base) %>%
    dplyr::rename(maximp = impact) ->
    ag_tmp1

  # Join that to the table of outliers, and replace the outlier years
  ag_tmp %>%
    dplyr::left_join(ag_tmp1, by = c("rcp", "gcm", "cropmodel", "GCAM_region_ID", "GLU", "GLU_name", "GCAM_commodity", "irr"))  %>%
    dplyr::mutate(impact = dplyr::if_else(outlier == TRUE, maximp, impact)) %>%
    dplyr::select(-outlier, -maxyr, -maximp) ->
    ag_impacts_rcp_gcm_gcm_R_GLU_C_IRR_allyears1

  rm(ag_tmp)
  rm(ag_tmp1)


  # #Bioenergy climate change impacts: use the median of the other crops
  # again update to keep irrigation and no AEZ info; also add the GCAM_commodity label here, not done in original
  ag_impacts_rcp_gcm_gcm_R_GLU_C_IRR_allyears1 %>%
    dplyr::ungroup() %>%
    dplyr::select(-GCAM_commodity) %>%
    dplyr::group_by(rcp, gcm, cropmodel, irr, GLU, GLU_name, GCAM_region_ID, year) %>%
    dplyr::summarise_if(is.numeric, stats::median) %>%
    dplyr::mutate(GCAM_commodity = "biomass") ->
    bio_impacts_rcp_gcm_gcm_R_GLU_C_IRR_allyears


  bio_impacts_rcp_gcm_gcm_R_GLU_C_IRR_allyears ->
    bio_impacts_rcp_gcm_gcm_R_GLU_C_IRR_allyears1


  # write
  utils::write.csv(ag_impacts_rcp_gcm_gcm_R_GLU_C_IRR_allyears1, paste0(write_dir, "/ag_impacts_", esm_name, "_", scn_name, "_rcp_gcm_gcm_R_GLU_C_IRR_allyears_RA",2*rolling_avg_years +1, "_gridcull_allyroutlier.csv"), row.names=FALSE)
  utils::write.csv(bio_impacts_rcp_gcm_gcm_R_GLU_C_IRR_allyears1, paste0(write_dir, "/bio_impacts_", esm_name, "_", scn_name, "_rcp_gcm_gcm_R_GLU_C_IRR_allyears_RA",2*rolling_avg_years +1, "_gridcull_allyroutlier.csv"), row.names=FALSE)


  rlang::inform("yield_to_gcam_basin complete.")
}
