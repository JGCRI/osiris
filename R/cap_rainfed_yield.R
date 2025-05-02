#' cap_rainfed_yield
#'
#' Function that takes the defined impacts and reference AgProdChange files then
#' updates and saves the new AgProdChange csv and xml files.
#'
#' @param write_dir Default = "step5_cap_rainfed_yield". Output Folder
#' @param input_dir Default = NULL. Input Folder (step 4)
#' @param GCAM_basin_mapping Default = NULL
#' @param GCAM_region_mapping Default = NULL
#' @param agyield_path_input Default = NULL
#' @param bioyield_path_input Default = NULL
#' @keywords test
#' @return number
#' @importFrom rlang :=
#' @importFrom magrittr %>%
#' @importFrom foreach %do%
#' @export
#' @examples
#' \dontrun{
#' library(osiris)
#' osiris::cap_rainfed_yield()
#' }

cap_rainfed_yield <- function(write_dir = "step5_cap_rainfed_yield",
                              input_dir = NULL,
                              GCAM_basin_mapping = NULL,
                              GCAM_region_mapping = NULL,
                              agyield_path_input = NULL,
                              bioyield_path_input = NULL) {



  #.........................
  # Initialize
  #.........................

  rlang::inform("Starting cap_rainfed_yield...")

  # Check write dir
  if(!dir.exists(write_dir)){dir.create(write_dir)}

  # Initialize values
  NULL -> AgProdChange -> AgProductionTechnology -> GCAM_region_ID ->
    GLU -> GLU_name -> GLU_code -> region -> year -> Irr_Rfd -> yield ->
    GCAM_commodity -> level -> value -> AgSupplySubsector -> AgSupplySector ->
    AgSupplySubsector -> do -> Irrigation -> agyield.rfd ->
    agyield.irr -> tech -> value.rfd -> value.irr -> lag ->.


  # Identify input APC CSV files
  apc_files <- list.files(path = input_dir, pattern = "^ag_prodchange_.*\\.csv$", full.names = TRUE)

  # Helper function to read APC files
  df_from_csv <- function(filepath) {
    headers <- utils::read.csv(filepath, skip = 4, header = FALSE, nrows = 1, as.is = TRUE)
    df <- utils::read.csv(filepath, skip = 5, header = FALSE)
    colnames(df) <- headers
    return(df)
  }

  # Read all APC data
  apc_data_list <- lapply(apc_files, function(f) list(name = basename(f), data = df_from_csv(f)))

  # Read in mapping file
  mapping_file <- utils::read.csv(GCAM_basin_mapping) %>%
    dplyr::select(GLU_code, GLU_name)

  # Read in agyield file
  agyield_headers <- utils::read.csv(agyield_path_input, skip = 2, header = FALSE, nrows = 1, as.is = TRUE)
  agyield <- utils::read.csv(agyield_path_input, skip = 3, header = FALSE)
  colnames(agyield) <- agyield_headers
  agyield <- dplyr::filter(agyield, year == 2015) %>%
    dplyr::mutate(Irr_Rfd = toupper(Irr_Rfd))

  # Read in bioyield file
  bioyield_headers <- utils::read.csv(bioyield_path_input, skip = 4, header = FALSE, nrows = 1, as.is = TRUE)
  bioyield <- utils::read.csv(bioyield_path_input, skip = 5, header = FALSE)
  colnames(bioyield) <- bioyield_headers

  # Read in GCAM region names
  GCAM_regions_headers <- utils::read.csv(GCAM_region_mapping, skip = 5, header = FALSE, nrows = 1, as.is = TRUE)
  GCAM_regions <- utils::read.csv(GCAM_region_mapping, skip = 6, header = FALSE)
  colnames(GCAM_regions) <- GCAM_regions_headers

  # Prepare bioyield
  df_bioyield <- bioyield %>%
    dplyr::left_join(GCAM_regions, by = "region") %>%
    tidyr::separate(AgProductionTechnology, into = c("GCAM_commodity", "GLU_name", "Irr_Rfd", "level"), sep = "_") %>%
    dplyr::left_join(mapping_file, by = "GLU_name") %>%
    dplyr::rename(GLU = GLU_code, value = yield) %>%
    dplyr::select(GCAM_region_ID, GCAM_commodity, GLU, Irr_Rfd, year, level, value) %>%
    dplyr::filter(year == 2015)

  ag_bio_yield <- dplyr::bind_rows(agyield, df_bioyield) %>%
    dplyr::rename("GLU_code" = "GLU")

  mapping_yield <- dplyr::left_join(mapping_file, ag_bio_yield, by = "GLU_code") %>%
    tidyr::unite(AgSupplySubsector, c("GCAM_commodity", "GLU_name")) %>%
    dplyr::mutate(AgProductionTechnology = paste(AgSupplySubsector, Irr_Rfd, level, sep = "_")) %>%
    dplyr::left_join(GCAM_regions, by = "GCAM_region_ID") %>%
    dplyr::select(GCAM_region_ID, AgProductionTechnology, value)

  # Process each APC scenario
  process_apc <- function(apc_entry) {
    apc_name <- tools::file_path_sans_ext(apc_entry$name)
    apc_df <- apc_entry$data %>%
      dplyr::left_join(GCAM_regions, by = "region") %>%
      dplyr::left_join(mapping_yield, by = c("GCAM_region_ID", "AgProductionTechnology")) %>%
      dplyr::select(-GCAM_region_ID)

    apc_df[,"agyield"] <- NA

    apc_df <- dplyr::group_by(apc_df, region, AgSupplySector, AgSupplySubsector, AgProductionTechnology) %>%
      do(dplyr::add_row(., year = 2015) %>%
           tidyr::fill(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, value) %>%
           dplyr::mutate(agyield = replace(agyield, year == 2015, unique(value))) %>%
           dplyr::arrange(year) %>%
           dplyr::mutate(agyield = purrr::accumulate(.[["AgProdChange"]][-1], .init = agyield[1], ~ .x * (1 + .y)^5))) %>%
      dplyr::ungroup() %>%
      tidyr::drop_na() %>%
      tidyr::separate(AgProductionTechnology, into = c("x", "y", "Irrigation", "tech"), sep = "_") %>%
      dplyr::select(-c("x", "y"))

    # Split into rainfed and irrigated
    rfd <- dplyr::filter(apc_df, grepl("RFD", Irrigation))
    irr <- dplyr::filter(apc_df, grepl("IRR", Irrigation))

    capped <- dplyr::left_join(rfd, irr,
                               by = c("AgSupplySubsector", "region", "AgSupplySector", "tech", "year"),
                               suffix = c(".rfd", ".irr")) %>%
      dplyr::mutate(agyield.rfd = pmin(agyield.rfd, agyield.irr))

    capped <- dplyr::full_join(rfd, irr,
                               by = c("AgSupplySubsector", "region", "AgSupplySector", "tech", "year"),
                               suffix = c(".rfd", ".irr")) %>%
      dplyr::mutate(agyield.rfd = pmin(agyield.rfd, agyield.irr, na.rm = TRUE))


    capped_long <- capped %>%
      dplyr::select(AgSupplySubsector, region, AgSupplySector, tech, year,
                    agyield.rfd, agyield.irr, value.rfd, value.irr) %>%
      tidyr::pivot_longer(cols = c(agyield.rfd, agyield.irr, value.rfd, value.irr),
                          names_to = c(".value", "Irrigation"),
                          names_sep = "\\.") %>%
      dplyr::mutate(Irrigation = toupper(Irrigation))

    capped_long[,"AgProdChange"] <- NA

    final_df <- capped_long %>%
      dplyr::mutate(AgProductionTechnology = paste(AgSupplySubsector, Irrigation, tech, sep = "_")) %>%
      dplyr::group_by(region, AgSupplySector, AgSupplySubsector, Irrigation, tech) %>%
      do(dplyr::add_row(., year = 2015) %>%
           tidyr::fill(region, AgSupplySector, AgSupplySubsector, Irrigation, value) %>%
           dplyr::mutate(agyield = replace(agyield, year == 2015, unique(value))) %>%
           dplyr::arrange(year) %>%
           dplyr::mutate(AgProdChange = ((agyield / lag(agyield))^(1/5)) - 1)) %>%
      dplyr::ungroup() %>%
      tidyr::drop_na() %>%
      dplyr::select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year, AgProdChange)


    # Write updated CSV with matching name
    output_csv <- file.path(write_dir, paste0(apc_name, ".csv"))
    write.table_with_header(final_df, output_csv,
                            paste(c("INPUT_TABLE,,,,,",  "Variable ID,,,,,", "AgProdChange,,,,,", ",,,,,")),
                            sep = ",", quote = FALSE, row.names = FALSE)

    # Write XML
    output_xml <- file.path(write_dir, paste0(apc_name, ".xml"))
    gcamdata::create_xml(output_xml) %>%
      gcamdata::add_xml_data(final_df, "AgProdChange") %>%
      gcamdata::run_xml_conversion()
  }

  # Helper to write CSV with header
  write.table_with_header <- function(x, file, header, ...) {
    cat(header, sep = '\n', file = file)
    utils::write.table(x, file, append = TRUE, ...)
  }

  # Process all scenarios
  lapply(apc_data_list, process_apc)


  rlang::inform("cap_rainfed_yield complete.")
}
