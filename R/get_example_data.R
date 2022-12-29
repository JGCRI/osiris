#' get_example_data
#'
#' Function to download the data files from zenodo that are needed to run Osiris.
#'
#' @param write_dir Default = getwd()
#' @param data_link Default = "https://zenodo.org/record/7474112/files/Osiris_Data.zip?download=1"
#' @keywords test
#' @return number
#' @importFrom rlang :=
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#' library(osiris)
#' osiris::get_example_data()
#' }

get_example_data <- function(write_dir = getwd(),
                             data_link = "https://zenodo.org/record/7474112/files/Osiris_Data.zip?download=1") {


  #.........................
  # Initialize
  #.........................

  rlang::inform("Starting get_example_data")

  # Download Osiris_Data.zip
  utils::download.file(url = data_link,
                       destfile = paste0(write_dir, "/Osiris_Data.zip"),
                       mode = "wb")

  utils::unzip(paste0(write_dir, "/Osiris_Data.zip"),
               exdir = "Osiris_Data")

  unlink(paste0(write_dir, "/Osiris_Data.zip"))

  rlang::inform(paste0("You can set data_folder = \"", write_dir, "/Osiris_Data\" to run the example scripts from https://jgcri.github.io/osiris/articles/vignette.html"))


  #.........................
  # Close Out
  #.........................

  rlang::inform("get_example_data completed.")
}
