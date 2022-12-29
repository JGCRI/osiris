#' get_example_data
#'
#' Function to download the data files from zenodo that are needed to run Osiris.
#'
#' @param write_dir Default = getwd()
#' @param dir_name = "Osiris_Data". Name of directory to install zip file to.
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
                             dir_name = "Osiris_Data",
                             data_link = "https://zenodo.org/record/7474112/files/Osiris_Data.zip?download=1") {


  #.........................
  # Initialize
  #.........................

  rlang::inform("Starting get_example_data")

  # Download zip file from zenodo
  rlang::inform("Starting download...")

  options(timeout = max(300, getOption("timeout")))

  utils::download.file(url = data_link,
                       destfile = paste0(write_dir, "/", dir_name, ".zip"),
                       mode = "wb")

  rlang::inform("Download complete.")

  # Download zip file from zenodo
  rlang::inform("Starting unzip...")

  utils::unzip(paste0(write_dir, "/", dir_name, ".zip"),
               exdir = paste0(write_dir, "/", dir_name))

  unlink(paste0(write_dir, "/", dir_name, ".zip"))

  rlang::inform("Unzip complete.")

  rlang::inform(paste0("You can set data_folder = \"", write_dir, "/", dir_name, "\" to run the example scripts from https://jgcri.github.io/osiris/articles/vignette.html"))


  #.........................
  # Close Out
  #.........................

  rlang::inform("get_example_data completed.")

  return(paste0(write_dir, "/", dir_name))
}
