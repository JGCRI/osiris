#' get_example_data
#'
#' Function to download the test data files from zenodo that are needed to run Osiris.
#'
#' @param write_dir Default = getwd()
#' @param dir_name = "Osiris_Data_Test". Name of directory to install zip file to.
#' @param data_link Default = "https://zenodo.org/record/7530067/files/Osiris_Data_Test.zip?download=1"
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
                             dir_name = "Osiris_Data_Test",
                             data_link = "https://zenodo.org/record/7530067/files/Osiris_Data_Test.zip?download=1") {


  #.........................
  # Initialize
  #.........................

  rlang::inform("Starting get_example_data")

  # Check if sample data already exists in current directory
  if (!dir.exists(paths = paste0(getwd(), "/Osiris_Data_Test"))) {
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

    rlang::inform(paste0("The example data was installed to: ", getwd(), "/Osiris_Data_Test
You can set data_folder = \"", write_dir, "/", dir_name, "\" to run the example scripts from https://jgcri.github.io/osiris/articles/vignette.html"))
  } else {
    rlang::inform(paste0("Example data folder already exists.
If you would like to reinstall the data, delete the ", getwd(), "/Osiris_Data_Test folder and run this function again."))
  }

  #.........................
  # Close Out
  #.........................

  rlang::inform("get_example_data completed.")

  return(paste0(write_dir, "/", dir_name))
}
