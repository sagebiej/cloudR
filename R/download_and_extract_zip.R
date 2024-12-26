#' Downloads and extracts external data to be used in the simulation
#'
#' This function downloads a ZIP file from the specified URL and extracts its contents
#' into the designated folder. It is designed to handle data setup for simulations
#' by ensuring that the downloaded data is stored correctly.
#'
#' @param url The URL from where the external data can be downloaded.
#'            The external data should be in a ZIP file.
#' @param dest_folder The folder where the data should be stored. Defaults to the current working directory.
#' @param zip_name Optional; specifies the name of the ZIP file if it differs from the last part of the URL.
#' @param overwrite set to true if you want to overwrite the existing folder. Be careful, the folder will be deleted first and then the content will be downloaded and extracted. Default is FALSE
#' @return The path to the folder where the ZIP file was extracted, invisibly.
#' @export
#' @examples
#' \dontrun{
#' download_and_extract_zip(url = "http://www.example.com/datafile.zip",
#'                          dest_folder = "path/to/destination")
#' }
download_and_extract_zip <- function(url, dest_folder = ".", zip_name = NULL, overwrite = FALSE) {
  # If zip_name is not provided, extract it from the URL
  if (is.null(zip_name)) {
    zip_name <- basename(url)
  }

  # Construct the full path for the ZIP file
  zip_path <- file.path(dest_folder, zip_name)

  # Check if the folder where the data is to be stored exists
  if (!dir.exists(dest_folder)) {
    dir.create(dest_folder, recursive = TRUE, showWarnings = TRUE)
  } else {
    # Check if overwrite is true; if so, delete existing files
    if (overwrite) {
      # Remove all files and directories in the destination folder
      files <- list.files(dest_folder, full.names = TRUE)
      if (length(files) > 0) {
        file.remove(files)
      }
    } else if (length(list.files(dest_folder)) > 0) {
      warning("Destination folder is not empty. Nothing copied.")
      return(invisible(NULL))
    }
  }

  # Download the zip file
  utils::download.file(url, zip_path, method = "auto", quiet = FALSE, mode = "wb", cacheOK = TRUE)

  # Extract the contents
  zip::unzip(zip_path, exdir = dirname(dest_folder))

  # Clean up by deleting the downloaded ZIP file
  file.remove(zip_path)

  # Return the path to the extracted folder, invisibly
  return(invisible(file.path(dest_folder, tools::file_path_sans_ext(zip_name))))
}
