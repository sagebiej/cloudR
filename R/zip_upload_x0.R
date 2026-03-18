#' Zip a Folder and Upload to a File Hosting Service
#'
#' @description
#' This function zips all files in a specified folder (including subfolders),
#' uploads the zip file to a specified file hosting service (default: x0.at),
#' and returns a shareable link.
#'
#' @param folder Character. Path to the folder to be zipped and uploaded.
#' @param zip_file Character. Path (including filename) where the zip file should be saved.
#' @param upload_link Character. URL of the file hosting service (default: "https://x0.at/").
#' @return Character. Shareable link to the uploaded zip file.
#' @examples
#' upload_x0("path/to/folder", "path/to/output.zip")
#' upload_x0("path/to/folder", "path/to/output.zip", "https://custom-upload-service.com/")
#' @export
zip_upload_x0 <- function(folder, zip_file, upload_link = "https://x0.at/") {
  # Check if folder exists
  if (!dir.exists(folder)) {
    stop("Error: The specified folder does not exist.")
  }

  # Get all files in the folder (including subfolders)
  files <- list.files(folder, recursive = TRUE, full.names = FALSE)

  # Check if there are files to zip
  if (length(files) == 0) {
    stop("Error: No files found in the specified folder.")
  }

  # Add the folder path to each file
  file_paths <- file.path(folder, files)

  # Zip the files, excluding the top-level folder in the zip
  zip(zip_file, file_paths)

  # Check if zip file was created
  if (!file.exists(zip_file)) {
    stop("Error: Failed to create the zip file.")
  }

  # Upload the zip file and get the shareable link
  link <- system(paste0("curl -F 'file=@", zip_file, "' ", upload_link), intern = TRUE)

  # Check if upload was successful
  if (grepl("Error|error|not found|failed", link, ignore.case = TRUE)) {
    stop(paste("Error: Upload failed. Service response:", link))
  }

  # Return the shareable link
  return(link)
}
