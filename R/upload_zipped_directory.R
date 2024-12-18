#' Upload a Zipped Directory to Nextcloud
#'
#' This function zips a local directory and uploads the resulting ZIP file to a specified
#' location in Nextcloud using WebDAV.
#'
#' @param local_folder_path The path to the local directory to be zipped.
#' @param zip_file_path Path where the ZIP file should be saved locally.
#' @param nextcloud_base_url The base URL of the Nextcloud instance.
#' @param target_nextcloud_folder The target folder in Nextcloud where the ZIP file will be uploaded.
#' @param username Nextcloud username.
#' @return A message indicating the outcome of the upload operation.
#' @export
#' @examples \dontrun{
#' upload_zipped_directory("path/to/local/folder", "path/to/zipfile.zip",
#'                                      "https://nextcloud.instance.com", "/path/in/nextcloud/",
#'                                      "username") }
upload_zipped_directory <- function(local_folder_path, zip_file_path, nextcloud_base_url, target_nextcloud_folder, username) {

  password <- getPass::getPass("Enter your password:")
  # Create the ZIP file

  cat("\n Starting to zip the file \n")
  zip::zip(zipfile = zip_file_path, files = local_folder_path, recurse = TRUE)

  cat("\n Zipping completed \n")

  # Full URL for the uploaded ZIP file on Nextcloud
  target_file_path <- paste0(nextcloud_base_url, target_nextcloud_folder, basename(zip_file_path))



  # Prepare the ZIP file for upload
  file_content <- httr::upload_file(zip_file_path, type = "application/zip")

  cat("\n Starting to upload zipped file \n")

  # Perform the PUT request
  response <- httr::PUT(url = target_file_path, body = file_content, httr::authenticate(username, password, type = "basic"))

  # Check the response and return a message
  if (httr::status_code(response) == 201) {
    cat("ZIP file upload successful:", target_file_path, "\n")
  } else {
    cat("ZIP file upload failed with status code:", httr::status_code(response), "\n")
  }
}
