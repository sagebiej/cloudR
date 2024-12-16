#' Create and Share Folder in Nextcloud
#'
#' This function creates a new folder and shares it publicly using the Nextcloud WebDAV and OCS APIs.
#'
#' @param webdav_url URL to the WebDAV endpoint.
#' @param folder_name The name of the folder to create.
#' @param username User's username.
#' @param base_ocs_url Base URL for the OCS API.
#' @return A message indicating success or failure.
#' @export
create_and_share_folder <- function(webdav_url, folder_name, username, base_ocs_url) {

  password <- getPass::getPass("Enter your password:")

  # Construct the full URL for the new folder
  new_folder_path <- paste0(webdav_url, "/", folder_name)

  # Send the MKCOL request to create the folder
  response_mkcol <- httr::VERB("MKCOL", url = new_folder_path, httr::authenticate(username, password, type = "basic"))
  status_mkcol <- httr::status_code(response_mkcol)

  # Check the response for folder creation
  if (status_mkcol == 201) {
    print("Folder created successfully.")
  } else if (status_mkcol == 405) {
    print("Folder already exists.")
  } else {
    print(paste("Failed to create folder. Status code:", status_mkcol))
    return(invisible(FALSE))  # Stop if folder creation fails
  }

  # Create a share if the folder is created or already exists
  # Set up the API request for creating a share
  response_share <- httr::POST(
    url = paste0(base_ocs_url, "/ocs/v2.php/apps/files_sharing/api/v1/shares"),
    httr::authenticate(username, password, type = "basic"),
    body = list(
      path = paste0("/", folder_name),  # Path must be relative to the root of the user's files
      shareType = 3,  # Share type 3 for public link
      permissions = 1  # Read permission
    ),
    encode = "form",
    httr::add_headers('OCS-APIRequest' = 'true')
  )

  # Check the response for share creation
  if (httr::status_code(response_share) == 200) {
    share_content <- httr::content(response_share, "parsed")
    share_url <- share_content$ocs$data$url
    cat("Public share URL:", share_url, "\n")
    return(invisible(TRUE))
  } else {
    print(paste("Failed to create share link. Status code:", httr::status_code(response_share)))
    return(invisible(FALSE))
  }



}
