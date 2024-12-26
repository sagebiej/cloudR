
# Define the download URL
dl_url <- "https://cloud.idiv.de/index.php/s/PZHAWorcKZcafwn/download?path=%2F&files=binaries.zip"

# Setup a temporary directory for testing
test_dir <- file.path(tempdir(),"testfolder")
if (dir.exists(test_dir)) {
  unlink(test_dir ,recursive = TRUE)
}
# Test 1: Downloading and extracting to an empty directory
test_that("Function extracts correctly to an empty directory", {
  # Setup
  temp_dir <- file.path(test_dir, "binaries")
  #dir.create(test_dir)
  dir.create(temp_dir,recursive = TRUE)

  # Test
  download_and_extract_zip(dl_url, temp_dir)
  expect_true(dir.exists(temp_dir), "The temporary directory should exist.")
  expect_true((length(list.files(temp_dir))) > 0, "The 'binaries directory should contain files.")
  ### Test if it does not overwrite
  expect_warning(download_and_extract_zip(dl_url, temp_dir) , "Destination folder is not empty. Nothing copied.")

  # Test 3: Overwrite existing files
  file.create(file.path(temp_dir, "oldfile.txt"))
  expect_true(file.exists(file.path(temp_dir, "oldfile.txt"))) ## check if file has been created successfully
  download_and_extract_zip(dl_url, temp_dir, overwrite = TRUE)
  expect_false(file.exists(file.path(temp_dir, "oldfile.txt")))
  unlink(temp_dir, recursive = TRUE)
  expect_error(download_and_extract_zip("http://thisurldoesnotexist.com/datafile.zip", temp_dir))
  # Cleanup

})



#
# # Test 4: Invalid URL handling
# test_that("Function handles invalid URLs gracefully", {
#   # Setup
#   temp_dir <- file.path(test_dir, "test_invalid_url")
#   dir.create(temp_dir)
#
#   # Test
#
#
#   # Cleanup
#   unlink(temp_dir, recursive = TRUE)
# })
#
# # Run all tests
# test_dir_cleanup <- function() {
#   unlink(test_dir, recursive = TRUE)
# }
# register_tester_exit_callback(test_dir_cleanup)
