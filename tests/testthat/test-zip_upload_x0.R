library(testthat)


  # Setup: Create a temporary folder and files
  temp_dir <- tempdir()
  test_folder <- file.path(temp_dir, "test_folder")
  dir.create(test_folder)
  writeLines("test content", file.path(test_folder, "file1.txt"))
  writeLines("test content 2", file.path(test_folder, "file2.txt"))

  # Define output zip file
  zip_file <- file.path(temp_dir, "test.zip")

  # Test 1: Successful zip and upload (real upload to x0.at)
  test_that("zip_upload_x0 returns a valid link for real upload", {
    # Suppress warnings (e.g., curl warnings) for cleaner output
    link <- suppressWarnings(zip_upload_x0(test_folder, zip_file))

    # Verify the link is a valid URL
    expect_true(grepl("^https://x0.at/.*.zip", link))
    # Verify the zip file was created
    expect_true(file.exists(zip_file))

    # Cleanup: Remove the zip file
    unlink(zip_file, recursive = TRUE)
  })

  # Test 2: Error for non-existent folder
  test_that("zip_upload_x0 throws error for non-existent folder", {
    expect_error(
      zip_upload_x0("non_existent_folder", file.path(temp_dir, "test.zip")),
      "Error: The specified folder does not exist."
    )
  })

  # Test 3: Error for empty folder
  test_that("zip_upload_x0 throws error for empty folder", {
    empty_folder <- file.path(temp_dir, "empty_folder")
    dir.create(empty_folder)
    expect_error(
      zip_upload_x0(empty_folder, file.path(temp_dir, "test.zip")),
      "Error: No files found in the specified folder."
    )
  })

  # Test 4: Custom upload link (real upload)
  test_that("zip_upload_x0 uses custom upload link for real upload", {
    zip_file <- file.path(temp_dir, "test_custom.zip")
    # Use a custom upload service (replace with a real or test service)
    custom_upload_link <- "https://x0.at/"  # Replace if testing another service
    link <- suppressWarnings(
      zip_upload_x0(test_folder, zip_file, custom_upload_link)
    )

    # Verify the link is a valid URL
    expect_true(grepl("^https?://x0.at/.*.zip", link))
    # Verify the zip file was created
    expect_true(file.exists(zip_file))

    # Cleanup: Remove the zip file
    unlink(zip_file, recursive = TRUE)
  })

