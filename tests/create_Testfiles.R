rm(list = ls())
devtools::load_all()





test_dir <- file.path(tempdir(), "testfolder/binaries/") ## folder to be uploaded
dir.create(test_dir,recursive = TRUE)                    ## create the folder
file.create(file.path(test_dir, "dummyfile.txt"))        ## put files into the folder
file.create(file.path(test_dir, "dummyfile2.RDS"))


folder_name <- "uploads/cloudR_testfiles/"              ## folder on nextcloud
nextcloud_base_url <- "https://cloud.idiv.de/"          ## nextcloud URL
webdav_url <- paste0(nextcloud_base_url, "/remote.php/dav/files/dj44vuri")   ## URL extention for WEBDAV
username <- "dj44vuri"                                  ## username


## create the folder on nextcloud and create a public link. If folder already exists, only the public link will be created
cloudR::create_and_share_folder(webdav_url = webdav_url, folder_name = folder_name, username = username,  base_ocs_url = nextcloud_base_url )

# save working directors
wd <- getwd()

## set the working directory to the parent folder of the folder that should be zipped- This is important to avoid uncessecesary paths but in most cases not required
setwd(dirname(test_dir))

## now zip the folder and upload it to nextcloud (password requred )
cloudR::upload_zipped_directory(local_folder_path = "binaries/", "binaries.zip", "https://cloud.idiv.de/remote.php/dav/files/dj44vuri/", folder_name, username )

setwd(wd)
getwd()
