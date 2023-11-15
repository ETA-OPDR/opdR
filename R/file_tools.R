
# Dependent packages:
library(here)
library(stringr)
library(lubridate)
library(tools)

# This function copies a file from the users local project to the related SharePoint location
# The only argument is the file path of the original
copy_to_DASP <- function(x){
  user <- Sys.info()[["user"]]
  project_path <- str_extract(x, "Projects.*")
  sp_dir <- paste0("C:/Users/", user, "/US Department of Labor - DOL/T-ETA-OPDR-DASP - Data Analytics/")
  sp <- paste0(sp_dir, project_path)
  sp_short <- dirname(sp)
  sp_archive <- paste0(sp_short, "/archive/")
  sp_created <- paste0(sp_short, "/created/")
  sp_archive_file <- paste0(sp_archive, basename(sp))
  sp_created_file <- paste0(sp_created, basename(sp))

  if (!dir.exists(sp_short)){
    dir.create(sp_short, recursive = TRUE)
  }

  if(!file.exists(sp)){
    file.copy(x, sp, overwrite = TRUE)
    cat("The file was copied to SharePoint.")
  } else {
      date_x <- file.info(x)$atime
      date_sp <- file.info(sp)$atime
      x_newname <- paste0(file_path_sans_ext(sp_created_file), "_", year(file.info(x)$atime), month(file.info(x)$atime), day(file.info(x)$atime), ".", file_ext(x))
      sp_newname <- paste0(file_path_sans_ext(sp_archive_file), "_", year(file.info(sp)$atime), month(file.info(sp)$atime), day(file.info(sp)$atime), ".", file_ext(sp))

      if(date_x > date_sp){
        if (!dir.exists(sp_archive)){
          dir.create(sp_archive, recursive = TRUE)
        }
        file.copy(sp, sp_newname, overwrite = TRUE)
        cat("There is an old file at the SharePoint loaction. That file was movied to the /archive subdirectory.")
        file.copy(x, sp, overwrite = TRUE)
        cat("The file was copied to SharePoint.")
      } else{
        if (!dir.exists(sp_created)){
          dir.create(sp_created, recursive = TRUE)
        }
        cat("The file at the SharePoint location is a more recent file. As a result, the file was not copied to the /created subdirectory.")
        file.copy(x, x_newname, overwrite = TRUE)
      }
  }
}



