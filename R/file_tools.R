
# Dependent packages:
library(here)
library(stringr)
library(lubridate)
library(tools)



# This function creates the SharePoint office path
get_main_SP_directory <- function(office){
  user <- Sys.info()[["user"]]
  main_dir <- paste0("C:/Users/", user, "/US Department of Labor - DOL/")
  if (office == "DASP") {
    sp_dir <- paste0(main_dir, "T-ETA-OPDR-DASP - Documents/Data Analytics/")
    return(sp_dir)
  } else if (office == "DP") {
    sp_dir <- paste0(main_dir, "T-ETA-ODPR-Data Team - Documents/")
    return(sp_dir)
  } else {
    cat("The office needs to be specified using the office argument for this function \ne.g., get_main_SP_directory(office = DASP) \nCurrent options include: DASP or DP. \nIf your office is not included please contact reuss.kevin.l@dol.gov to get your office location added.")
  }
}



# This function copies a file from the users local project to the related SharePoint location
# The only argument is the file path of the original
copy_to_SP <- function(file_path, office, project_location = "mirror"){
  x <- file_path

  if (project_location == "mirror"){
    project_path <- stringr::str_extract(x, "Projects.*")
  } else if(project_location != "mirror") {
    select_file <- basename(x)
    project_path <- paste0("Projects/", project_location, "/", select_file)
  }

  sp_dir <- get_main_SP_directory(office)
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
      date_x <- file.info(x)$mtime
      date_sp <- file.info(sp)$mtime
      x_newname <- paste0(tools::file_path_sans_ext(sp_created_file), "_", year(file.info(x)$mtime), month(file.info(x)$mtime), day(file.info(x)$mtime), ".", file_ext(x))
      sp_newname <- paste0(tools::file_path_sans_ext(sp_archive_file), "_", year(file.info(sp)$mtime), month(file.info(sp)$mtime), day(file.info(sp)$mtime), ".", file_ext(sp))

      if(date_x > date_sp){
        if (!dir.exists(sp_archive)){
          dir.create(sp_archive, recursive = TRUE)
        }
        file.copy(sp, sp_newname, overwrite = TRUE)
        cat("There is an old file at the SharePoint location. That file was movied to the /archive subdirectory.")
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


copy_dir_to_SP <- function(dir_path, office, project_location = "mirror"){
  x <- dir_path
  sp_dir <- get_main_SP_directory(office)

  if (project_location == "mirror"){
    project_path <- str_extract(x, "Projects.*")
  } else {
    project_path <- paste0("Projects/", project_location, "/")
  }

  new_folder <- paste0(sp_dir, "/", project_path)
  if (!dir.exists(new_folder)){
    dir.create(new_folder, recursive = TRUE)
  }

  list_of_files <- list.files(x)
  file.copy(from = paste0(x, list_of_files),
            to = paste0(new_folder, list_of_files),
            overwrite = TRUE)

  cat("The directory was copied to SharePoint.")
}


