
# Dependent packages:
library(here)
library(stringr)
library(lubridate)
library(tools)
library(arrow)


#' Get the main SharePoint directory path
#'
#' A function to create a string of the path to the chosen office's main SharePoint directory
#'
#' @param office The office of the SharePoint path you want. This currently set up for OPDR's DASP and DP Teams. To be added email zzETA-DASP@dol.gov
#' @examples
#' DASP_path <- get_main_SP_directory(office = "DASP")
#' DP_path <- get_main_SP_directory(office = "DP")
#' @export
get_main_SP_directory <- function(office){
  user <- Sys.info()[["user"]]
  main_dir <- paste0("C:/Users/", user, "/US Department of Labor - DOL/")
  if (office == "DASP") {
    sp_dir <- paste0(main_dir, "T-ETA-OPDR-DASP - Documents/Data Analytics/")
    return(sp_dir)
  } else if (office == "DP") {
    sp_dir <- paste0(main_dir, "T-ETA-OPDR-Data Team - Documents/")
    return(sp_dir)
  } else {
    cat("\nThe office needs to be specified using the office argument for this function \ne.g., get_main_SP_directory(office = DASP).
    Current options include: DASP or DP. \nIf your office is not included please contact zzETA-DASP@dol.gov to get your office location added.\n\n")
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
    cat("\nThe file was copied to SharePoint.")
  } else {
      date_x <- file.info(x)$mtime
      date_sp <- file.info(sp)$mtime
      x_newname <- paste0(tools::file_path_sans_ext(sp_created_file), "_", lubridate::year(file.info(x)$mtime), lubridate::month(file.info(x)$mtime), lubridate::day(file.info(x)$mtime), ".", tools::file_ext(x))
      sp_newname <- paste0(tools::file_path_sans_ext(sp_archive_file), "_", lubridate::year(file.info(sp)$mtime), lubridate::month(file.info(sp)$mtime), lubridate::day(file.info(sp)$mtime), ".", tools::file_ext(sp))

      if(date_x > date_sp){
        if (!dir.exists(sp_archive)){
          dir.create(sp_archive, recursive = TRUE)
        }
        file.copy(sp, sp_newname, overwrite = TRUE)
        cat("\nThere is an old file at the SharePoint location. That file was moved to the /archive subdirectory.")
        file.copy(x, sp, overwrite = TRUE)
        cat("\nThe file was copied to SharePoint.")
      } else{
        if (!dir.exists(sp_created)){
          dir.create(sp_created, recursive = TRUE)
        }
        cat("\nThe file at the SharePoint location is a more recent file. As a result, the file was not copied to the /created subdirectory.")
        file.copy(x, x_newname, overwrite = TRUE)
      }
  }
}



copy_dir_to_SP <- function(dir_path, office, project_location = "mirror"){

  x <- paste0(dir_path, "/")
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
  for (file in list_of_files) {
    file.copy(from = paste0(x, file),
              to = paste0(new_folder, file),
              overwrite = TRUE)
  }

  cat("The directory was copied to SharePoint.")
}




copy_from_SP <- function(file_path, office, project_location = "mirror") {
  x <- file_path

  if (project_location == "mirror"){
    project_path <- stringr::str_extract(x, "Projects.*")
  } else if(project_location != "mirror") {
    select_file <- basename(x)
    project_path <- paste0("Projects/", project_location, "/", select_file)
  }

  sp_dir <- get_main_SP_directory(office)
  sp <- paste0(sp_dir, project_path)

  x_short <- dirname(x)
  x_archive <- paste0(x_short, "/archive/")
  x_archive_file <- paste0(x_archive, basename(x))

  if (!dir.exists(x_short)){
    dir.create(x_short, recursive = TRUE)
    cat("\nThe directory you are copying from in SharePoint does not exist locally so it was created.\n
        This is unlikely to occur to verify that your project is set up correctly.\n")
  }
  if(!file.exists(sp)) {
    cat("\nThis operation did not complete because a file does not exist at the SharePoint location.
        \nThis could be because this is a new script and the file has not been created yet.
        \nIf you beleive there should be a shared file at this location please verify before continuing.
        \n")
  } else {
    if(!file.exists(x)){
      file.copy(sp, x, overwrite = TRUE)
      cat("\nThe file was copied to your local project.")
    } else {
      date_x <- file.info(x)$mtime
      date_sp <- file.info(sp)$mtime
      x_newname <- paste0(tools::file_path_sans_ext(x_archive_file), "_", lubridate::year(file.info(x)$mtime), lubridate::month(file.info(x)$mtime), lubridate::day(file.info(x)$mtime), ".", tools::file_ext(x))

      if(date_x < date_sp){
        if (!dir.exists(x_archive)){
          dir.create(x_archive, recursive = TRUE)
        }
        file.copy(x, x_newname, overwrite = TRUE)
        cat("\nThere is an old file in your local directory. That file was moved to the /archive subdirectory.")
        file.copy(sp, x, overwrite = TRUE)
        cat("\nThe newer file from SharePoint was copied to your local directory.")
      }
    }
  }

}




copy_source_wipr_data <- function(program_years, office, destination_dir = here::here("data", "wipr"), source_type = "clean", py_quarter = 4) {

  output_dir <- destination_dir
  if (!dir.exists(output_dir)){
    dir.create(output_dir, recursive = TRUE)
  }

  file_quarter <- paste0("Q", py_quarter)

  if (source_type == "clean"){
    filename_type <- "_SPRA_RAW_CSV.zip"
    dir_type <- "SPRA/RAW_CSV/"
  } else if (source_type == "raw") {
    filename_type <- "_WIPS_RAW_CSV.zip"
    dir_type <- "WIPS/RAW_CSV/"
  }

  for (program_year in program_years) {
    wipr_filename <- paste0("PY", program_year, file_quarter, filename_type)
    file.copy(from = paste0("S:/PRO/WIOA Performance/WIOA Quarterly Data Files/", dir_type,  wipr_filename),
              to = output_dir)

    cat("\nCopied file for PY", program_year )
    #Unzip the file so a csv can be read. When reading in the file from zip sometimes the full dataset is not being read in due to its size.
    py_file_path <- paste0(output_dir, "/", wipr_filename)
    unzip(py_file_path, exdir = output_dir)
    cat("\nUnzipped file for PY", program_year )

    # Read in the csv file using data.table
    wipr_filename <- paste0("PY", program_year, "Q4_SPRA_RAW.csv")
    py_file_csv <- paste0(output_dir, "/", wipr_filename)
    wipr <- data.table::fread(py_file_csv) |>
      as_tibble()

    export_parquet_file <- paste0(output_dir, "/PY", program_year, ".parquet")
    write_parquet(wipr, export_parquet_file)
    cat("\nSaved parquet file for PY", program_year )

    file.remove(from = py_file_path)
    file.remove(from = py_file_csv)
    #Note: sometimes there is an error in removing the file because a connection is still open. Try again later or manually remove
    cat("\nRemoved old files for PY", program_year)

    copy_to_SP(export_parquet_file, office = office)
  }
}


