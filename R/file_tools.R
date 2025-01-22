
# Dependent packages:

#' @import here
#' @import stringr
#' @import lubridate
#' @import tools
#' @import arrow
#'



#' Get the main SharePoint directory path
#'
#' A function to create a string of the path to the chosen office's main SharePoint directory
#'
#' @param office The office of the SharePoint path you want. This currently set up for OPDR's DASP and DP Teams. To be added email zzETA-DASP@dol.gov
#'
#' @examples
#' DASP_path <- get_main_SP_directory(office = "DASP")
#' DP_path <- get_main_SP_directory(office = "DP")
#'
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



#' Copy a local file in a local project to the team project folder on SharePoint
#'
#' This function copies a file from the users local project to the related SharePoint location
#'
#' @param file_path The local file path of the file you want to copy to SharePoint. This is typically the file you just wrote to a local directory.
#' @param office The office of the SharePoint path you want. This currently set up for OPDR's DASP and DP Teams. To be added email zzETA-DASP@dol.gov
#' @param project_location The default is "mirror" which means the file will be copied to the same location in the SharePoint directory as the local directory. If you want to copy the file to a different location in the SharePoint directory you can specify the location here.
#' @examples
#'
#' write_path <- here::here("data",  "PY2021.csv")
#' write_csv(df, write_path)
#' copy_to_SP(write_path, office = "DASP")
#'
#' @export
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


#' Copy the whole directory in a local project to the team project folder on SharePoint
#'
#' This function copies a directory from the users local project to the related SharePoint location
#'
#' @param dir_path The local directory path of the directory you want to copy to SharePoint.
#' @param office The office of the SharePoint path you want. This currently set up for OPDR's DASP and DP Teams. To be added email zzETA-DASP@dol.gov
#' @param project_location The default is "mirror" which means the directory will be copied to the same location in the SharePoint directory as the local directory. If you want to copy the directory to a different location in the SharePoint directory you can specify the location here.
#' @examples
#'
#' dir <- here::here("data")
#' copy_dir_to_SP(dir, office = "DASP")
#'
#' @export
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



#' Copy a local file in a team SharePoint project to user's local project
#'
#' This function copies a file from the team project directory on SharePoint to the related project directory on the user's local machine.
#'
#' @param file_path The team SharePoint project path of the file you want to copy to your local. This is typically the file you are about to read in.
#' @param office The office of the SharePoint path you want. This currently set up for OPDR's DASP and DP Teams. To be added email zzETA-DASP@dol.gov
#' @param project_location The default is "mirror" which means the file will be copied from the same location in the SharePoint directory as the local directory. If you want to copy the file from a different location in the SharePoint directory you can specify the location here.
#' @examples
#'
#' read_path <- here::here("data",  "PY2021.csv")
#' copy_from_SP(read_path, office = "DASP")
#' df <- read_csv(read_path)
#'
#' @export
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



#' Copy a WIPR quarterly data file from the S drive to different location
#'
#' This function copies a WIPR quarterly data file from the S drive to a local directory, unzips the file, reads in the csv file, saves the file as a parquet file, and then copies the parquet file to the related SharePoint location.
#'
#' @param program_years The WIOA program year files you want. This can be a single year or a vector of years.
#' @param destination_dir The path of the directory you want to copy the files to. The default is the data/wipr directory in the project.
#' @param source_type The type of WIPR file you want to copy. The default is "clean" which is the SPRA file. The other option is "raw" which is the WIPS file.
#' @param py_quarter The quarter of the program year file you want. The default is 4 because it is the file you want to use for annual data.
#' @examples
#'
#' copy_source_wipr_data(program_years = 2021)
#' copy_source_wipr_data(program_years = c(2020, 2021), source_type = "raw", py_quarter = 2)
#'
#' @export
copy_source_wipr_data <- function(program_years, destination_dir = here::here("data", "wipr"), source_type = "clean", py_quarter = 4) {

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

    # copy_to_SP(export_parquet_file, office = office)
  }
}


