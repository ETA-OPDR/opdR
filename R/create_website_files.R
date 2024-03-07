library(tidyverse)
library(openxlsx)
library(here)



# This function creates anonymous files for the DOL website and a spreadsheet with the file/link info.
create_anonymous_files <- function(files_dir, year, product){

  file_dir <- paste0(files_dir, "/", year)
  output_dir <- paste0(file_dir, "/anonymous/")

  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  }

  set.seed(222)
  flist <- list.files(file_dir, full.names = TRUE, recursive = TRUE) |>
    as_tibble() |>
    filter(!str_detect(value, "anonymous")) |>
    mutate(state_file = basename(value)) |>
    # mutate(state_file = str_sub(value, start = -28)) |>
    mutate(state = str_sub(state_file, end = 2)) |>
    mutate(runif = round(runif(value, min=100000, max=999999), 0)) |>
    mutate(newfile = paste0(output_dir, "/", state, "_", runif, ".html")) |>
    mutate(dol_link = paste0(state, "_", runif, ".html"))

  file.copy(flist$value, flist$newfile, overwrite = TRUE)


  if(product == "state_equity") {
    drupal_location <- "DASP/Workforce_Services_Dashboards/"
    select_office <- "DASP"
  } else if(product == "state_assessments") {
    drupal_location <- "DoP/WIOA_Annual_Reports/"
    select_office <- "DP"
  } else {
    cat("No product was identified. \nThe product argument (i.e., product = ) must be indicated. \nThis function currently supports the following products: state_equity, state_assessments \nTo get your product added contact reuss.kevin.l@dol.gov")
  }


  copy_dir_to_SP(dir_path = output_dir, office = select_office)

  file_info <- flist |>
    mutate(program_year = year,
           # value = str_extract(value, "/OneDrive - US Department of Labor - DOL.*"),
           value = str_extract(value, "/Projects.*"),
           # newfile = str_extract(newfile, "/OneDrive - US Department of Labor - DOL.*")
           newfile = str_extract(newfile, "/Projects.*")
           ) |>
    mutate(dol_link = paste0("https://www.dol.gov/sites/dolgov/files/ETA/opder/", drupal_location, year, "/", dol_link),
           anonymous_name = paste0(state, "_", runif)) |>
    rename(original_path = value,
           anonymous_file_path = newfile,
           DOL_website_link = dol_link) |>
    relocate(state, program_year, state_file, original_path, anonymous_name, anonymous_file_path, DOL_website_link) |>
    select(-runif)

  class(file_info$DOL_website_link)<-"hyperlink" # mark as a hyperlink
  wb_path <- here::here("reports", "state_file_info.xlsx")

  sheet_name <- paste0("PY", year, "_files")

  if(file.exists(wb_path)){
    wb <- loadWorkbook(wb_path)
  } else {
    wb <- createWorkbook()
    saveWorkbook(wb, wb_path, overwrite = TRUE)
    wb <- loadWorkbook(wb_path)
  }

  current_sheets <- getSheetNames(wb_path)
  sheet_name <- paste0("PY", year, "_files")

  if (!sheet_name %in% current_sheets) {
    addWorksheet(wb, sheetName = sheet_name)
  }

  writeData(wb, sheet_name, file_info)

  current_sheets <- getSheetNames(wb_path)
  default_name <- "Sheet 1"
  if (default_name %in% current_sheets) {
    removeWorksheet(wb, sheet = 1)
  }

  saveWorkbook(wb, wb_path, overwrite = TRUE)

  copy_to_SP(wb_path, office = select_office)
}




