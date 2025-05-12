

#' Connect to WIPS
#'
#' This function creates a connection to the WIPS redshift data and stores that connection to the object name assigned.
#'
#' @param wips_password The users WIPS password.
#' @examples
#'
#' wips_user_pasword <- "mypassword"
#' con <- connect_to_WIPS(wips_password = wips_user_password)
#'
#' @import RPostgres
#'
#' @export
connect_to_WIPS <- function(wips_password){

  user_dir <- dirname("~")
  user_name <-  paste0(tolower(basename(user_dir)),"@dol.gov")
  con <- dbConnect(RPostgres::Postgres(),
                   host = "biw-prod.723295778562.us-east-1.redshift-serverless.amazonaws.com",
                   port = "5439",
                   user = user_name,
                   password = wips_password,
                   dbname = "biw",
                   sslmode='require')
  return(con)
}




#' Identify Name of WIPR table
#'
#' This function creates a string for the WIPR table you want. It is needed because WIPS uses a different naming convention than the program year.
#'
#' @param program_year The program year of the reported data file table you want.
#' @param quarter The program year quarter of the reported data file you want.
#' @examples
#'
#' select_py <- 2023
#' select_quarter <- 4
#' table_name <- wipr_table_name(program_year = select_py, quarter = select_quarter)
#'
#'
#' @export
wipr_table_name <- function(program_year, quarter){

  year <- program_year+1
  pyq <- paste0("PY", program_year, "Q", quarter)

  if (quarter == 4) {
    qtr <- paste0(year, "-06-30")
  }
  if (quarter == 3) {
    qtr <- paste0(year, "-03-31")
  }
  if (quarter == 2) {
    qtr <- paste0(program_year, "-12-31")
  }
  if (quarter == 1) {
    qtr <- paste0(program_year, "-09-30")
  }

  table_name <- paste0("wips_warehouse_py", program_year-2015,"q",quarter)
  return(table_name)
}






#' Change the Column Names of the Columns from WIPS Warehouse Names to PIRL Names
#'
#' This function will automatically change the column names to the standard PIRL names (i.e., pxxx) from the names used in the warehouse.
#' Right now the function just works for one type of name, but additonal options can be added in the future.
#'
#' @param data The data pulled from the WIPS redshift warehouse
#' @param wips_con The WIPS Redshift connection object created to access the tables
#' @param select_columns By default this is NA because all columns are selected. This can be changed to a list of columns if you only want to keep certain columns.
#' @param version The default is the 2021 version of the PIRL, but older versions are available.
#' @param header The default is "pirl" which filters the table to select the names that match the standard PIRL format
#' @param schema The defaul is "pirl" which filters the table to select the names that match the standard PIRL format
#' @examples
#'
#'
#' data <- data %>% change_wips_columns(., wips_con = con)
#'
#' @import dplyr
#' @import RPostgres
#'
#' @export
change_wips_columns <- function(data, wips_con, select_columns = NA, version = 2021, header = "pirl", schema = "pirl") {


  if (header == "pirl" & schema == "pirl") {
    header <- 2
    schema <- 28
  }


  print("Column name information is pulled from the wips_export_metadata table in WIPS.")
  print("View the table for other column naming options.")

  pirl_names <- as.data.frame(tbl(wips_con, "wips_export_metadata")) |>
    filter(pirl_version == version &
             header_type == header &
             schema_id == schema)

  if (!is.na(select_columns)) {
    pirl_names <- pirl_names |>
      filter(column_name %in% select_columns)
  }

  current_cols <- colnames(data)

  pirl_names <- pirl_names %>%
    add_row(sequence = 1, warehouse_name = "fileid", column_name = "fileid") %>%
    arrange(sequence) %>%
    mutate(warehouse_name = tolower(warehouse_name)) |>
    filter(warehouse_name %in% current_cols) |>
    mutate(column_name = case_when(
      column_name == "p108 - A" ~ "p108a",
      column_name == "p108 - B" ~ "p108b",
      column_name == "p108 - C" ~ "p108c",
      TRUE ~ column_name
    ))

  warehouse_cols<- pirl_names$warehouse_name

  data <- data %>%
    select(all_of(warehouse_cols)) %>%
    rename_with(~ pirl_names$column_name[match(., pirl_names$warehouse_name)], .cols = everything())

  return(data)
}
