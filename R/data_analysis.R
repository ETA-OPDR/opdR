

#' Create WIOA program columns
#'
#' This function creates columns for each WIOA program (Adult, Dislocated Worker, Youth, and Wagner-Peyser) and indicates if the person is in the program based on the values in the p903, p904, p905, and p918 columns.
#'
#' @param data The data frame you want to add the columns to. It should include the columns p903, p904, p905, and p918.
#' @examples
#'
#' df <- create_wioa_program_columns(df)
#'
#' @import dplyr
#'
#' @export
create_wioa_program_columns <- function(data) {
  data <- data |>
    mutate(adult = ifelse(p903 %in% 1:3, 1, NA),
         dw = ifelse(p904 %in% 1:3 | p909 == 1, 1, NA),
         youth = ifelse(p905 %in% 1:3, 1, NA),
         wp = ifelse(p918 == 1, 1, NA))

  cat("\nA column indicating if a participant was in each WIOA program was added.")
  return(data)
}




#' Create a list of WIOA state abbreviations
#'
#' This function creates a vector that lists all the state abbreviations (includes DC and PR). Territories can be included as well.
#'
#' @param include_territories A logical value that indicates if territories should be included in the list. The default is FALSE.
#' @examples
#'
#' states <- list_of_wioa_states()
#' all_eta_areas <- list_of_wioa_states(include_territories = TRUE)
#'
#' @import dplyr
#'
#' @export
list_of_wioa_states <- function(include_territories = FALSE){

  if (include_territories == TRUE) {
    state_list <- state_info$Alpha_code
    return(state_list)
    state_list <- c(state_list, "AS", "GU", "MP", "VI")
  } else {
    state_info_temp <- state_info |>
      filter(Status != "Territory")
    state_list <- state_info_temp$Alpha_code
    return(state_list)
  }

}



#' Format a column with zip codes
#'
#' This function formats zip code columns to have 5 characters. It pads the zip code with zeros if it has less than 5 characters.
#'
#' @param data The data frame that contains the zip code column.
#' @param zip_column The name of the zip code column in the data frame.
#' @examples
#'
#' df <- format_zip_code(df, zip_column = "zip_code")
#'
#' @import dplyr
#' @import stringr
#'
#' @export
format_zip_code <- function (data, zip_column) {

  data <- data |>
    mutate(zip_column = ifelse(str_length(zip_column) < 3, 99999, zip_column)) |>
    mutate(zip_column = str_pad(as.character(zip_column), 5, side = "left", pad = "0"))

  cat("Formatted the zip code column to have 5 characters.")
  return(data)
}




#' Add columns with state identification information
#'
#' This function adds colums with state identification information to a data frame. The function identifies the source column as the State ID, State FIPS code, or State full name. It then adds a column with the State Alpha code, Numeric code, or full name to the data frame.
#'
#' @param data The data frame you want to add the columns to.
#' @param source_column The name of the source column that contains the state identification information.
#' @param state_id_type The type of state identification information in the source column. It can be "id", "name", or "fips".
#' @param column_name The name of the column you want to add to the data frame.
#' @param state_data The data frame that contains the state identification information. The default is the state_info dataframe this is part of the opdR package.
#' @examples
#'
#' df <- df |> add_state_information(source_column = "state", state_id_type = "id", column_name = "state_abbreviation")
#'
#' @import dplyr
#' @import stringr
#'
#' @export
add_state_information <- function(data, source_column, state_id_type, column_name, state_data = state_info) {

  state_data <- state_data |>
    mutate(Numeric_code = str_pad(as.character(Numeric_code), 2, side = "left", pad = "0"))

  if (state_id_type == "id") {
    state_type <- "Alph"
  } else if (state_id_type == "name") {
    state_type <- "Name"
  } else if (state_id_type == "fips") {
    state_type <- "Numeric"
  }

  column_name <- enquo(column_name)
  source_column_temp <- enquo(source_column)
  s_col <- data |>
    select(!!source_column_temp) |>
    rename(temp_col = !!source_column_temp) |>
    mutate(temp_col = str_pad(as.character(temp_col), 2, side = "left", pad = "0"))

  s_col <- s_col[[1,1]]

  if (s_col %in% state_data$Alpha_code) {
    cat("\nIdentified the source column as the State ID.")

    state_data <- state_data |>
      select(Alpha_code, starts_with(state_type))

    colname <- colnames(state_data) |> nth(2)

    data <- data |>
      rename(Alpha_code = !!source_column_temp) |>
      left_join(state_data, by = "Alpha_code") |>
      rename(!!source_column_temp := Alpha_code) |>
      rename(!!column_name := colname)

  } else if (s_col %in% state_data$Numeric_code) {
    cat("\nIdentified the source column as the State FIPS code.")

    state_data <- state_data |>
      select(Numeric_code, starts_with(state_type))

    colname <- colnames(state_data) |> nth(2)

    data <- data |>
      rename(Numeric_code = !!source_column_temp) |>
      mutate(Numeric_code = str_pad(as.character(Numeric_code), 2, side = "left", pad = "0")) |>
      left_join(state_data, by = "Numeric_code") |>
      rename(!!source_column_temp := Numeric_code) |>
      rename(!!column_name := colname)


  } else if (s_col %in% state_data$Name){
    cat("\nIdentified the source column as the State full name.")
    state_data <- state_data |>
      select(Name, starts_with(state_type))

    colname <- colnames(state_data) |> nth(2)

    data <- data |>
      rename(Name = !!source_column_temp) |>
      left_join(state_data, by = "Name") |>
      rename(!!source_column_temp := Name) |>
      rename(!!column_name := colname)

  }

  cat("\nAdded a column with the State", state_type, ", which is aligned to the", source_column, "in the data.")
  return(data)
}






#' Consolidate the reporting state column
#'
#' This function consolidates the reporting state column in the data frame. It can be needed because the column that identifies the reporting state differs by the program year of the data. The function checks the program year and consolidates the reporting state column to a single column. The function drops the p3000 and p4000 columns if they are present.
#'
#' @param data The data frame you want to consolidate the reporting state column in.
#' @param program_year The program year of the data.
#' @examples
#'
#' df <- consolidate_reporting_state_column(df, program_year = 2021)
#'
#' @import dplyr
#'
#' @export
consolidate_reporting_state_column <- function(data, program_year) {
  py <- as.numeric(program_year)

  if (py < 2021) {
    data <- data |>
      mutate(state = p3000)
  } else if (py == 2021) {
    data <- data |>
      mutate(state = p4000)
  } else {
    data <- data
  }

  fncols <- function(data, cname) {
    add <-cname[!cname%in%names(data)]

    if(length(add)!=0) data[add] <- NA
    data
  }

  cnames <- c("p3000", "p4000")

  data <- data %>%
    fncols(cname = cnames) |>
    select(-p3000, -p4000)

  cat("\nThe 'state' column now indicates the reporting state. p3000 and/or p4000 were dropped if present.")

  return(data)

}



#' Add columns with dates relevant to the WIOA outcome types
#'
#' This function adds columns with dates relevant to the WIOA outcome types. The function adds columns with the program year start and end dates, the quarter 2 start and end dates, the quarter 4 start and end dates, and the quarter 4 roll forward start date.
#'
#' @param program_year The program year of the data.
#' @examples
#'
#' df <- add_outcome_type_dates(program_year = 2021)
#'
#' @import dplyr
#' @import lubridate
#'
#' @export
add_outcome_type_dates <- function(program_year) {

  py <- as.numeric(program_year)

  cat("\nAdded outcome type date variables to the environment.")

  py_end <<- ymd(paste0(py + 1, "0630"))
  py_start <<- py_end %m-% years(1) %m+% days(1)
  py_roll4_start <<- py_start %m-% months(9, abbreviate = FALSE)
  q2_end <<- py_end %m-% years(1)
  q2_start <<- py_start %m-% years(1)
  q2_roll4_start <<- py_start %m-% years(1) %m-% months(9, abbreviate = FALSE)
  q4_end <<- ceiling_date((q2_end %m-% months(6, abbreviate = FALSE)),"month") - days(1)
  q4_start <<- q2_start %m-% months(6, abbreviate = FALSE)
  q4_roll4_start <<- q2_start %m-% months(6, abbreviate = FALSE) %m-% months(9, abbreviate = FALSE)


}


#' Generate WIOA outcomes
#'
#' This function generates WIOA outcomes based on the data provided. The function creates columns that indicate if the participant received a credential, entered employment, retained employment, and received a measurable skill gain. For MST, the function can be used to generate outcomes for all participants in the data or only for participants that are included per the official rules used to calculate the MSG performance indicator in WIOA.
#'
#' @param df The data frame that contains the data you want to generate outcomes for.
#' @param period_start The start date of the period you want to generate outcomes for.
#' @param period_end The end date of the period you want to generate outcomes for.
#' @param msg_restricted A logical value that indicates if the function should generate outcomes for all participants or only for participants that are included per the official rules used to calculate the MSG performance indicator in WIOA. The default is FALSE.
#' @examples
#'
#' df <- generate_wioa_outcomes(df, period_start = ymd("2021-07-01"), period_end = ymd("2022-06-30"))
#'
#' @import dplyr
#' @import lubridate
#'
#' @export
generate_wioa_outcomes <- function(df, period_start, period_end, msg_restricted = FALSE) {

  df <- df
  cat("\nGenerating outcomes...")

  df <- df %>%
    mutate(p1704 = as.numeric(p1704),
           p900 = ymd(p900),
           p901 = ymd(p901),
           p1406 = ymd(p1406),
           p1801 = ymd(p1801),
           p1803 = ymd(p1803),
           p1805 = ymd(p1805),
           p1806 = ymd(p1806),
           p1807 = ymd(p1807),
           p1808 = ymd(p1808),
           p1809 = ymd(p1809),
           p1810 = ymd(p1810),
           p1811 = ymd(p1811),
           p1813 = ymd(p1813)) %>%
    mutate(cred_den = ifelse((p1303 %in% 2:4 | p1303 %in% 6:10 | p1310 %in% 2:4 |
                                p1310 %in% 6:10 | p1315 %in% 2:4 | p1315 %in% 6:10 |
                                p1332 == 1 | (p408 == 0 & p1401 == 1)), 1, NA))


  if (msg_restricted == FALSE) {
    df <- df |>
      mutate(msg_den = ifelse(!is.na(p1811), 1, NA))
  } else {
    df <- df |>
      mutate(msg_den = ifelse(((!is.na(p1811) & (p1811 <= {{period_end}})) &
                                 ((p1813 >= {{period_start}}) | (is.na(p1813)))), 1, NA))
  }


  df <- df %>%
        mutate(erq2 = ifelse(p1602 %in% 1:3, 1, 0),
           eeq2 = ifelse(p1602 %in% 1:3 | p1900 %in% 1:3, 1, 0),
           erq4 = ifelse(p1606 %in% 1:3, 1, 0),
           eeq4 = ifelse(p1606 %in% 1:3 | p1901 %in% 1:3, 1, 0)) %>%
    mutate(erq2 = ifelse(program == "youth", eeq2, erq2),
           erq4 = ifelse(program == "youth", eeq4, erq4)) %>%
    mutate(meq2 = ifelse(erq2 == 1 & (p1704 > 0 & p1704 <= 999999.99), p1704, NA),
           cred = case_when(
             ((cred_den == 1) &
                (((!is.na(p1801) & (p1801 - p901 <= years(1)) & p1800 %in% 2:7) |
                    (!is.na(p1803) & (p1803 - p901 <= years(1)) & p1802 %in% 2:7) |
                    (!is.na(p1805) & (p1805 - p901 <= years(1)) & p1804 %in% 2:7)) |
                   (((!is.na(p1801) & (p1801 - p901 <= years(1)) & p1800 == 1) |
                       (!is.na(p1803) & (p1803 - p901 <= years(1)) & p1802 == 1) |
                       (!is.na(p1805) & (p1805 - p901 <= years(1)) & p1804 == 1)) &
                      ((!is.na(p1406) & (p1406 - p901 <= years(1))) |(p1600 %in% 1:3) |
                         (p1602 %in% 1:3) | (p1604 %in% 1:3) | (p1606 %in% 1:3))))) ~ 1,
             cred_den == 1 ~ 0,
             is.na(cred_den) ~ NA_real_))

  if (msg_restricted == TRUE) {
    df <- df |>
      mutate(msg = case_when(
        msg_den == 1 &
            ((p1801 >= {{period_start}} & p1801 <= {{period_end}}) & p1800 == 1 |
             (p1803 >= {{period_start}} & p1803 <= {{period_end}}) & p1802 == 1 |
             (p1805 >= {{period_start}} & p1805 <= {{period_end}}) & p1804 == 1 |
             (p1806 >= {{period_start}} & p1806 <= {{period_end}}) |
             (p1807 >= {{period_start}} & p1807 <= {{period_end}}) |
             (p1808 >= {{period_start}} & p1808 <= {{period_end}}) |
             (p1809 >= {{period_start}} & p1809 <= {{period_end}}) |
             (p1810 >= {{period_start}} & p1810 <= {{period_end}})) ~ 1,
        msg_den == 1 ~ 0,
        is.na(msg_den) ~ NA_real_))
  } else {
    df <- df |>
      mutate(msg = case_when(
        msg_den == 1 & (
          !is.na(p1801) & p1800 == 1 |
          !is.na(p1803) & p1802 == 1 |
          !is.na(p1805) & p1804 == 1 |
          !is.na(p1806) |
          !is.na(p1807) |
          !is.na(p1808) |
          !is.na(p1809) |
          !is.na(p1810)) ~ 1,
        msg_den == 1 ~ 0,
        is.na(msg_den) ~ NA_real_))
  }


  cat("completed!\n")
  return(df)
}


#' Add general WIOA services received by the participants
#'
#' This function adds columns that indicate the high-level services received by the participant. The function creates columns for basic career services, individualized career services, and training services. The function also creates columns for work experience, youth work experience, supportive services, financial literacy, and follow-up services.
#'
#' @param data The data frame you want to add the columns to. It should include the columns p1003, p1200, p1300, p1300, p1310, p1315, p1203, p1405, p1409, p1206, and p1412.
#' @examples
#'
#' df <- add_general_wioa_services(df)
#'
#' @import dplyr
#'
#' @export
add_general_wioa_services <- function(data) {

  data <- data |>
    mutate(bcsvc = ifelse(!is.na(p1003), 1, NA),
           icsvc = ifelse(!is.na(p1200), 1, NA),
           tsvc = ifelse(p1300 == 1, 1, NA),
           tsvc1 = ifelse(is.na(p1300), 0, p1300),
           tsvc2 = ifelse(is.na(p1310), 0, p1310),
           tsvc3 = ifelse(is.na(p1315), 0, p1315),
           svc_work_exp = ifelse(!is.na(p1203), 1, NA),
           svc_youth_work_exp = ifelse(!is.na(p1405), 1, NA),
           svc_supportive = ifelse(!is.na(p1409), 1, NA),
           svc_financial_lit = ifelse(!is.na(p1206), 1, NA),
           svc_followup = ifelse(!is.na(p1412), 1, NA)) |>
    mutate(tsvc1 = ifelse(tsvc1 != 0, 1, 0),
           tsvc2 = ifelse(tsvc2 != 0, 1, 0),
           tsvc3 = ifelse(tsvc3 != 0, 1, 0)) |>
    mutate(highest_svc = case_when(
      tsvc == 1 ~ "tsvc",
      icsvc == 1 & is.na(tsvc) ~ "icsvc",
      bcsvc == 1 & is.na(icsvc) & is.na(tsvc) ~ "bcsvc")) |>
    mutate(tsvc_count = tsvc1 + tsvc2 + tsvc3)

  cat("\nAdded columns that show indicate the high-level services recieved by the participant.")
  return(data)
}


