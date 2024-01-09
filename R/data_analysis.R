
library(dplyr)
library(lubridate)

create_wioa_program_columns <- function(data) {
  data <- data |>
    mutate(adult = ifelse(p903 %in% 1:3, 1, NA),
         dw = ifelse(p904 %in% 1:3 | p909 == 1, 1, NA),
         youth = ifelse(p905 %in% 1:3, 1, NA),
         wp = ifelse(p918 == 1, 1, NA))

  return(data)
  cat("\nA column indicating if a participant was in each WIOA program was added.")
}





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

  data <- data |>
    select(-p3000, -p4000)

  return(data)

  cat("\nThe 'state' column now indicates the reporting state. p3000 and/or p4000 were dropped if present.")
}




add_outcome_type_date_variables <- function(program_year) {

  py <- as.numeric(program_year)

  py_end <<- ymd(paste0(py + 1, "0630"))
  py_start <<- py_end %m-% years(1) %m+% days(1)
  py_roll4_start <<- py_start %m-% months(9, abbreviate = FALSE)
  q2_end <<- py_end %m-% years(1)
  q2_start <<- py_start %m-% years(1)
  q2_roll4_start <<- py_start %m-% years(1) %m-% months(9, abbreviate = FALSE)
  q4_end <<- ceiling_date((q2_end %m-% months(6, abbreviate = FALSE)),"month") - days(1)
  q4_start <<- q2_start %m-% months(6, abbreviate = FALSE)
  q4_roll4_start <<- q2_start %m-% months(6, abbreviate = FALSE) %m-% months(9, abbreviate = FALSE)

  cat("\nAdded outcome type date variables to the environment.")
}



