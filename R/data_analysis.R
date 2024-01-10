
library(dplyr)
library(lubridate)

create_wioa_program_columns <- function(data) {
  data <- data |>
    mutate(adult = ifelse(p903 %in% 1:3, 1, NA),
         dw = ifelse(p904 %in% 1:3 | p909 == 1, 1, NA),
         youth = ifelse(p905 %in% 1:3, 1, NA),
         wp = ifelse(p918 == 1, 1, NA))

  cat("\nA column indicating if a participant was in each WIOA program was added.")
  return(data)
}




format_zip_code <- function (data, zip_column) {

  data <- data |>
    mutate(zip_column = ifelse(str_length(zip_column) < 3, 99999, zip_column)) |>
    mutate(zip_column = str_pad(as.character(zip_column), 5, side = "left", pad = "0"))

  cat("Formatted the zip code column to have 5 characters.")
  return(data)
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



generate_program_outcomes <- function(df, period_start, period_end) {

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
                                p1332 == 1 | (p408 == 0 & p1401 == 1)), 1, NA),
           msg_den = ifelse(((!is.na(p1811) & (p1811 <= period_end)) &
                               ((p1813 >= period_start) | (is.na(p1813)))), 1, NA)) %>%
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
             is.na(cred_den) ~ NA_real_),
           msg = case_when(
             msg_den == 1 &
               ((p1801 >= period_start & p1801 <= period_end) & p1800 == 1 |
                  (p1803 >= period_start & p1803 <= period_end) & p1802 == 1 |
                  (p1805 >= period_start & p1805 <= period_end) & p1804 == 1 |
                  (p1806 >= period_start & p1806 <= period_end) |
                  (p1807 >= period_start & p1807 <= period_end) |
                  (p1808 >= period_start & p1808 <= period_end) |
                  (p1809 >= period_start & p1809 <= period_end) |
                  (p1810 >= period_start & p1810 <= period_end)) ~ 1,
             msg_den == 1 ~ 0,
             is.na(msg_den) ~ NA_real_))

  cat("completed!\n")
  return(df)
}



add_general_services_received <- function(data) {

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


