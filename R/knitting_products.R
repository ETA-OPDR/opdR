

#' Renders state documents
#'
#' This function generates all the reports/dashboards for each state using a template file with the appropriate parameters.
#'
#' @param product The type of document you want to generate. Options are "performance_assessment", "workforce_services", and "model_summary". To add a product email zzETA-DASP@dol.gov
#' @param product_template The name of the template file you want to use to generate the reports. This should be the name of the Rmd file in the template directory.
#' @param template_dir The directory where the template file is located.
#' @param year The year of the program data you want to use to generate the reports.
#' @param select_states A vector of state codes for the states you want to generate reports for. If NULL, reports will be generated for all states and this is the default option.
#' @param exclude_states A vector of state codes for the states you want to exclude from the reports. If NULL, no states will be excluded and this is the default option.
#' @param copy_file A logical value indicating whether to copy the generated reports to the SharePoint project folder. The default is TRUE.
#' @param custom_write_dir A character string indicating the directory where the reports should be written to. If FALSE, the reports will be written to the default directory. The default is FALSE.
#' @param program_type A character string indicating the type of program data used to generate the reports. This is only required for the "workforce_services" product. The default is FALSE.
#' @param timing A character string indicating the timing of the reports. Options are "ANNUAL" and "MIDYEAR" and is only used in the "performance_assessment" product. The default is FALSE.
#' @param data_used A character string indicating the type of data used to generate the reports. This is only required for the "workforce_services" product. The default is FALSE.
#' @examples
#'
#' render_state_documents(product = "performance_assessment", product_template = "performance_assessment_template.Rmd",
#'                      template_dir = here::here("src", "templates"), year = 2021,  select_states = c("CA", "NY"), , timing = "ANNUAL")
#'
#' render_state_documents(product = "workforce_services", product_template = "workforce_services_template.Rmd",
#'                      template_dir = here::here("src", "templates"), year = 2021,  program_type = "WIOA", data_used = "imputed")
#'
#' @import here
#' @import rmarkdown
#'
#' @export
# This function generates all the state reports from a template file
render_state_documents = function(product, product_template, template_dir, year,
                                  select_states = NULL, exclude_states = NULL,
                                  copy_file = TRUE, custom_write_dir = FALSE,
                                  program_type = FALSE, timing = FALSE,  data_used = FALSE) {

  states_df <- state_info |>
    rename(s_name = Name, s_code = Alpha_code)

  if (is.null(select_states)) {
    s_list <- states_df$state_code
  } else {
    states_df <- states_df |>
      filter(s_code %in% select_states)
    s_list <- states_df$s_code
  }

  if (is.null(exclude_states)) {
    s_list <- s_list
  } else {
    states_df <- states_df |>
      filter(!s_code %in% exclude_states)
    s_list <- states_df$s_code
  }

  rmd_file <- paste0(template_dir, "/", product_template)

  for (s in s_list) {

    s_name <- states_df$state_name[states_df$s_code == s]
    r_number <- states_df$Region[states_df$s_code == s]
    r_title <- paste0("Region ", r_number)

    if (custom_write_dir == FALSE & (timing == FALSE|timing == "ANNUAL")) {
      write_dir <- here::here("reports", year, r_title)
    } else if (custom_write_dir == FALSE & timing == "MIDYEAR") {
      midyear <- paste0(year, "_MIDYEAR")
      write_dir <- here::here("reports", midyear, r_title)
    }else {
      write_dir <- custom_write_dir
    }

    if (!dir.exists(write_dir)){
      dir.create(file.path(write_dir), recursive = TRUE)
    }


    if (product == "performance_assessment") {
      cat(paste0("\n\n\nGenerating assessment for ", s, "..."))
      output_filename <- paste0(s, "_PY_", year, "_WIOA_Performance_Assessment.html")
      product_office <- "DP"

      rmarkdown::render(
        rmd_file,
        output_dir = write_dir,
        params = list(
          region = s,
          region_name = s_name,
          program_year = year),
        output_file = output_filename
      )


    } else if (product == "workforce_services") {
      cat(paste0("\n\n\nGenerating Workforce Services Dashboard for ", s, "..."))
      output_filename <- paste0(s, "_PY", year, "_Workforce_Services_Dashboard.html")
      product_office <- "DASP"

      rmarkdown::render(
        rmd_file,
        output_dir = write_dir,
        params = list(
          region = s,
          region_name = s_name,
          program_year = year,
          program_name = program_type,
          data_type = data_used),
        output_file = output_filename
      )

    } else if (product == "model_summary") {
      cat(paste0("\n\n\nGenerating model summary for ", s, "..."))
      output_filename <- paste0(s, "_PY", year, "_Model_Summary.html")
      product_office <- "DP"

      rmarkdown::render(
        rmd_file,
        output_dir = write_dir,
        params = list(
          region = s,
          region_name = s_name,
          program_year = year),
        output_file = output_filename
      )

    }

    cat("Completed")
    cat("The document is in the the reports/{year} folder of the project directory.")

    if (copy_file == TRUE) {
      cat(paste0("\nCopying the report for ", s, " to the SharePoint project folder..."))

      assessment_file <- paste0(write_dir, "/", output_filename)

      copy_to_SP(assessment_file, office = product_office)

      cat("Completed")
    }
  }
}




# This function generates all the state performance assessments.
render_state_assessments = function(product_template, template_dir, year, select_states = "all", exclude_states = "none", copy_file = TRUE, custom_write_dir = FALSE) {

  states <- state_info |>
    rename(state_name = Name, state_code = Alpha_code)

  if (select_states == "all") {
    state_list <- states$state_code
  } else {
    states <- states |>
      filter(state_code %in% select_states)
    state_list <- states$state_code
  }

  if (exclude_states == "none") {
    state_list <- state_list
  } else {
    states <- states |>
      filter(!state_code %in% exclude_states)
    state_list <- states$state_code
  }

  rmd_file <- paste0(template_dir, "/", product_template)

  for (state in state_list){

    state_name <- states$state_name[states$state_code == state]
    region <- states$Region[states$state_code == state]
    region_title <- paste0("Region ", region)

    if (custom_write_dir == FALSE) {
      output_dir <- here::here("reports", year, region_title)
    } else {
      output_dir <- custom_write_dir
    }

    if (!dir.exists(output_dir)){
      dir.create(file.path(output_dir), recursive = TRUE)
    }

    cat(paste0("\nGenerating assessment for ", state, "..."))

    output_filename <- paste0(state, "_PY_", year, "_WIOA_Performance_Assessment.html")

    rmarkdown::render(
      rmd_file,
      output_dir = output_dir,
      params = list(
        region = state,
        region_name = state_name,
        program_year = year),
      output_file = output_filename
    )

    cat("Completed")
    cat("The document is in the the reports/{year} folder of the project directory.")

    if (copy_file == TRUE) {
      cat(paste0("\nCopying the assessment for", state, " to the SharePoint project folder..."))

      assessment_file <- paste0(output_dir, "/", output_filename)

      copy_to_SP(assessment_file, office = "DP")

      cat("Completed")
    }

  }
}



#This function generates all the (or those selected) state equity reports
render_state_equity = function(product_template, template_dir, year, data_used, select_states = "all", exclude_states = "none", program, copy_file = TRUE, custom_write_dir = FALSE) {

  states <- state_info |>
    rename(state_name = Name, state_code = Alpha_code)

  if (select_states == "all") {
    state_list <- states$state_code
  } else {
    states <- states |>
      filter(state_code %in% select_states)
    state_list <- states$state_code
  }

  if (exclude_states == "none") {
    state_list <- state_list
  } else {
    states <- states |>
      filter(!state_code %in% exclude_states)
    state_list <- states$state_code
  }

  rmd_file <- paste0(template_dir, "/", product_template)

  for (state in state_list){

    state_name <- states$state_name[states$state_code == state]
    region <- states$Region[states$state_code == state]
    region_title <- paste0("Region ", region)

    if (custom_write_dir == FALSE) {
      output_dir <- here::here("reports", year, region_title)
    } else {
      output_dir <- custom_write_dir
    }

    if (!dir.exists(output_dir)){
      dir.create(file.path(output_dir), recursive = TRUE)
    }

    cat(paste0("\nGenerating equity report for ", state, "..."))

    output_filename <- paste0(state, "_PY", year, "_Equity_Report.html")

    rmarkdown::render(
      rmd_file,
      output_dir = output_dir,
      params = list(
        region = state,
        region_name = state_name,
        program_year = year,
        program_name = program,
        data_type = data_used),
      output_file = output_filename
    )

    cat("Completed")
    cat("The document is in the the reports/{year} folder of the project directory.")

    if (copy_file == TRUE) {
      cat(paste0("\nCopying the report for", state, " to the SharePoint project folder..."))

      equity_file <- paste0(output_dir, "/", output_filename)

      copy_to_SP(equity_file, office = "DASP")

      cat("Completed")
    }

  }

}







