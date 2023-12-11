# Dependent packages:
library(tidyverse)

# This function generates all the state reports from a template file
render_state_documents = function(product, product_template, template_dir, year,
                                  select_states = "all", exclude_states = "none",
                                  copy_file = TRUE, custom_write_dir = FALSE,
                                  program_type = FALSE, timing = FALSE,  data_used = FALSE) {

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
      write_dir <- here::here("reports", year, region_title)
    } else {
      write_dir <- custom_write_dir
    }

    if (!dir.exists(write_dir)){
      dir.create(file.path(write_dir), recursive = TRUE)
    }


    if (product == "performance_assessment") {
      cat(paste0("\nGenerating assessment for ", state, "..."))
      output_filename <- paste0(state, "_PY_", year, "_WIOA_Performance_Assessment.html")
      product_office <- "DP"

      rmarkdown::render(
        rmd_file,
        output_dir = write_dir,
        params = list(
          region = state,
          region_name = state_name,
          program_year = year),
        output_file = output_filename
      )


    } else if (product == "equity") {
      cat(paste0("\nGenerating equity report for ", state, "..."))
      output_filename <- paste0(state, "_PY", year, "_Equity_Report.html")
      product_office <- "DASP"

      rmarkdown::render(
        rmd_file,
        output_dir = write_dir,
        params = list(
          region = state,
          region_name = state_name,
          program_year = year,
          program_name = program_type,
          data_type = data_used),
        output_file = output_filename
      )

    } else if (product == "model_summary") {
      cat(paste0("\nGenerating model summary for ", state, "..."))
      output_filename <- paste0(state, "_PY", year, "_Model_Summary.html")
      product_office <- "DP"

      rmarkdown::render(
        rmd_file,
        output_dir = write_dir,
        params = list(
          region = state,
          region_name = state_name,
          program_year = year),
        output_file = output_filename
      )

    }

    cat("Completed")
    cat("The document is in the the reports/{year} folder of the project directory.")

    if (copy_file == TRUE) {
      cat(paste0("\nCopying the assessment for", state, " to the SharePoint project folder..."))

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







