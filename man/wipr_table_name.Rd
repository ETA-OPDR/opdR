% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_management.R
\name{wipr_table_name}
\alias{wipr_table_name}
\title{Identify Name of WIPR table}
\usage{
wipr_table_name(program_year, quarter)
}
\arguments{
\item{program_year}{The program year of the reported data file table you want.}

\item{quarter}{The program year quarter of the reported data file you want.}
}
\description{
This function creates a string for the WIPR table you want. It is needed because WIPS uses a different naming convention than the program year.
}
\examples{

select_py <- 2023
select_quarter <- 4
table_name <- wipr_table_name(program_year = select_py, quarter = select_quarter)


}
