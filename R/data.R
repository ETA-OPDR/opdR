

#' State Information
#'
#' A dataset containing naming information for all states and territories.
#' @format A data frame
#' \describe{
#' \item{Name}{The full name of the state or territory}
#' \item{Alpha_code}{The abbreviation of the state or territory}
#' \item{Status}{The type of area}
#' \item{Region}{The ETA Regional Office of the area}
#' }
#' @source \url{https://en.wikipedia.org/wiki/List_of_U.S._state_and_territory_abbreviations}
"state_info"



#' Rural-Urban Communting Area Codes
#'
#' A dataset containing codes on rural or urban by zip code.
#' @format A data frame
#' \describe{
#' \item{ZIP_CODE}{The 5 digit zip code in a character format}
#' \item{STATE}{The abbreviation of the state or territory}
#' \item{ZIP_TYPE}{The type of zip code area}
#' \item{RUCA1}{The primary RUCA codes}
#' \item{RUCA2}{The secondary RUCA codes}
#' }
#' @source \url{https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes}
"RUCA_zipcode"

