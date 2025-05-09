

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

