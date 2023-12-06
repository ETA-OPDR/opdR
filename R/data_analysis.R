
library(dplyr)

create_wioa_program_columns <- function(data) {
  data <- data |>
    mutate(adult = ifelse(p903 %in% 1:3, 1, NA),
         dw = ifelse(p904 %in% 1:3 | p909 == 1, 1, NA),
         youth = ifelse(p905 %in% 1:3, 1, NA),
         wp = ifelse(p918 == 1, 1, NA))

  return(data)
  cat("\nA column indicating if a participant was in each WIOA program was added.")
}



