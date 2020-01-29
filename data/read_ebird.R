#' @import tidyverse
ebird <- readr::read_rds('ebirdjan2019.rds')
usethis::use_data(ebird, overwrite = T)
