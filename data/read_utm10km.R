#' @import tidyverse
utm10k <- readr::read_rds('utm10k.rds')
usethis::use_data(utm10k, overwrite = T)
