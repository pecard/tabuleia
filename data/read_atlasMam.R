#' @import tidyverse
atlas_mam <- readr::read_rds('atlas_mam.rds')
usethis::use_data(atlas_mam, overwrite = T)
