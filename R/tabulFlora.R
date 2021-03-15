#' Build report-ready table with all known bird occurences for a region of interest.
#' @description The \code{tabulAve} function will create a reference table (Excel file)
#'     considering all plant species occurring in a Roi, joining all relevant information
#'     (taxonomic, conservation, occurrence status and legal framework).
#'     It will also summarize Roi status (ratios for protected and threatened species).
#'
#' @param ae any spatial (sp, spatialdataframe) or coordinates vector indicating a region
#'     of interest.
#'
#' @param fielddata a dataframe with bid data obtained for the roi. Tipically a table with species
#'     utm reference.
#' @param biblio character vector. Databases to get data from.  One of c('all', 'floraon', 'pterid').
#'
#' @param utm_ae character vector of utm grid cells touching roi.
#'
#' @param utm_q character vector of utm grid cells around roi
#'
#' @return an Excel file with a complete and pre-formatted table,
#'     ready for use on reports.
#'
#' @details ensure that field datasheet follow the standardized formatting.
#'
#' @author Paulo E. Cardoso
#'
#' @import sf
#' @import sp
#' @import tidyverse
#' @export
#' @examples
#'    # read roi from shapefile
#'    library(tidyverse)
#'    ae = sf::read_sf(here::here('sig', 'ae_buffer250m.shp'),
#'                     stringsAsFactors = F) %>%
#'     st_set_crs(3763)
#'    # Cast multipolygon geometry to single parts
#'    aeu = st_cast(ae, "POLYGON")
#'    # Add an UID to each
#'    aeu$id = c(1:2)
#'    # get UTM codes for your roi
#'    utm_all = utm_id(grid = utm10k, roi = aeu %>% filter(id == 2), buff = NULL, contiguity = 'queen')
#'    # tabulate species occurence and status in the area
#'    tflora = tabulEIA::tabulFlora(fielddata = NULL, utm_ae = utm_ae, utm_q = utm_contig,
#'                                biblio = 'all')
#'    # export to csv
#     write_excel_csv(tflora, path=here::here('output', 'tabela_avifauna_agolada.csv'))
tabulFlora = function(utm_ae = utm_ae, utm_q = utm_contig, fielddata = NULL, biblio = 'all'){
  # Field Data
  if(is.null(fielddata)){
    t_campo <- data.frame(
      especie=character(),
      origem=character(),
      utm=character(),
      ocorr=numeric(),
      stringsAsFactors=FALSE)
  } else t_campo <- fielddata
  t_campo <-
    t_campo %>%
    dplyr::select('especie', 'utm', 'origem', 'ocorr') %>%
    dplyr::distinct(especie, .keep_all = TRUE) %>%
    dplyr::mutate(ocorr = 1, origem = 'campo')
  if(biblio == 'all'){
    # Flora on
    t_floraon <-
      floraon %>%
      mutate(words = sapply(strsplit(especie, " "), length)) %>%
      filter(words < 4) %>%
      dplyr::select('especie', utm = 'quad') %>%
      dplyr::filter(utm %in% c(utm_ae, utm_q)) %>%
      dplyr::distinct(especie, utm, .keep_all = TRUE) %>%
      dplyr::mutate(ocorr = 2, origem = 'floraon')
    t_pterid <-
      pterid_utm %>%
      dplyr::select('especie', 'utm') %>%
      dplyr::filter(utm %in% c(utm_ae, utm_q)) %>%
      dplyr::distinct(especie, utm, .keep_all = TRUE) %>%
      dplyr::mutate(ocorr = 2, origem = 'Pterid')

  }
  # Bind all
  tm <-
    dplyr::bind_rows(t_campo, t_floraon, t_pterid) %>%
    dplyr::group_by(especie, utm) %>%
    dplyr::filter(ocorr == min(ocorr)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ocorr_o = dplyr::case_when(ocorr == 1 & utm %in% utm_ae   ~ 'Confirmado (C)',
                                             ocorr > 1 & utm %in% utm_ae    ~ 'Confirmado (B)',
                                             !utm %in% utm_ae               ~ 'Provável',
                                             TRUE ~ 'REVER!')) %>%
    dplyr::mutate(ocorr_rank = dplyr::case_when(ocorr_o == 'Confirmado (C)' ~ 1,
                                                ocorr_o == 'Confirmado (B)' ~ 2,
                                                TRUE ~ 3)) %>%
    dplyr::group_by(especie) %>%
    arrange(ocorr_rank) %>% distinct(especie, .keep_all = TRUE) %>% dplyr::ungroup()

  if(nrow(tm) == 0) stop('Cannot proceed. No data available for your Region')
  if(nrow(tm) > 0){
    tm <-
      tm %>%
      left_join(lvflora, by = c('especie' = 'Taxon'))
  } else stop('Cannot proceed. No data available for your Region')

  return(tm)
}
