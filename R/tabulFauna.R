#' Build report-ready table with all known bird occurences for a region of interest.
#' @description The \code{tabulAve} function will create a reference table (Excel file)
#'     considering all species occuring in a roi, joining all relevant information
#'     (taxonomic, conservation, occurence status).
#'     It will also summarize roi status (ratios for protected and threatened species).
#'
#' @param ae any spatial (sp, spatialdataframe) or coordinates vector indicating a region
#'     of interest.
#'
#' @param fielddata a dataframe with bid data obtained for the roi. Tipically a table with species
#'     utm reference.
#' @param atlas dataset with reference distribution data for a taxonomic group, like the Atlas das Aves.
#'
#' @param utm_ae character vector of utm grid cells touching roi.
#'
#' @param utm_q character vector of utm grid cells around roi
#'
#' @return an Excel file with a complete and pre-formatted table,
#'     ready for use on reports.
#'
#' @details ensure that field data sheet follow the standardized formatting.
#'
#' @author Paulo E. Cardoso
#'
#' @import sf
#' @import sp
#' @import tidyverse
#' @import dplyr
#' @export
#' @examples
#'    # read roi from shapefile
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
#'    tave = tabulEIA::tabulAve(fielddata = NULL, utm_ae = utm_all$ae, utm_q = utm_all$contig, atlas = atlas_ave)
#'    # export to csv
#     write_excel_csv(tave, '~path/to/output/tabela_avifauna_agolada.csv')
#
tabulFauna = function(utm_ae = utm_ae, utm_q = utm_q, fielddata = NULL, atlas=atlas){
  if(is.null(fielddata)){
    t_campo <- data.frame(
      gps=numeric(),
      grupo=character(),
      especie=character(),
      nind=numeric(),
      metodo=character(),
      obs=character(),
      utm=character(),
      stringsAsFactors=FALSE)
  } else t_campo = fielddata
  tmam = atlas %>%
    dplyr::distinct(especie, utm) %>%
    dplyr::filter(utm %in% c(utm_ae, utm_q)) %>%
    dplyr::distinct(especie, utm, .keep_all = TRUE) %>%
    dplyr::mutate(ocorr = 2, origem = 'atlas') %>%
    dplyr::bind_rows(t_campo %>%
                       dplyr::filter(grupo == 'aves') %>%
                       dplyr::select('especie', 'utm') %>%
                       dplyr::distinct(especie, .keep_all = TRUE) %>%
                       dplyr::mutate(ocorr = 1, origem = 'campo')
    ) %>%
    dplyr::group_by(especie) %>%
    dplyr::filter(ocorr == min(ocorr)) %>%
    dplyr::mutate(nutm=n(), ocor_n=ocorr) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ocorr = dplyr::case_when(ocorr == 1 & utm %in% utm_ae ~ 'Confirmado',
                                    ocorr == 2 & utm %in% utm_ae ~ 'Confirmado (b)',
                                    nutm >= 4 & !utm %in% utm_ae ~ 'Muito provavel',
                                    nutm >= 2 | nutm < 4 & !utm %in% utm_ae  ~ 'Provavel',
                                    nutm < 2 & !utm %in% utm_ae ~ 'Possivel',
                                    TRUE ~ 'Rever')) %>%
    dplyr::mutate(ocor_n = dplyr::case_when(ocorr == 'Confirmado'     ~ 1,
                                     ocorr == 'Confirmado (b)' ~ 2,
                                     ocorr == 'Muito provavel' ~ 3,
                                     ocorr == 'Provavel'       ~ 4,
                                     ocorr == 'Possivel'       ~ 5,
                                     TRUE ~ 6)) %>%
    dplyr::mutate(ocor_pa = 'x') %>%
    dplyr::ungroup()
  if(nrow(tmam) == 0) stop('Cannot proceed. No data available for your ROI')

  tmam1 = tmam %>%
    dplyr::group_by(especie) %>%
    dplyr::slice(which.min(ocor_n)) %>%
    dplyr::left_join(tab_ref %>%
                       dplyr::select(grupo, especie, nomecomum,
                                     lvvp06_estatuto_continente,
                                     dl156a_2013_anexos, spec,
                                     convencao_berna, convencao_bona),
                     by = c('especie' = 'especie')) %>%
    dplyr::select(grupo, especie, nomecomum, origem, ocorr, lvvp06_estatuto_continente,
                  dl156a_2013_anexos, spec, convencao_berna, convencao_bona)

  tmam2 = tmam %>%
    dplyr::select(especie, utm, ocor_pa) %>%
    tidyr::spread(key = utm, value = ocor_pa, fill = NA)

  tmam1 = tmam1 %>%
    dplyr::left_join(tmam2, by = c('especie' = 'especie'))

  return(tmam1)
}
