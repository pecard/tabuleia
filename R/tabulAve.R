#' Build report-ready table with all known bird occurrences for a region of interest.
#' @description The \code{tabulAve} function will create a reference table
#'     considering all bird species occurring in a roi, joining all relevant information
#'     (taxonomic, conservation, occurrence status and legal framework).
#'     It will also summarize roi status (ratios for protected and threatened species).
#'
#' @param ae any spatial (sp, spatialdataframe) or coordinates vector indicating a region
#'     of interest.
#'
#' @param fielddata a data.frame with bid data obtained for the roi. Typically a table with species
#'     utm reference.
#' @param atlas data.frame with Mammal and Bat Atlas data with references to the UTM 10x10 grid cell.
#'
#' @param ebird data.frame with eBird Dataset from 2005 to 2019.
#'
#' @param gbif data.frame with GBIF Dataset from 2005 to 2019.
#'
#' @param utm_ae character vector of utm grid cells touching roi.
#'
#' @param utm_q character vector of utm grid cells around roi
#'
#' @return an data.frame file with a complete and pre-formatted table,
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
#'    ae = sf::read_sf(here::here('sig', 'ae_buffer250m.shp'),
#'                     stringsAsFactors = F) %>%
#'     st_set_crs(3763)
#'    # Cast multipolygon geometry to single parts
#'    aeu = st_cast(ae, "POLYGON")
#'    # Add an UID to each
#'    aeu$id = c(1:2)
#'    # get UTM codes for your roi
#'    utm_all = utm_id(grid = utm10k, roi = aeu %>% filter(id == 2), buff = NULL, contiguity = 'queen')
#'    utm_ae <- utm_all$ae
#'    utm_contig <- utm_all$contig
#'    # tabulate species occurence and status in the area
#'    tave = tabulEIA::tabulAve(utm_ae = utm_ae, utm_q = utm_contig, fielddata = NULL, atlas = atlas_aves, ebird = ebird, gbif = gbif)
#'    # export to csv
#     write_excel_csv(tave, path=here::here('output', 'tabela_avifauna_agolada.csv'))
tabulAve <- function(utm_ae = utm_ae, utm_q = utm_contig, fielddata = NULL, atlas = atlas_aves, ebird = NULL, gbif = NULL){
  # Field Data
  if(is.null(fielddata)){
    t_campo <- data.frame(
      especie = character(),
      utm = character(),
      ocorr = numeric(),
      origem = character(),
      fonte = character(),
      uid = character(),
      stringsAsFactors=FALSE)
  } else t_campo <- fielddata
  t_campo <-
    t_campo %>%
    #dplyr::filter(grupo == 'aves') %>%
    dplyr::select('especie', 'utm', uid) %>%
    dplyr::distinct(especie, .keep_all = TRUE) %>%
    dplyr::mutate(ocorr = 1, origem = 'campo', fonte = 'Campo')

  # Atlas
  t_atlas <-
    atlas %>%
    dplyr::select('especie', 'cod_nidificacao', 'nidificacao', 'utm', uid) %>%
    dplyr::filter(utm %in% c(utm_ae, utm_q)) %>%
    dplyr::distinct(especie, utm, .keep_all = TRUE) %>%
    dplyr::mutate(ocorr = 2, origem = 'atlas',
                  fonte = 'Bibliografia')

  # eBird
  if(is.null(ebird)){
    t_ebird <- data.frame(
      especie = character(),
      utm = character(),
      ocorr = numeric(),
      origem = character(),
      fonte = character(),
      uid = character(),
      stringsAsFactors=FALSE)
  } else t_ebird <-
    ebird %>% filter(duration_minutes >=5,
                     wcount == 2, # reduce to species with genus and specific name
                     str_detect(scientific_name, pattern = '.sp', negate = T)) %>%
    dplyr::select('especie'= scientific_name, 'utm' = UTM, uid) %>%
    dplyr::filter(utm %in% c(utm_ae, utm_q)) %>%
    dplyr::distinct(especie, utm, .keep_all = TRUE) %>%
    dplyr::mutate(ocorr = 3, origem = 'ebird',
                  fonte = 'Bibliografia')
  # GBIF
  if(is.null(gbif)){
    t_gbif <- data.frame(
      especie = character(),
      utm = character(),
      ocorr = numeric(),
      origem = character(),
      fonte = character(),
      uid = character(),
      stringsAsFactors=FALSE)
  } else t_gbif <-
    gbif %>% filter(class == 'Aves') %>%
    dplyr::select('especie'= species, 'utm' = UTM, uid) %>%
    dplyr::filter(utm %in% c(utm_ae, utm_q)) %>%
    dplyr::distinct(especie, utm, .keep_all = TRUE) %>%
    dplyr::mutate(ocorr = 4, origem = 'gbif',
                  fonte = 'Bibliografia')

  # Bind all
  tm <-
    dplyr::bind_rows(t_campo, t_atlas, t_ebird, t_gbif) %>%
    dplyr::group_by(especie, utm) %>%
    dplyr::filter(ocorr == min(ocorr)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ocorr_o = dplyr::case_when(ocorr == 1 & utm %in% utm_ae   ~ 'Confirmado (C)',
                                             utm %in% utm_ae                ~ 'Confirmado (B)',
                                             !utm %in% utm_ae               ~ 'Provável',
                                             TRUE ~ 'REVER')) %>%
    dplyr::mutate(ocorr_rank = dplyr::case_when(ocorr_o == 'Confirmado (C)' ~ 1,
                                                ocorr_o == 'Confirmado (B)' ~ 2,
                                                TRUE ~ 3)) %>%
    dplyr::group_by(especie) %>%
    arrange(ocorr_rank) %>% distinct(especie, .keep_all = TRUE) %>% dplyr::ungroup()
  if(nrow(tm) == 0) stop('Cannot proceed. No data available for your Region')

  #join with partial match
  tm <- partial_join(tm,
                     tab_ref %>%
                       dplyr::select(
                         grupo, familia,  especieRef = especie, nomecomum,
                         lvvp06_estatuto_continente, iucn20092_rl_estatuto, spec,
                         dl156a_2013_anexos, convencao_berna, convencao_bona,
                         lvvp06_endemismo, lvvp06_ocorr_continente,
                         uid
                       ),
                     by_x = "uid", pattern_y = "uid") %>%
    select(
      Grupo = grupo, Familia=familia,  Especie = especieRef, Nome_comum = nomecomum,
      Ocorrencia_na_AE = ocorr_o, LVVP_Portugal = lvvp06_estatuto_continente,
      LVIUCN = iucn20092_rl_estatuto,
      Estatuto_SPEC = spec,
      dl156a_2013_anexos, convencao_berna, convencao_bona,
      lvvp06_endemismo, Tipo_de_ocorrencia = lvvp06_ocorr_continente
    )

  # Nidificacao
  # t_nidif <- t_atlas %>% group_by(especie) %>%
  #   slice(which.max(cod_nidificacao)) %>%
  #   mutate(nidifAE =
  #            case_when(utm %in% utm_ae ~ 'Nidificação confirmada',
  #                      !utm %in% utm_ae & nidificacao == 'Nidificação confirmada' ~ 'Nidificação confirmada (*)'
  #            )) %>% select(especie, nidifAE)
  # # join reproduction code
  # tm <- tm %>% dplyr::left_join(t_nidif, by = c('especie' = 'especie')) %>% select(-especie)

  return(tm)
}
