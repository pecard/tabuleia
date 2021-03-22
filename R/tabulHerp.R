#' Build report-ready table with all known amphibians and reptiles occurrences for a region of interest.
#' @description The \code{tabulMam} function will create a reference table (Excel file)
#'     considering all amphibians and reptiles species occurring in a region, joining all relevant information
#'     (taxonomic, conservation, occurrence status and legal framework).
#'     It will also (in the future) summarize region status (ratios for protected and threatened species).
#'
#' @param ae sf object. any spatial (sp, spatialdataframe) or coordinates vector indicating a region
#'     of interest.
#'
#' @param fielddata dataframe. Table with bid data obtained for the roi. Typically a table with species
#'     utm reference.
#' @param atlas character. ICNF Atlas data with references 17384 entries to the UTM 10x10 grid cell.
#'
#' @param inat logical. TRUE to use Inaturalist Dataset for 2018 and 2019 with thousands of entries available
#' on iNaturalist database from 2018 and 2019.
#'
#' @param dhab character. Use Habitat Directive data from 2013-2018 report published by ICNF.
#'
#' @param utm_ae character. vector of utm grid cells touching roi.
#'
#' @param utm_q character. vector of utm grid cells around roi
#'
#' @return an Excel file with a complete and pre-formatted table,
#'     (almost) ready for use on reports.
#'
#' @details ensure that field dataset follow the standardized formatting.
#'
#' @author Paulo E. Cardoso
#'
#' @import sf
#' @import sp
#' @import tidyverse
#' @export
#' @examples
#' \dontrun{
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
#'    tmam = tabulEIA::tabulMam(utm_ae = utm_ae, utm_q = utm_contig, fielddata = NULL, atlas = 'all', inat = FALSE)
#'    # export to csv
#'     write_excel_csv(tmam, path=here::here('output', 'tab_mammals.csv'))
#'     }
tabulHerp = function(utm_ae = utm_ae, utm_q = utm_contig, fielddata = NULL, atlas = atlas_herp, dhab = dh13_18, inat = NULL){
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
  t_atlash <-
    atlas_herp %>%
    dplyr::select(especie, utm=UTM, uid) %>%
    dplyr::filter(utm %in% c(utm_ae, utm_q)) %>%
    dplyr::distinct(especie, utm, .keep_all = TRUE) %>%
    dplyr::mutate(ocorr = 2, origem = 'ICNF', fonte = 'Bibliografia')

  if(is.null(dhab)){
    # diretiva habitats 2013-2018
    t_dhab <- data.frame(
      especie = character(),
      utm = character(),
      ocorr = numeric(),
      origem = character(),
      fonte = character(),
      uid = character(),
      stringsAsFactors=FALSE)
  } else{
    t_dhab <-
      dhab %>%
      dplyr::filter(grupo %in% c("Anfíbios",   "Reptiles"), utm %in% c(utm_ae, utm_q)) %>%
      dplyr::select(especie, utm, uid) %>%
      dplyr::distinct(especie, utm, .keep_all = TRUE) %>%
      dplyr::mutate(ocorr = 3, origem = 'DH2013-2018', fonte = 'Bibliografia')
  }

  if(is.null(inat)){
    # iNaturalist
    t_inat <- data.frame(
      especie = character(),
      utm = character(),
      ocorr = numeric(),
      origem = character(),
      fonte = character(),
      uid = character(),
      stringsAsFactors=FALSE)
  } else{
    t_inat <-
      inat18_19 %>%
      dplyr::select(especie, utm, uid) %>%
      dplyr::filter(utm %in% c(utm_ae, utm_q)) %>%
      dplyr::distinct(especie, utm, .keep_all = TRUE) %>%
      dplyr::mutate(ocorr = 4, origem = 'iNat', fonte = 'Bibliografia')
  }

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
    gbif %>% filter(class %in% c("Reptilia", "Amphibia")) %>%
    dplyr::select('especie'= species, 'utm' = UTM, uid) %>%
    dplyr::filter(utm %in% c(utm_ae, utm_q)) %>%
    dplyr::distinct(especie, utm, .keep_all = TRUE) %>%
    dplyr::mutate(ocorr = 5, origem = 'gbif',
                  fonte = 'Bibliografia')

  # Bind all
  tm <-
    dplyr::bind_rows(t_campo, t_atlash, t_dhab, t_inat, t_gbif) %>%
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
                         lvvp06_endemismo,lvvp06_ocorr_continente,
                         uid
                       ),
                     by_x = "uid", pattern_y = "uid") %>%
    select(
      Grupo = grupo, Familia=familia,  Especie = especieRef, Nome_comum = nomecomum,
      LVVP_Portugal = lvvp06_estatuto_continente,
      LVIUCN = iucn20092_rl_estatuto,
      Estatuto_SPEC = spec,
      dl156a_2013_anexos, convencao_berna, convencao_bona,
      lvvp06_endemismo, Tipo_de_ocorrencia = lvvp06_ocorr_continente,
      Ocorrencia_na_AE = ocorr_o
    )
  return(tm)
}
