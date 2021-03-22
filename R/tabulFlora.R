#' Build report-ready table with all known plant species occurences for a region of interest (roi).
#' @description The \code{tabulAve} function will create a reference table (Excel file)
#'     considering all plant species occurring in a Roi, joining all relevant information
#'     (taxonomic, conservation, occurrence status and legal framework).
#'
#' @param ae any spatial (sp, sf, spatialdataframe) or coordinates vector indicating a region
#'     of interest.
#'
#' @param fielddata a dataframe with data obtained for the roi. Tipically a table with species and
#'     utm 10x10 code reference.
#'
#' @param biblio character vector. Databases to get data from.  One of c('all', 'floraon', 'pterid').
#'
#' @param utm_ae character vector of utm grid cells touching roi.
#'
#' @param utm_q character vector of utm grid cells around roi
#'
#' @return a table with a complete and pre-formatted table,
#'     for use in reports.
#'
#' @details ensure that field datasheet follow the standardized formatting (Flora_TabulEIA_Template.xlsx).
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
#'    write_excel_csv(tflora, path=here::here('output', 'tabela_avifauna_agolada.csv'))
#'    }
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

    tm$genero <- sub("^(\\w+)\\s?(.*)$","\\1",tm$especie) # split name at 1st space
    tm <-
      tm %>%
      left_join(ref_flora_alfa, by = c('genero' = 'genero')) %>%
      left_join(lvflora, by = c('especie' = 'Taxon')) %>%
      select(Especie=especie, Familia = family, Ocorrencia = ocorr_o, UTM = utm,
             Grau_Endemismo = Endemismo, Categoria_LVF = Categoria,
             CriteriosLVF = Criterios) %>%
      pivot_wider(names_from = UTM, values_from = UTM, values_fill = '-')

  } else stop('Cannot proceed. No data available for your Region')

  return(tm)
}
