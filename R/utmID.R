#' Identify the UTM grid squares touching and around a region of interest (roi).
#' @description The \code{utm_id} function will identify the UTM grid codes
#' that touches and those in the vicinity (queen or rook) of roi.
#'
#' @param roi an spatial object sf, st, spatialpolygonsdataframe identifying
#' the roi.' It can be of any valid vector type (point, line, polygon or collection).
#'
#' @param buf numeric indication whether the roi should be buffered to a given distance (meter).
#' Default is NULL
#'
#' @param contiguity character identifying the contiguity search method "queen" or "rook".
#'
#' @param grid character indicating the UTM grid. Ensure you provide an 'UTM' column as
#' grid code.
#'
#' @return A vector with UTM codes for study site (ae) and contiguous grids.
#'
#' @details Make sure you have a topologically valid  vector. Using external files (like a shapefile)
#' will require the .prj otherwise identify it with the corresponding numeric epsg code.
#' Currently only rook and queen implemented (not bishop)
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
#'                     stringsAsFactors = F)
#'    # Cast multipolygon geometry to single parts
#'    aeu = st_cast(ae, "POLYGON")
#'    # Add an UID to each
#'    aeu$id = c(1:2)
#'    # get UTM codes for your roi
#'    utm_all = utm_id(grid = utm10k, roi = aeu %>% filter(id == 2), buff = NULL, contiguity = 'queen')
#'    utm_ae <- utm_all$ae
#'    utm_contig <- utm_all$contig
#'    }
utm_id = function(grid = utm10, roi = ae, buff = NULL, contiguity = 'queen'){
  grid = grid %>% dplyr::select('UTM')
  if(is.na(st_crs(roi))) stop('No projection provided for your roi')
  #' check projection supplied
  if(sf::st_crs(roi)$epsg != 3763){
    roi = sf::st_transform(roi, 3763)
  }
  #' buffer roi
  if(!is.null(buff)){
    roi = sf::st_buffer(roi, buff)
  }
  #' Intersects
  if(any(class(roi) == 'sf')){
    utm_ae = sf::st_join(roi, grid)[['UTM']]
  }
  if(any(class(roi) == 'character')){
    utm_ae = roi
  }
  # Contiguity
  if(contiguity == 'queen'){
    #coln = as.name(gridid)
    utm_l = list()
    for(i in utm_ae){
      utm_l[[i]] = grid %>%
        dplyr::mutate(nb_rook =
                        as.numeric(ist_queen(.,
                                             b = grid %>%
                                               subset(UTM %in% i)
                        )
                        )
        ) %>%
        dplyr::filter(nb_rook == 1)
    }
    utm_c = do.call(rbind, utm_l) %>%
      dplyr::distinct(UTM, .keep_all = T) %>%
      st_set_geometry(NULL) %>%
      dplyr::select(UTM) %>%
      dplyr::filter(! UTM %in% utm_ae) %>%
      dplyr::pull(UTM)
  }
  if(contiguity == 'rook'){
    utm_l = list()
    for(i in utm_ae){
      utm_l[[i]] = grid %>%
        dplyr::mutate(nb_rook =
                        as.numeric(ist_rook(.,
                                             b = grid %>%
                                               subset(UTM %in% i)
                        )
                        )
        ) %>%
        dplyr::filter(nb_rook == 1)
    }
    utm_c = do.call(rbind, utm_l) %>% dplyr::distinct(UTM, .keep_all = T) %>%
      st_set_geometry(NULL) %>% select(UTM) %>% filter(! UTM %in% utm_ae) %>%
      pull(UTM)

  }
  utml = list("ae"=utm_ae, 'contig' = utm_c)
  return(utml)
}
