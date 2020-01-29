
#' @export
ist_rook = function(a, b = a) st_relate(a, b, pattern = "F***1****")

#' @export
ist_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")
