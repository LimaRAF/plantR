#' @title Format Geographical Coordinates
#'
#' @description This function formats geographical coordinates to decimal
#'   degrees and replaces missing coordinates by the coordinates obtained from
#'   the gazetteer.
#'
#' @return The input data frame \code{coords}, plus the new columns
#'   'origin.coord' and 'precision.coord' with the formatted information. The
#'   new columns have the same name as proposed by the Darwin Core standards
#'   followed by the suffix '.new'.
#'
#' @param coords data.frame
#'
#' @details The function works as a wrapper, where the individuals steps of the
#'   proposed __plantR__ workflow for editing the geographical coordinates are
#'   performed altogether (see the __plantR__ tutorial and the help of each
#'   function for details).
#'
#'   The input data frame usually contains the following locality fields (the
#'   function defaults):
#'   - 'decimalLongitude' and 'decimalLatitude';
#'   - 'latitude.gazetteer' and 'longitude.gazetteer';
#'   - 'resolution.gazetteer';
#'   - 'decimalLatitude.new' and 'decimalLongitude.new'.
#'
#' @seealso
#'  \link[plantR]{prepCoord} and \link[plantR]{getCoord}.
#'
#' @inheritParams prepCoord
#' @inheritParams getCoord
#'
#' @export formatCoord
#'
formatCoord <- function(coords, ...) {

  # check input:
  if (!class(coords) == "data.frame")
    stop("input object needs to be a data frame!")

  # prepCoord
  coords1 <- prepCoord(coords, ...)

  # getCoord
  coords2 <- getCoord(coords1, ...)

  return(coords2)
}
