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
#'   The input data frame usually contains the following fields (the
#'   function defaults):
#'   - the source coordinates: 'decimalLongitude' and 'decimalLatitude';
#'   - the gazetteer coordinates: 'latitude.gazetteer' and 'longitude.gazetteer';
#'   - the precision of the gazetteer coordinates: 'resolution.gazetteer';
#'   - the working coordinates: 'decimalLatitude.new' and 'decimalLongitude.new'.
#'
#' @seealso
#'  \link[plantR]{prepCoord} and \link[plantR]{getCoord}.
#'
#' @inheritParams prepCoord
#' @inheritParams getCoord
#'
#' @export formatCoord
#'
formatCoord <- function(coords,
                        lat = "decimalLatitude",
                        lon = "decimalLongitude",
                        flag = TRUE,
                        lat.orig = "decimalLatitude",
                        lon.orig = "decimalLongitude",
                        lat.gazet = "latitude.gazetteer",
                        lon.gazet = "longitude.gazetteer",
                        res.gazet = "resolution.gazetteer",
                        lat.new = "decimalLatitude.new",
                        lon.new = "decimalLongitude.new",
                        rm.gazet = FALSE) {

  # check input:
  if (!class(coords) == "data.frame")
    stop("input object needs to be a data frame!")

  # prepCoord
  coords1 <- prepCoord(x = coords, lat, lon, flag)

  # getCoord
  coords2 <- getCoord(x = coords1,
                      lat.orig,
                      lon.orig,
                      lat.gazet,
                      lon.gazet,
                      res.gazet,
                      lat.new,
                      lon.new,
                      rm.gazet)

  return(coords2)
}
