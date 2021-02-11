#' @title Check Coordinates Near Shores
#'
#' @description Checks records whose coordinates in the sea but close to the
#'   shore using different approaches.
#'
#' @param x Dataframe with species occurrence
#' @param geo.check character. Column with the result from the coordinate checking.
#'   Defaults to 'geo.check'
#' @param lat character. Column with the corrected latitude to be checked.
#'   Defaults to 'decimalLatitude.new'
#' @param lon character. Column with the corrected longitude to be checked.
#'   Defaults to 'decimalLongitude.new'
#' @param type character. Type ...
#' @param dist.max numerical. Maximum distance (in kilometers) to detect records
#'   near to the shore. Default to 50 km.
#'
#' @importFrom dplyr left_join if_else
#' @importFrom sf st_as_sf st_crs st_set_crs st_intersects st_distance
#'
#' @return a TRUE/FALSE vector in which TRUE means coordinates close to the
#'   shore and FALSE means coordinate in open sea.
#'
#' @details
#'
#' Only coordinates flagged as 'sea' by the function `checkCoord()` are included
#' in the verification. For all other coordinates, the function returns NA.
#'
#' Two approaches can be used to detect coordinates in the sea but near to the
#' shore, which are controlled by the argumente `type`. The first one uses a 0.5
#' degree (~ 55 km in the Equator) buffer around the country map (`type` =
#' 'buffer'). The second approach calculates the distance between the shore and
#' the record (`type` = 'buffer'), in which the user can provide a maximum distance
#' from the shore. This second approach, however, is much slower. For the
#' record, rounding or imprecision of coordinates at the degree and minute
#' levels can generate distance up to 130-157 km and 2.2-2.6 km, respectively,
#' depending on the latitude considered.
#'
#' @author Andrea Sánchez-Tapia, Sara Mortara & Renato A. Ferreira de Lima
#'
#' @export checkShore
checkShore <- function(x,
                     geo.check = "geo.check",
                     lat = "decimalLatitude.new",
                     lon = "decimalLongitude.new",
                     type = "buffer",
                     dist.max = 50) {

  ## check input
  if (!class(x)[1] == "data.frame")
    stop("Input object needs to be a data frame!")

  ## Selecting the coordinates falling into the sea
  x$shore.check <- NA
  check_these <- grepl("sea", x[, geo.check], perl = TRUE) &
                    !x[, geo.check] %in% "no_cannot_check"

  if (any(check_these)) {
    ## filtering the data frame and transforming it into an sf object
    tmp <- x[check_these, c(lon, lat)]
    tmp <- sf::st_as_sf(tmp, coords = c(lon, lat))
    # set spatial coordinates
    prj <- sf::st_crs(4326)
    tmp <- sf::st_set_crs(tmp, prj)

    ## Flagging coordinates
    if (type == "buffer") {
      land <- landBuff
      test_shore <- sf::st_intersects(tmp, land, by_element = TRUE)
      shore.check <- lengths(test_shore) == 1
    }

    if (type == "distance") {
      shores <- shoreLines
      dists <- sf::st_distance(tmp, shores)
      shore.check <- dists <= (dist.max * 1000)
    }
    x$shore.check[check_these] <- shore.check

  } else {
    x$shore.check[check_these] <- FALSE
  }

  ## raflima: converting world map to sp (takes 9 seconds to convert: move to sysdata?)
  # worldMap.sp <- sf::as_Spatial(worldMap)
  # ## raflima: esse passo é mesmo necessário? Não mais!
  # test <- ##raflima: 24 secs c/ 87 registros vs. worldMap.sp
  #   CoordinateCleaner::cc_sea(x = check_sea,
  #                             lon = lon,
  #                             lat = lat,
  #                             ref = worldMap.sp,
  #                             value = "flagged")
  # sea.check <- ifelse(test, "land", "sea")
  #
  # test_shore <-
  #   CoordinateCleaner::cc_sea(check_sea,
  #                             lon = lon,
  #                             lat = lat,
  #                             value = "flagged",
  #                             ref = buffland)
  # shore.check <- ifelse(test_shore, "shore", "sea")
  # check_sea$sea.shore.check <-
  #   if_else(sea.check == "sea" & shore.check == "sea", "sea",
  #           if_else(sea.check == "sea" & shore.check == "land", "shore",
  #                   "land"))

  return(x)
}

