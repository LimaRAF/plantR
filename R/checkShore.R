#' @title Check Coordinates Near Shores
#'
#' @description Checks records whose coordinates fall in the sea, but close to
#'   the shoreline using different approaches.
#'
#' @param x a data frame with the species records
#' @param geo.check character. Column with the result from the coordinate checking.
#'   Defaults to 'geo.check'
#' @param lat character. Column with the corrected latitude to be checked.
#'   Defaults to 'decimalLatitude.new'
#' @param lon character. Column with the corrected longitude to be checked.
#'   Defaults to 'decimalLongitude.new'
#' @param type character. Type of approach to be used to flag records near the
#'   shore (see Details). Default to "buffer".
#' @param dist.max numerical. Maximum distance (in kilometers) to detect records
#'   near to the shore. Default to 50 km.
#' @param output a character string with the type of output desired: 'new.col'
#'   (new column with the result is added to the input data) or 'same.col'
#'   (results overwritten into column `geo.check`).
#'
#' @importFrom dplyr left_join if_else
#' @importFrom sf st_as_sf st_crs st_set_crs st_intersects st_distance
#'
#' @return if `output` is 'new.col', a new column named 'shore.check' is added
#'   to the data, containing a TRUE/FALSE vector in which TRUE means coordinates
#'   close to the shore and FALSE means coordinate in open sea. If `output` is
#'   'same.col', the column defined by `geo.check` is updated with a the
#'   previous class 'sea' is updated to 'shore' (records near the shore) and
#'   'open_sea' (records far from the shore)
#'
#' @details
#'
#' Only coordinates flagged as 'sea' by the function `checkCoord()` are included
#' in the verification. For all other coordinates, the function returns NA.
#'
#' Two approaches can be used to detect coordinates in the sea but near to the
#' shore, which are controlled by the argumente `type`. The first one uses a 0.5
#' degree (~ 55 km in the Equator) buffer around the world map (`type` =
#' 'buffer'). The second approach calculates the distance between the shore and
#' the record (`type` = 'buffer'), in which the user can provide a maximum
#' distance from the shore. This second approach, however, is much slower. For
#' the record, rounding or imprecision of coordinates at the degree and minute
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
                     dist.max = 50,
                     output = "new.col") {

  #Escaping R CMD check notes
  landBuff <- landBuff
  shoreLines <- shoreLines

  ## check input
  if (!class(x)[1] == "data.frame")
    stop("Input object needs to be a data frame!")

  ## Selecting the coordinates falling into the sea
  check_these <- grepl("sea", x[, geo.check], fixed = TRUE)

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
      test_shore <-
        suppressMessages(sf::st_intersects(tmp, land, by_element = TRUE))
      shore.check <- lengths(test_shore) == 1
    }

    if (type == "distance") {
      shores <- shoreLines
      dists <- sf::st_distance(tmp, shores)
      shore.check <- dists <= (dist.max * 1000)
    }

    if (output == "new.col") {
      x$shore.check <- NA
      x$shore.check[check_these] <- shore.check
    }

    if (output == "same.col") {
      x[check_these, geo.check][shore.check] <-
        "shore"
      x[check_these, geo.check][!shore.check] <-
        "open_sea"
    }
  } else {

    if (output == "new.col") {
      x$shore.check <- NA
      x$shore.check[check_these] <- FALSE
    }

    if (output == "same.col") {
      x[check_these, geo.check] <- "open_sea"
    }
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

