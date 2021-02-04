#' Checks occurrences that fall in the sea and close to the shores
#'
#' @param x Dataframe with species occurrence
#' @param geo.check Column with the result from the coordinate checking.
#'   Defaults to 'geo.check'
#' @param lat Column with the corrected latitude to be checked. Defaults to 'decimalLatitude.new'
#' @param lon Column with the corrected longitude to be checked. Defaults to 'decimalLongitude.new'
#'
#' @importFrom CoordinateCleaner cc_sea
#' @importFrom sf as_Spatial
#' @importFrom dplyr left_join if_else
#' @importFrom stats complete.cases
#' @importFrom utils data
#'
#' @author Andrea Sánchez-Tapia & Sara Mortara
#'
#' @export
checkSea <- function(x,
                     geo.check = "geo.check",
                     lat = "decimalLatitude.new",
                     lon = "decimalLongitude.new") {

  ## check input
  if (!class(x) == "data.frame")
    stop("Input object needs to be a data frame!")

  ## Preliminary edits
  x$tmp.order <- 1:nrow(x)

  # check_these <- complete.cases(x[, c(lon, lat)]) &
  #   x$geo.check %in% "coord_original"
  check_these <- grepl("country_bad|coord_bad", x[, geo.check], perl = TRUE) &
                    !x[, geo.check] %in% "no_cannot_check"

  ## filtering the data frame
  cols <- c("tmp.order", geo.check, lat, lon)
  check_sea <- x[check_these, cols]

  ## raflima: converting world map to sp (takes 9 seconds to convert: move to sysdata?)
  worldMap.sp <- sf::as_Spatial(worldMap)
  ## raflima: esse passo é mesmo necessário? a categoria
  test <- ##raflima: 24 secs c/ 87 registros vs. worldMap.sp
    CoordinateCleaner::cc_sea(x = check_sea,
                              lon = lon,
                              lat = lat,
                              ref = worldMap.sp,
                              value = "flagged")
  sea.check <- ifelse(test, "land", "sea")
  test_shore <-
    CoordinateCleaner::cc_sea(check_sea,
                              lon = lon,
                              lat = lat,
                              value = "flagged",
                              ref = CoordinateCleaner::buffland)
  shore.check <- ifelse(test_shore, "land", "sea")
  check_sea$sea.shore.check <-
    if_else(sea.check == "sea" & shore.check == "sea", "sea",
            if_else(sea.check == "sea" & shore.check == "land", "shore",
                    "land"))

  ## Preparing to return
  x <- dplyr::left_join(x,
                        check_sea[, c("tmp.order", "sea.shore.check")],
                        by = "tmp.order")
  x <- x[order(x$tmp.order),]
  x <- x[, -which(names(x) == "tmp.order")] # remove temporary order
  return(x)
}

