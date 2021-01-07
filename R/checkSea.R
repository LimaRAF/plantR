#' Checks occurrences that fall in the sea and close to the shores
#'
#' @param x Dataframe with species occurrence
#' @param lat Column with the latitude to be checked. Defaults to *.new.new that was corrected for inverse coordinates
#' @param lon Column with the latitude to be checked. Defaults to *.new.new that was corrected for inverse coordinates
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
checkSea <- function(x = data.frame(occs),
                     lat = "decimalLatitude.new.new",
                     lon = "decimalLongitude.new.new") {
  check_these <- complete.cases(x[, c(lon, lat)]) &
    x$geo.check %in% "coord_original"
  check_sea   <- x[check_these,]

  test <-
    CoordinateCleaner::cc_sea(x = check_sea,
                              lon = lon,
                              lat = lat,
                              ref = sf::as(worldMap, "Spatial"),
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
  x <- left_join(x, check_sea)
}

