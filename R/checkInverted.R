#' Checks records for inverted and transposed coordinates
#'
#'
#' @param x Occurrences data frame
#' @param country_gazetteer Column with the country according to the exsicatta
#' @param lat Column with the corrected clean latitude Defaults to decimalLatitude.new
#' @param lon Column with the corrected clean latitude Defaults to decimalLongitude.new
#' @param worldMap Shape with the world map borders, defaults to worldMap
#'
#' @importFrom dplyr full_join
#' @importFrom sf st_as_sf st_crs st_set_crs st_join
#'
checksInverted <- function(x = occs,
                           country_gazetteer = "country",
                           lat = "decimalLatitude.new",
                           lon = "decimalLongitude.new",
                           worldMap = worldMap) {
  cols_to_check <- c("border.check", "geo.check")
  cols <- cols_to_check[cols_to_check %in% names(x)]
  if ("border.check" %in% cols) {
    check_inv1 <- x[x$border.check %in% "check_inverted",]
  }
  if ("geo.check" %in% cols) {
    check_inv2 <- x[x$geo.check %in% "coord_original",]
  }
  check_inv <- full_join(check_inv1, check_inv2)

  check_inv$check_inv <- NULL #we'll use this column

  # create inverse lonlat #there must be a better way
  check_inv$inv_lon <- check_inv[,lon]*(-1)
  check_inv$inv_lat <- check_inv[,lat]*(-1)
  inv_lon <- "inv_lon"
  inv_lat <- "inv_lat"

  # test inverted_lon
  tmp <- sf::st_as_sf(check_inv, coords = c(inv_lon, lat))
  prj <- st_crs(4326)
  tmp <- st_set_crs(tmp, prj)
  tmp <- st_join(tmp, worldMap, join = st_intersects)
  check_inv$check_inv[tmp$NAME_0.y == tmp[, country_gazetteer]] <- "inverted_lon"

  # test inverted_lat
  tmp <- sf::st_as_sf(check_inv, coords = c(lon, inv_lat))
  tmp <- st_set_crs(tmp, prj)
  tmp <- st_join(tmp, worldMap, join = st_intersects)
  check_inv$check_inv[tmp$NAME_0.y == tmp[, country_gazetteer]] <- "inverted_lat"

  # test both inverted
  tmp <- sf::st_as_sf(check_inv, coords = c(inv_lon, inv_lat))
  tmp <- st_set_crs(tmp, prj)
  tmp <- st_join(tmp, worldMap, join = st_intersects)
  check_inv$check_inv[tmp$NAME_0.y == tmp[, country_gazetteer]] <- "inverted_lat_lon"

  # transposed: lat, lon
  tmp <- sf::st_as_sf(check_inv, coords = c(lat, lon))
  tmp <- st_set_crs(tmp, prj)
  tmp <- st_join(tmp, worldMap, join = st_intersects)
  check_inv$check_inv[tmp$NAME_0.y == tmp[, country_gazetteer]] <- "transposed_lat_lon"

  # tranposed and wrong x sign (invlat)
  tmp <- sf::st_as_sf(check_inv, coords = c(inv_lat, lon))
  st_crs(tmp)
  prj <- st_crs(4326)
  tmp <- st_set_crs(tmp, prj)
  tmp <- st_join(tmp, worldMap, join = st_intersects)
  check_inv$check_inv[tmp$NAME_0.y == tmp[, country_gazetteer]] <- "transposed_lat_and_lon_and_inv_lat"

  # tranposed and wrong y sign (invlon)
  tmp <- sf::st_as_sf(check_inv, coords = c(lat, inv_lon))
  tmp <- st_set_crs(tmp, prj)
  tmp <- st_join(tmp, worldMap, join = st_intersects)
  check_inv$check_inv[tmp$NAME_0.y == tmp[, country_gazetteer]] <- "transposed_lat_and_lon_and_inv_lon"

  y <- left_join(x, check_inv)
  return(y)
}
