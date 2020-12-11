#' Checks records for inverted and transposed coordinates
#'
#'
#' @param x Occurrences data frame
#' @param country_gazetteer Column with the country according to the exsicatta
#' @param lat Column with the corrected clean latitude Defaults to decimalLatitude.new
#' @param lon Column with the corrected clean latitude Defaults to decimalLongitude.new
#'
#' @importFrom dplyr full_join
#' @importFrom sf st_as_sf st_crs st_set_crs st_centroid st_distance
#'
checksInverted <- function(x = occs,
                           country_gazetteer = "country",
                           lat = "decimalLatitude.new",
                           lon = "decimalLongitude.new") {
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

  worldMap_centroids <- sf::st_centroid(worldMap)
  check <- st_as_sf(check_inv)
  paises <- match(check$country, worldMap_centroids$NAME_0)
  #calcula distancia NAO PAIRWISE MAS do vetor de paises originl com o centroide correspondente
  original <- st_distance(check, worldMap_centroids[paises,], by_element = TRUE)

  # create inverse lonlat #there must be a better way
  check_inv$inv_lon <- check_inv[,lon]*(-1)
  check_inv$inv_lat <- check_inv[,lat]*(-1)
  inv_lon <- "inv_lon"
  inv_lat <- "inv_lat"

  # test inverted_lon
  tmp <- sf::st_as_sf(check_inv, coords = c(inv_lon, lat))
  prj <- st_crs(4326)
  tmp <- st_set_crs(tmp, prj)
  inverted_lon <- st_distance(tmp, worldMap_centroids[paises,], by_element = TRUE)

  # test inverted_lat
  tmp <- sf::st_as_sf(check_inv, coords = c(lon, inv_lat))
  tmp <- st_set_crs(tmp, prj)
  inverted_lat <- st_distance(tmp, worldMap_centroids[paises,], by_element = TRUE)

  # test both inverted
  tmp <- sf::st_as_sf(check_inv, coords = c(inv_lon, inv_lat))
  tmp <- st_set_crs(tmp, prj)
  inverted_both <- st_distance(tmp, worldMap_centroids[paises,], by_element = TRUE)

  # transposed: lat, lon
  tmp <- sf::st_as_sf(check_inv, coords = c(lat, lon))
  tmp <- st_set_crs(tmp, prj)
  transposed <- st_distance(tmp, worldMap_centroids[paises,], by_element = TRUE)

  # transposed and wrong x sign (invlat)
  tmp <- sf::st_as_sf(check_inv, coords = c(inv_lat, lon))
  tmp <- st_set_crs(tmp, prj)
  transposed_inv_lat <- st_distance(tmp, worldMap_centroids[paises,], by_element = TRUE)

  # transposed and wrong y sign (invlon)
  tmp <- sf::st_as_sf(check_inv, coords = c(lat, inv_lon))
  tmp <- st_set_crs(tmp, prj)
  transposed_inv_lon <- st_distance(tmp, worldMap_centroids[paises,], by_element = TRUE)

  # transposed and both wrong signs
  tmp <- sf::st_as_sf(check_inv, coords = c(inv_lat, inv_lon))
  tmp <- st_set_crs(tmp, prj)
  transposed_inv_both <- st_distance(tmp, worldMap_centroids[paises,], by_element = TRUE)

  dists <- data.frame(original,
                      inverted_lon,
                      inverted_lat,
                      inverted_both, #signos
                      transposed, #transposto
                      transposed_inv_lon,
                      transposed_inv_lat,
                      transposed_inv_both)#signos e transposto
  options_dist <- names(dists)
  check_inv$inv.check <- options_dist[max.col(-dists)] #min.col não existe ¬¬
  y <- left_join(x, check_inv)
  return(y)
}
