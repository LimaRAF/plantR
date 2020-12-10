#' Checks records for inverted and transposed coordinates
#'
#'
#' @param x Occurrences data frame
#' @param country_gazetteer Column with the country according to the exsicatta
#' @param lat Column with the corrected clean latitude Defaults to decimalLatitude.new
#' @param lon Column with the corrected clean latitude Defaults to decimalLongitude.new
#'
#' @importFrom dplyr full_join
#' @importFrom sf st_as_sf st_crs st_set_crs st_join
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
  cent_dist <- st_distance(check, worldMap_centroids[paises,], by_element = TRUE)

  # create inverse lonlat #there must be a better way
  check_inv$inv_lon <- check_inv[,lon]*(-1)
  check_inv$inv_lat <- check_inv[,lat]*(-1)
  inv_lon <- "inv_lon"
  inv_lat <- "inv_lat"

  # test inverted_lon
  tmp <- sf::st_as_sf(check_inv, coords = c(inv_lon, lat))
  prj <- st_crs(4326)
  tmp <- st_set_crs(tmp, prj)
  # checa por onde cai
  # tmp <- st_join(tmp, worldMap, join = st_intersects)
  # check_inv$check_inv[tmp$NAME_0.y == check_inv[, country_gazetteer]] <- "inverted_lon"
  # checa por distancia
  inv_lon_dist <- st_distance(tmp, worldMap_centroids[paises,], by_element = TRUE)

  # test inverted_lat
  tmp <- sf::st_as_sf(check_inv, coords = c(lon, inv_lat))
  tmp <- st_set_crs(tmp, prj)
  #tmp <- st_join(tmp, worldMap, join = st_intersects)
  #check_inv$check_inv[tmp$NAME_0.y == check_inv[, country_gazetteer]] <- "inverted_lat"
  # checa por distancia
  inv_lat_dist <- st_distance(tmp, worldMap_centroids[paises,], by_element = TRUE)

  # test both inverted
  tmp <- sf::st_as_sf(check_inv, coords = c(inv_lon, inv_lat))
  tmp <- st_set_crs(tmp, prj)
  #tmp <- st_join(tmp, worldMap, join = st_intersects)
  #check_inv$check_inv[tmp$NAME_0.y == check_inv[, country_gazetteer]] <- "inverted_both"
  # checa por distancia
  both_inv_dist <- st_distance(tmp, worldMap_centroids[paises,], by_element = TRUE)

  # transposed: lat, lon
  tmp <- sf::st_as_sf(check_inv, coords = c(lat, lon))
  tmp <- st_set_crs(tmp, prj)
  #tmp <- st_join(tmp, worldMap, join = st_intersects)
  #check_inv$check_inv[tmp$NAME_0.y == check_inv[, country_gazetteer]] <- "transposed"
  # checa por distancia
  transposed_dist <- st_distance(tmp, worldMap_centroids[paises,], by_element = TRUE)

  # transposed and wrong x sign (invlat)
  tmp <- sf::st_as_sf(check_inv, coords = c(inv_lat, lon))
  tmp <- st_set_crs(tmp, prj)
  #tmp <- st_join(tmp, worldMap, join = st_intersects)
  #check_inv$check_inv[tmp$NAME_0.y == check_inv[, country_gazetteer]] <- "transposed_lat_and_lon_and_inv_lat"
  trans_inv_lat_dist <- st_distance(tmp, worldMap_centroids[paises,], by_element = TRUE)

  # transposed and wrong y sign (invlon)
  tmp <- sf::st_as_sf(check_inv, coords = c(lat, inv_lon))
  tmp <- st_set_crs(tmp, prj)
  #tmp <- st_join(tmp, worldMap, join = st_intersects)
  #check_inv$check_inv[tmp$NAME_0.y == check_inv[, country_gazetteer]] <- "transposed_lat_and_lon_and_inv_lon"
  trans_inv_lon_dist <- st_distance(tmp, worldMap_centroids[paises,], by_element = TRUE)

  # transposed and both wrong signs
  tmp <- sf::st_as_sf(check_inv, coords = c(inv_lat, inv_lon))
  tmp <- st_set_crs(tmp, prj)
  #tmp <- st_join(tmp, worldMap, join = st_intersects)
  #check_inv$check_inv[tmp$NAME_0.y == check_inv[, country_gazetteer]] <- "transposed_and_both_bad_sign"
  trans_both_inv_dist <- st_distance(tmp, worldMap_centroids[paises,], by_element = TRUE)

  dists <- data.frame(original = cent_dist,
                      inverted_lon = inv_lon_dist,
                      inverted_lat = inv_lat_dist,
                      inverted_both = both_inv_dist, #signos
                      transposed = transposed_dist, #transposto
                      transposed_inv_lon = trans_inv_lon_dist,
                      transposed_inv_lat = trans_inv_lat_dist,
                      transposed_inv_both = trans_both_inv_dist)#signos e transposto
  options_dist <- names(dists)
  check_inv$check_inv_dist <- options_dist[max.col(-dists)]
  y <- left_join(x, check_inv)
  return(y)
}
