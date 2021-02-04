#' Checks records for inverted and swapped geographical coordinates
#'
#' @param x Occurrences data frame
#' @param check.names The columns with the results from the border checking and
#'   coordinate checking, in that order. Defaults to 'border.check' and 'geo.check'.
#' @param country.gazetteer Column with the country according to the validation againt the gazetteer
#' @param lat Column with the corrected latitude. Defaults to 'decimalLatitude.new'
#' @param lon Column with the corrected longitude. Defaults to 'decimalLongitude.new'
#' @param overwrite logical. Should the newly validated coordinates overwrite
#'   the problematic ones or should they be stored in separate, new columns.
#'   Defaults to TRUE
#'
#' @importFrom dplyr full_join
#' @importFrom sf st_as_sf st_crs st_set_crs st_coordinates
#'
#' @export
#'
#' @author Andrea Sánchez-Tapia, Sara Mortara & Renato A. F. de Lima
#'
checkInverted <- function(x,
                          check.names = c("border.check", "geo.check"),
                          country.gazetteer = "country.gazet",
                          lat = "decimalLatitude.new",
                          lon = "decimalLongitude.new") {

  ## check input
  if (!class(x) == "data.frame")
    stop("Input object needs to be a data frame!")

  ## Preliminary edits
  x$tmp.order <- 1:nrow(x)

  # the input data frame may not have everything
  cols <- check.names[check.names %in% names(x)]
  if (check.names[1] %in% cols) {
    check_inv1 <- x[x[,check.names[1]] %in% "check_inverted",]
  }
  if (check.names[2] %in% cols) {
    check_inv2 <- x[grepl("country_bad|coord_bad", x[,check.names[2]], perl = TRUE) &
                      !x[,check.names[2]] %in% "no_cannot_check",]
    # check_inv2 <- x[x$geo.check %in% "coord_original",]

  }
  # check_inv <- dplyr::full_join(check_inv1, check_inv2) # rafl: dplyr is printing all variables used to join
  check_inv <- rbind.data.frame(check_inv1, check_inv2,
                                stringsAsFactors = FALSE) # rafl: is this an option?

  check_inv$check_inv <- NULL #we'll use this column

  # create inverted lonlat data
  cols1 <- c("tmp.order", check.names, country.gazetteer, lon, lat)
  tmp <- check_inv[, cols1]
  tmp$inv_lon <- -tmp[, lon]
  tmp$inv_lat <- -tmp[, lat]
  inv_lon <- "inv_lon"
  inv_lat <- "inv_lat"
  #rafl: no codigo antigo eu fazia apenas os casos 1,2,3 e 4. Se me lembro bem, não achei os casos 5 e 6. Mas o 7 deve ter...
  types <- list(inverted_lon = c(inv_lon, lat),
             inverted_lat = c(lon, inv_lat),
             inverted_both = c(inv_lon, inv_lat), #signos
             transposed = c(lat, lon), #transposto
             transposed_inv_lon = c(lat, inv_lon),
             transposed_inv_lat = c(inv_lat, lon),
             transposed_inv_both = c(inv_lat, inv_lon)) #signos e transposto
  trans.data <- vector("list", length(types))
  names(trans.data) <- names(types)
  for(i in 1:length(types))
    trans.data[[i]] <- sf::st_as_sf(tmp, coords = types[[i]])
  check <- dplyr::bind_rows(trans.data, .id = "types")

  #Removing any latitudes above the possible
  bad.lat <- abs(sf::st_coordinates(check)[,2]) > 90
  if (any(bad.lat))
    check <- check[!bad.lat,]

  ## Overlaying inverted lonlat data with the world map
  check <- sf::st_set_crs(check, sf::st_crs(worldMap))
  check1 <- sf::st_join(check, worldMap, join = sf::st_intersects)
  check1 <- check1[!is.na(check1$NAME_0),]
  if (dim(check1)[1] > 0) {
    check.ids <- data.frame(check1)[,country.gazetteer] %in% check1$NAME_0
    if (any(check.ids)) {
      check1 <- check1[check.ids, c("types", "tmp.order", check.names)]
      check1[, check.names[1]] <- NA_character_
      check1[, check.names[2]] <- paste0("ok_country[", check1$types, "]")
      new.coords <- sf::st_coordinates(check1)
      colnames(new.coords) <- c(lon, lat)
      check1$geometry <- NULL
      check1 <- cbind.data.frame(check1[,-1], new.coords,
                                 stringsAsFactors = FALSE)
    } else {
      check1 <- NULL
    }
  } else { check1 <- NULL }

  # worldMap_centroids <- sf::st_centroid(worldMap)
  # cols1 <- c(check.names, country.gazetteer, lon, lat)
  # check <- sf::st_as_sf(check_inv[, cols1], coords = c(lon, lat))
  # check <- sf::st_set_crs(check, sf::st_crs(worldMap_centroids))
  # # check <- sf::st_as_sf(check_inv, coords = c(lon, lat))
  # # paises <- match(check[, country.gazetteer], worldMap_centroids$NAME_0)
  # paises <-
  #   worldMap_centroids$NAME_0[worldMap_centroids$NAME_0 %in% check_inv[, country.gazetteer]]
  # ids.paises <- worldMap_centroids$NAME_0 %in% paises
  # original <-
  #   sf::st_distance(check, worldMap_centroids[ids.paises, ], by_element = TRUE)
  # # original <- sf::st_distance(check, worldMap_centroids[paises,], by_element = TRUE)
  #
  #
  # # create inverse lonlat #there must be a better way
  # check_inv$inv_lon <- -check_inv[,lon]
  # check_inv$inv_lat <- -check_inv[,lat]
  # inv_lon <- "inv_lon"
  # inv_lat <- "inv_lat"
  #
  # # test inverted_lon
  # cols2 <- c(check.names, country.gazetteer, lon, lat, inv_lon, inv_lat)
  # tmp <- sf::st_as_sf(check_inv[, cols2], coords = c(inv_lon, lat))
  # prj <- sf::st_crs(worldMap_centroids)
  # # prj <- sf::st_crs(4326)
  # tmp <- sf::st_set_crs(tmp, prj)
  # inverted_lon <-
  #   sf::st_distance(tmp, worldMap_centroids[ids.paises, ], by_element = TRUE)
  #
  # # test inverted_lat
  # tmp <- sf::st_as_sf(check_inv[, cols2], coords = c(lon, inv_lat))
  # tmp <- sf::st_set_crs(tmp, prj)
  # inverted_lat <-
  #   sf::st_distance(tmp, worldMap_centroids[ids.paises, ], by_element = TRUE)
  #
  # # test both inverted
  # tmp <- sf::st_as_sf(check_inv[, cols2], coords = c(inv_lon, inv_lat))
  # tmp <- sf::st_set_crs(tmp, prj)
  # inverted_both <-
  #   sf::st_distance(tmp, worldMap_centroids[ids.paises, ], by_element = TRUE)
  #
  # # transposed: lat, lon
  # tmp <- sf::st_as_sf(check_inv[, cols2], coords = c(lat, lon))
  # tmp <- sf::st_set_crs(tmp, prj)
  # transposed <-
  #   suppressWarnings(
  #     sf::st_distance(tmp, worldMap_centroids[ids.paises, ], by_element = TRUE))
  #
  # # transposed and wrong x sign (invlat)
  # tmp <- sf::st_as_sf(check_inv[, cols2], coords = c(inv_lat, lon))
  # tmp <- sf::st_set_crs(tmp, prj)
  # transposed_inv_lat <-
  #   suppressWarnings(
  #     sf::st_distance(tmp, worldMap_centroids[ids.paises, ], by_element = TRUE))
  #
  # # transposed and wrong y sign (invlon)
  # tmp <- sf::st_as_sf(check_inv[, cols2], coords = c(lat, inv_lon))
  # tmp <- sf::st_set_crs(tmp, prj)
  # transposed_inv_lon <-
  #   suppressWarnings(
  #     sf::st_distance(tmp, worldMap_centroids[ids.paises, ], by_element = TRUE))
  #
  # # transposed and both wrong signs
  # tmp <- sf::st_as_sf(check_inv[, cols2], coords = c(inv_lat, inv_lon))
  # tmp <- sf::st_set_crs(tmp, prj)
  # transposed_inv_both <-
  #   suppressWarnings(
  #     sf::st_distance(tmp, worldMap_centroids[ids.paises, ], by_element = TRUE))

  ### PROBLEM: If not even the country was found, the distance does not work! Check with the tutorial example
  #need to overlay with the world map again....

  # dists <- data.frame(original,
  #                     inverted_lon,
  #                     inverted_lat,
  #                     inverted_both, #signos
  #                     transposed, #transposto
  #                     transposed_inv_lon,
  #                     transposed_inv_lat,
  #                     transposed_inv_both)#signos e transposto
  # options_dist <- names(dists)
  # check_inv$inv.check <- options_dist[max.col(-dists)] #min.col não existe ¬¬

  ## Preparing to return
  if (is.null(check1)) {
    x <- x[, -which(names(x) == "tmp.order")] # remove temporary order
    return(x)
  } else {
    y <- dplyr::left_join(x,
                          check1,
                          by = "tmp.order", suffix = c("", ".new"))
    sub.cols <- names(check1)[!names(check1) %in% "tmp.order"]
    sub.cols.new <-paste0(sub.cols, ".new")
    if (overwrite) {
      sud.rows <- y$tmp.order %in% check1$tmp.order
      x[sud.rows, sub.cols] <- y[sud.rows, sub.cols.new]
    } else {
      x[, sub.cols.new] <- y[sud.rows, sub.cols.new]
    }
    x <- x[order(x$tmp.order),]
    x <- x[, -which(names(x) == "tmp.order")] # remove temporary order
    return(x)
  }

  # y <- dplyr::left_join(x, check_inv)
  # return(y)
}
