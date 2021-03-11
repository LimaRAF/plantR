#' @title Check Inverted/Swapped Coordinates
#'
#' @description The function detects possible inversions and/or swaps in the
#' geographical coordinates of the records, by creating all possible combinations
#' of inverted and swapped latitude/longitudes (see Details) and crossing them
#' with the world map. If the procedure does not found at least one
#' inverted/swapped coordinate, the input data is returned without edits.
#'
#' @param x Occurrences data frame
#' @param check.names The columns with the results from the border checking and
#'   coordinate checking, in that order. Defaults to 'border.check' and
#'   'geo.check'.
#' @param country.gazetteer Name of the column with the country that comes from
#'   the gazetteer
#' @param lat Column with the corrected latitude. Defaults to
#'   'decimalLatitude.new'
#' @param lon Column with the corrected longitude. Defaults to
#'   'decimalLongitude.new'
#' @param output a character string with the type of output desired: 'new.col'
#'   (new column with the newly validated coordinates are added to the input
#'   data) or 'same.col' (results are overwritten into the existing columns).
#'
#' @return if `output` is 'new.col', new columns with a suffix '.new' are added
#'   to the data, containing the update information on the columns defined by
#'   `check.names`, `lat` and `lon`. If `output` is 'same.col', the columns
#'   defined by these arguments are updated with the validated information after
#'   inverting/swapping the coordinates.
#'
#' @details Besides the newly validade geographical coordinates, the function
#' returns a 'ok_country' followed by the information on which combination of
#' inverted (change in coordinate signal) and/or swapped coordinates (longitude
#' as latitude and vice-versa) the validation was acquired. This information is
#' provided in brackets, as follows:
#'    - 'invert_lon': inverted longitude
#'    - 'invert_lat': inverted latitude
#'    - 'invert_both': inverted longitude and latitude
#'    - 'transposed': longitude as latitude and vice-versa (i.e. swap)
#'    - 'transp_inv_lon': latitude as longitude and inverted longitude as latitude
#'    - 'transp_inv_lat': inverted latitude as longitude and longitude as latitude
#'    - 'transp_inv_both': inverted latitude as longitude and inverted longitude as latitude
#'
#' @importFrom sf st_as_sf st_crs st_set_crs st_coordinates st_join st_intersects st_geometry
#'
#' @export checkInverted
#'
#' @author Andrea Sánchez-Tapia, Sara Mortara & Renato A. F. de Lima
#'
checkInverted <- function(x,
                          check.names = c("border.check", "geo.check"),
                          country.gazetteer = "country.gazet",
                          lat = "decimalLatitude.new",
                          lon = "decimalLongitude.new",
                          output = "new.col") {

  #Escaping R CMD check notes from using dplyr syntax
  worldMap <- worldMap

  ## check input
  if (!class(x) == "data.frame")
    stop("Input object needs to be a data frame!")

  if (!all(c(lat, lon, country.gazetteer) %in% colnames(x)))
    stop("One or more column names declared do not match those of the input object: please rename or specify the correct names")

  ## Check the gazetteer country information
  if (any(grepl("_", x[, country.gazetteer], fixed = TRUE))) {
    country.gazet <- sapply(strsplit(x[, country.gazetteer], "_", fixed = TRUE),
                            function (x) x[1])
  } else {
    country.gazet <- x[, country.gazetteer]
  }

  ## Preliminary edits
  x$tmp.order <- 1:nrow(x)
  x$tmp.country.gazet <- country.gazet

  # the input data frame may not have all columns
  cols <- check.names[check.names %in% names(x)]
  if (check.names[1] %in% cols) {
    check_inv1 <- x[x[, check.names[1]] %in% FALSE,]
  } else {
    check_inv1 <- x[FALSE,]
  }

  if (check.names[2] %in% cols) {
    check_these <-
      grepl("inverted|bad_country|open_sea", x[, check.names[2]], perl = TRUE)
    check_inv2 <- x[check_these, ]
  } else {
    check_inv2 <- x[FALSE,]
  }

  # binding the two groups of checks and creating the results column
  check_inv <- rbind.data.frame(check_inv1, check_inv2,
                                stringsAsFactors = FALSE)
  check_inv <- check_inv[!duplicated(check_inv$tmp.order),]
  check_inv$check_inv <- NULL

  if (dim(check_inv)[1] == 0) {

    # remove temporary columns
    tmp.cols <- c("tmp.order", "tmp.country.gazet")
    x <- x[, -which(names(x) %in% tmp.cols)]
    return(x)

  } else {
    # create inverted lonlat data
    cols1 <- c("tmp.order", check.names, "tmp.country.gazet", lon, lat)
    cols1 <- cols1[cols1 %in% names(x)]
    tmp <- check_inv[, cols1]
    tmp$inv_lon <- -tmp[, lon]
    tmp$inv_lat <- -tmp[, lat]
    inv_lon <- "inv_lon"
    inv_lat <- "inv_lat"
    #rafl: no codigo antigo eu fazia apenas os casos 1,2,3 e 4. Se me lembro bem, não achei os casos 5 e 6. Mas o 7 deve ter...
    types <- list(invert_lon = c(inv_lon, lat),
               invert_lat = c(lon, inv_lat),
               invert_both = c(inv_lon, inv_lat), #signos
               transposed = c(lat, lon), #transposto
               transp_inv_lon = c(lat, inv_lon),
               transp_inv_lat = c(inv_lat, lon),
               transp_inv_both = c(inv_lat, inv_lon)) #signos e transposto
    trans.data <- vector("list", length(types))
    names(trans.data) <- names(types)

    for(i in 1:length(types))
      trans.data[[i]] <- sf::st_as_sf(tmp, coords = types[[i]])

    check <- dplyr::bind_rows(trans.data, .id = "types")

    #Removing any latitudes above the possible
    bad.lat <- abs(sf::st_coordinates(check)[,2]) > 90
    if (any(bad.lat))
      check <- check[!bad.lat, ]

    ## Overlaying inverted lonlat data with the world map
    check <- sf::st_set_crs(check, sf::st_crs(worldMap))
    check1 <- suppressMessages(
      sf::st_join(check, worldMap, join = sf::st_intersects))
    check1 <- check1[!is.na(check1$NAME_0),]

    if (dim(check1)[1] > 0) {

      check_these <- check1$tmp.country.gazet == check1$NAME_0

      if (any(check_these)) {

        #getting the new coordinates
        new.coords <- sf::st_coordinates(check1)
        colnames(new.coords) <- c(lon, lat)
        new.coords <- new.coords[check_these, ]
        sf::st_geometry(check1) <- NULL

        #editing the validation columns
        cols2 <- c("types", "tmp.order", check.names)
        cols2 <- cols2[cols2 %in% names(check1)]
        check1 <- check1[check_these, cols2]

        if (length(cols2) == 4) {
          check1[, check.names[1]] <- NA_character_
          check1[, check.names[2]] <-
            paste0("ok_country[", check1$types, "]")
        } else {
          check1[, check.names[2]] <-
            paste0("ok_country[", check1$types, "]")
        }
        #combining coords and edited results
        check1 <- cbind.data.frame(check1[, -1], new.coords,
                                   stringsAsFactors = FALSE)

      } else {
        check1 <- NULL
      }
    } else {
      check1 <- NULL
    }

    ## Preparing to return
    if (is.null(check1)) {
      # update classes that were checked
      x[, check.names[2]] <-
        gsub("bad_country[inverted?]", "bad_country", x[, check.names[2]],
             fixed = TRUE)

      # remove temporary columns and returnig
      tmp.cols <- c("tmp.order", "tmp.country.gazet")
      x <- x[, -which(names(x) %in% tmp.cols)]
      return(x)

    } else {

      ## binding main data and checks
      y <- dplyr::left_join(x,
                            check1,
                            by = "tmp.order", suffix = c("", ".new"))
      tmp.cols <- c("tmp.order", "tmp.country.gazet")
      sub.cols <- names(check1)[!names(check1) %in% tmp.cols]
      sub.cols.new <- paste0(sub.cols, ".new")

      if (output == "new.col") {
        sub.rows <- x$tmp.order %in% check1$tmp.order
        x[sub.rows, sub.cols.new] <- y[sub.rows, sub.cols.new]
      }

      if (output == "same.col") {
        sub.rows <- x$tmp.order %in% check1$tmp.order
        x[sub.rows, sub.cols] <- y[sub.rows, sub.cols.new]
      }

      # update classes that were checked
      x[, check.names[2]] <-
        gsub("bad_country[inverted?]", "bad_country", x[, check.names[2]],
             fixed = TRUE)

      # re-ordering, removing temporary columns and returnig
      x <- x[order(x$tmp.order),]
      tmp.cols <- c("tmp.order", "tmp.country.gazet")
      x <- x[, -which(names(x) %in% tmp.cols)]
      return(x)
    }
  }
}
