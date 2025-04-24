#' @title Check Geographical Coordinates
#'
#' @description This function makes the check of the coordinates
#'   against the world and Latin-American maps. Optionally, it returns
#'   the distance between the original coordinates and those from a
#'   gazetteer for coordinates not validated at the county level.
#'
#' @param x a data frame with the species records and their
#'   coordinates in decimal degrees.
#' @param lon Name of the column with the longitude to be validated.
#'   Default to 'decimalLongitude.new'
#' @param lat Name of the column with the latitude to be validated.
#'   Default to 'decimalLatitude.new'
#' @param str.name Column with the verified locality search string
#' @param orig.coord Column with the origin of the coordinates
#'   (typically the output of function `getCoord()`)
#' @param low.map a sf multipolygon object containing the global
#'   administrative map at the lowest level (e.g. country). The
#'   default is "plantR", the default map obtained from
#'   [GADM]{https://gadm.org} (see `worldMap`).
#' @param high.map a sf multipolygon object or a list of sf objects
#'   containing the regional map at the highest administrative level
#'   (e.g. municipality). The default is "plantR", the map for all
#'   Latin American countries and dependent territories  obtained
#'   [GADM]{https://gadm.org} (see `latamMap`).
#' @param res.gazet Column with the locality resolution level
#'   retrieved from the gazetteer
#' @param dist.center Logical. Should the distance (in meters) between
#'   the original coordinates and those retrieved in the gazetteer be
#'   returned? Defaults to FALSE.
#' @param lon.gazet Column with the longitude obtained from a
#'   gazetteer
#' @param lat.gazet Column with the latitude obtained from a gazetteer
#' @param keep.cols character. Name of columns that should be kept in
#'   the output.
#'
#' @importFrom dplyr select one_of rename mutate if_else filter ends_with
#' @importFrom tidyr separate
#' @importFrom sf st_crs st_as_sf st_join st_intersects st_set_crs
#' @importFrom spatialrisk haversine
#'
#' @author Andrea Sánchez-Tapia, Sara Mortara & Renato A. F. de Lima
#'
#' @details
#'
#' By default the function returns only the geographical validation
#' column ('geo.check') and the distance between the original
#' coordinates and those from a gazetteer, if `dist.center` is TRUE.
#' Other columns available for the output that may be relevant are:
#' - the name administrative levels obtained from the maps: 'NAME_0', 'NAME_1',
#'  'NAME_2' and 'NAME_3';
#' - the locality string in the __plantR__ format combining the locality
#' obtained from the maps: 'loc.coord';
#' - the checks between the locality info obtained from `str.name` and the
#' administrative levels from the maps: 'country.check', 'state.check' and
#' 'county.check'.
#'
#' By default, a global map and a regional for Latin America are used
#' in the validation of the geographical coordinates. But, different
#' maps than the __plantR__ defaults can be used. These maps must be
#' provided using the arguments `low.map` and `high.map`. Ideally,
#' these maps which should have the same format of the locality
#' information in the gazetteer used for the validation of the
#' locality information (see function `getLoc()` and the default
#' __plantR__ maps 'worldMap' and 'latamMap').
#'
#' @encoding UTF-8
#'
#' @export checkCoord
#'
checkCoord <- function(x,
                       lon = "decimalLongitude.new",
                       lat = "decimalLatitude.new",
                       str.name = "loc.correct",
                       orig.coord = "origin.coord",
                       low.map = "plantR",
                       high.map = "plantR",
                       res.gazet = "resolution.gazetteer",
                       dist.center = FALSE,
                       lon.gazet = "longitude.gazetteer",
                       lat.gazet = "latitude.gazetteer",
                       keep.cols = c("geo.check", "distCentroid_m")) {

  #Escaping R CMD check notes from using dplyr syntax
  NAME_0 <- pais_latam <- pais_wo <- NULL
  #Escaping R CMD check notes
  worldMap <- worldMap
  latamMap <- latamMap

  ## check input
  if (!inherits(x, "data.frame"))
    stop("Input object needs to be a data frame!")

  if (dim(x)[1] == 0)
    stop("Input data frame is empty!")

  if (!all(c(lat, lon, str.name, orig.coord) %in% colnames(x)))
    stop("One or more column names declared do not match those of the input object: please rename or specify the correct names")

  if (dist.center)
    if (any(!c(lon.gazet, lat.gazet) %in% names(x)))
      stop("If 'dist.center' is TRUE, the input must contain the longitude/latitude obtained from a gazetteer: please rename or specify the correct names")

  ##Preliminary edits
  cols.x <- names(x) # original data column names
  x$tmp.order <- 1:nrow(x)
  x[, str.name][x[, str.name] %in% "no_loc"] <- NA #porque nao eh pais (rafl: concordo, mas nao achei nehuma funcao onde esse 'no_loc' eh gerado; melhor alterar direto na funcao que obtem o string, getLoc()?)

  ##Defining the country, state and county columns
  x <- tidyr::separate(
    data = x,
    col = str.name,
    sep = "_",
    into = c("country.gazet", "state.gazet", "county.gazet",
             "locality.gazet", "sublocality.gazet"),
    remove = FALSE,
    fill = "right"
  )

  ##Creating the geo.check column and assigning the gazetteer and missing classes
  geo.check <- rep(NA_character_, dim(x)[1])

  ids.gazet <- x[, orig.coord] %in% "coord_gazet"
  if (any(ids.gazet))
    geo.check[ids.gazet] <-
      paste0("ok_", x[ids.gazet, res.gazet], "_gazet")

  ids.no.coord <- x[, orig.coord] %in% "no_coord"
  if (any(ids.no.coord))
    geo.check[ids.no.coord] <- "no_cannot_check"

  ids.nas.coord <- x[, orig.coord] %in% "coord_original" &
                    (is.na(x[lon]) | is.na(x[lat]))
  if (any(ids.nas.coord))
    geo.check[ids.nas.coord] <- "no_cannot_check"

  ## Subsetting data for geographical checking
  tmp <- x[is.na(geo.check), ]

  ##Getting data frame with spatial coordinates and standardizing the projection
  tmp <- sf::st_as_sf(tmp, coords = c(lon, lat))
  # set spatial coordinates
  prj <- sf::st_crs(4326)
  tmp <- sf::st_set_crs(tmp, prj)

  ##Getting the global map
  class.low.map <- class(low.map)[1]
  low_map <- NULL
  if (class.low.map == "character") {
    if (any(low.map %in% c("plantR", "plantr"))) {
      low_map <- worldMap
    } else {
      stop("Please chose between the default map or a user-provided map")
    }
  }

  if (class.low.map == "list") {
    if (all(sapply(low.map, function(x) class(x)[1]) %in% "sf")) {
      if(!"NAME_0" %in% names(low.map[[1]]))
        stop("The sf objects must have a column 'NAME_0': the lowest administrative level")
      low_map <- dplyr::bind_rows(low.map)
    } else {
      stop("The user-provided map must be an sf object or a list of sf objects")
    }
  }

  if (class.low.map == "sf") {
    if(!"NAME_0" %in% names(low.map))
      stop("The global map must have a column 'NAME_0': the lowest administrative level")
    low_map <- low.map
  }

  if(is.null(low_map))
    stop("The user-provided map must be an sf object or a list of sf objects")

  ##Crossing the coordinates with the global map
  tmp <- suppressMessages(sf::st_join(tmp,
                                      low_map,
                                      join = sf::st_intersects))
  names(tmp)[which(names(tmp) == "NAME_0")] <- "pais_wo"

  ##Solving misterious problems with the country map (could not isolate the problem)
  check_these <- grepl("\\.[0-9]", rownames(tmp))
  if (any(check_these)){
    tmp$keep_these <- rep(TRUE, dim(tmp)[1])
    dup.orders <- tmp$tmp.order[check_these]
    for(i in seq_along(dup.orders)) {
      dups.i <- tmp[tmp$tmp.order %in% dup.orders[i], ]
      dups.i$keep_these[dups.i$country.new != dups.i$pais_wo] <- FALSE
      if (all(dups.i$keep_these))
        dups.i$keep_these[-1] <- FALSE
      tmp$keep_these[tmp$tmp.order %in% dup.orders[i]] <- dups.i$keep_these
    }
    tmp <- tmp[tmp$keep_these, ]
  }

  ##Defining which coordinates fall into the sea (i.e. original coordinates but no country, state or county)
  geo.check[is.na(geo.check)][is.na(tmp$pais_wo)] <- "sea"

  ##Getting the high-resolution map
  class.high.map <- class(high.map)[1]
  high_map <- NULL
  if (class.high.map == "character") {
    if (any(high.map %in% c("plantR", "plantr"))) {
      high_map <- dplyr::bind_rows(latamMap)
    } else {
      stop("Please chose between the default map or a user-provided map")
    }
  }

  if (class.high.map == "list") {
    if (all(sapply(high.map, function(x) class(x)[1]) %in% "sf")) {
      if (!all(c("NAME_0", "NAME_1") %in% names(high.map[[1]])))
        stop("Ideally, the high resolution map should have the columns 'NAME_0', 'NAME_1', 'NAME_2' and 'NAME_3'")
      high_map <- dplyr::bind_rows(high.map)
    } else {
      stop("The user-provided map must be an sf object or a list of sf objects")
    }
  }

  if (class.high.map == "sf") {
    if (!all(c("NAME_0", "NAME_1") %in% names(high.map[[1]])))
      stop("Ideally, the high resolution map should have the columns 'NAME_0', 'NAME_1', 'NAME_2' and 'NAME_3'")
    high_map <- high.map
  }

  if (is.null(high_map))
    stop("The user-provided map must be an sf object or a list of sf objects")

  ##Comparing the spatial data frame with the selected country shapefiles
  x2 <- suppressMessages(
          sf::st_join(tmp, high_map, join = sf::st_intersects))

  ##Solving misterious problems with the map (could not isolate the problem)
  check_these <- grepl("\\.[0-9]", rownames(x2))
  if (any(check_these)){
    x2$keep_these <- rep(TRUE, dim(x2)[1])
    dup.orders <- x2$tmp.order[check_these]
    for(i in seq_along(dup.orders)) {
      dups.i <- x2[x2$tmp.order %in% dup.orders[i], ]
      dups.i$keep_these[dups.i$country.new != dups.i$pais_wo] <- FALSE
      if (all(dups.i$keep_these))
        dups.i$keep_these[-1] <- FALSE
      x2$keep_these[x2$tmp.order %in% dup.orders[i]] <- dups.i$keep_these
    }
    x2 <- x2[x2$keep_these, ]
  }

  x2 <- dplyr::rename(x2,
                      pais_latam = NAME_0
                      #estado = NAME_1,
                      #municipio = NAME_2
                      #localidade = NAME_3
                      #vai ter um NAME_4 e talvez mais
  )
  #checa diferencas paises e preenche com latam se faltar no mundo
  x2 <- dplyr::mutate(x2, NAME_0 = dplyr::if_else(is.na(pais_wo) &
                                                    !is.na(pais_latam), pais_latam, pais_wo))
  # cria o vetor para checar
  x2$loc.coord <- paste(x2$NAME_0, x2$NAME_1, x2$NAME_2, sep = "_")
  x2$loc.coord[x2$loc.coord %in% "NA_NA_NA"] <- NA_character_
  x2$loc.coord <- gsub("_NA_NA$", "", x2$loc.coord, perl = TRUE)
  x2$loc.coord <- gsub("_NA$", "", x2$loc.coord, perl = TRUE)

  # recupera todas as linhas
  x3 <- suppressMessages(
    dplyr::left_join(x,
                     x2[,c("tmp.order",
                           "NAME_0", "NAME_1", "NAME_2", "NAME_3",
                           "loc.coord")]))

  ### GEO-VALIDATION STEPS ###
  ##1- Validating the coordinates at different levels - exact matches
  #1.1 Country-level: good country? All countries
  x3$country.check <- dplyr::if_else(x3$country.gazet == x3$NAME_0,
                                     "ok_country", "bad_country",
                                     missing = "no_country")

  #1.2 State-level: good state? All countries
  x3$state.check <- dplyr::if_else(x3$state.gazet == x3$NAME_1,
                                   "ok_state", "bad_state",
                                   missing = "no_state")

  #1.3 County-level. All countries
  x3$county.check <- dplyr::if_else(x3$county.gazet == x3$NAME_2,
                                    "ok_county", "bad_county",
                                    missing = "no_county")

  ## Updating geo.check
  tmp1 <- apply(x3[ , c("country.check",
                        "state.check",
                        "county.check")], 1, paste, collapse="/")
  geo.check[is.na(geo.check)] <- tmp1[is.na(geo.check)]

  ## Simplifying geo.check
  repl.check <- simpGeoCheck
  geo.check <- stringr::str_replace_all(geo.check, repl.check)

  ## Calculating the distance between the original and the gazetter coordinates
  if (dist.center) {
    x3$distCentroid_m <- NA
    if (dim(tmp)[1] > 0) {
      ids.dist <- !grepl("ok_county|ok_locality|no_cannot_check", x3$geo.check) &
        !(is.na(x3[, lon.gazet]) | is.na(x3[, lat.gazet]))
      tmp2 <- x3[ids.dist, c(lat, lon, lat.gazet, lon.gazet, "distCentroid_m"), ]
      tmp2$distCentroid_m <- # 0.4s for ~2 million (1s using fields::rdist.earth.vec)
        spatialrisk::haversine(tmp2[, 1], tmp2[, 2], tmp2[, 3], tmp2[, 4])
      x3$distCentroid_m[ids.dist] <-
        tmp2$distCentroid_m
    }
  }

  ## Preparing to return
  #re-ordering (for safety), adding geo.check and removing unecessary columns from the tmp object
  x3 <- x3[order(x3$tmp.order), ]
  x3$geo.check <- geo.check
  x3 <- x3[, -which(names(x3) %in% c("tmp.order"))]

  #defining the new columns from the tmp object to be returned
  new.cols <- names(x3)[!names(x3) %in% cols.x]
  if (!is.null(keep.cols))
    new.cols <- new.cols[new.cols %in% keep.cols]

  #removing unecessary column from the original data frame and returning
  tmp.cols <- c("tmp.order", "country.gazet", "state.gazet", "county.gazet",
                "locality.gazet", "sublocality.gazet")
  x <- x[, -which(names(x) %in% tmp.cols)]
  if (length(new.cols) == 0) {
    return(x)
  } else {
    x4 <- x3[, which(names(x3) %in% new.cols), drop = FALSE]
    return(cbind.data.frame(x, x4))
  }
}

