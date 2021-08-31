#' @title Get Working Geographical Coordinates
#'
#' @description This function assess the availability and precision of
#'   the original geographical coordinates of the occurrence and it replaces
#'   missing coordinates by the coordinates obtained from the gazetteer.
#'
#' @param x a data.frame containing the original coordinates and the coordinates
#'   obtained from a gazetteer.
#' @param lat.orig character. The name of the column containing the latitude.
#'   Default to "decimalLatitude".
#' @param lon.orig character. The name of the column containing the longitude.
#'   Default to "decimalLongitude".
#' @param lat.gazet character. The name of the column containing the latitude
#'   from the gazetteer. Default to "latitude.gazetteer".
#' @param lon.gazet character. The name of the column containing the longitude
#'   from the gazetteer. Default to "longitude.gazetteer".
#' @param res.gazet character. The name of the column containing the resolution
#'   of the locality information from which the gazetteer coordinates were
#'   obtained.
#' @param lat.new character. The name of the column containing the edited
#'   latitude, combining the coordinates from the source data and the gazetteer.
#'   Default to "decimalLatitude.new".
#' @param lon.new character. The name of the column containing the working
#'   longitude combining the coordinates from the source data and the gazetteer.
#'   Default to "decimalLongitude.new".
#' @param rm.gazet logical. Should the coordinates form the gazetterr be removed
#'   from the data after the replacement of missing coordinates? Default to
#'   FALSE.
#'
#' @return The data frame \code{x} with the columns 'decimalLatitude.new' and
#'   'decimalLongitude.new' containing the geographical coordinates at the best
#'   locality resolution (i.e. 'administrative level') available. The function
#'   also returns the precision of the coordinates.
#'
#' @details The function was initially designed as part of a routine to edit and
#'   validate geographical coordinates of plant occurrence data. It is possible
#'   to use it separately, but it may be easier to use it under the workflow
#'   presented in __plantR__ tutorial (see functions `getLoc` and `prepCoord`).
#'   If used separately, users should provide a data frame preferabley with the
#'   following six columns (the defaults): 'decimalLatitude',
#'   'decimalLongitude', 'latitude.gazetteer', 'longitude.gazetteer',
#'   'resolution.gazetteer', 'decimalLatitude.new' and 'decimalLongitude.new')
#'   or to specify the respective columns in the input data.
#'
#'   The columns `lat.new` and `lon.new` are the columns containing the working
#'   coordinates that may have been already edited by `prepCoord()`. It is also
#'   the columns were the function will store the coordinates from the gazetter
#'   in the case of missing coordinates. If these columns are not provided in
#'   the input data, they are assumed to be equal to `lat.orig` and `lon.orig`
#'   and they are created internally.
#'
#'   The precision of the geographical coordinates are classified as: "degrees",
#'   "minutes", "seconds" and "miliseconds". This classification is performed
#'   for the latitude and longitude of each record and the worst precision
#'   retrieved is assigned to both of them (i.e. if lat = "minutes" and lon=
#'   "seconds", the precision = "minutes"). For simplicity, all
#'   latitude/longitude not classified as "degrees", "minutes" or "seconds" are
#'   classified as "miliseconds".
#'
#'   The function implicitly assumes that NA coordinates are missing and they
#'   are replaced by the coordinates coming from the gazetteer. See the examples
#'   below.
#'
#'
#' @seealso
#'  \link[plantR]{getLoc} and \link[plantR]{prepCoord}.
#'
#' @author Renato A. F. de Lima
#'
#' @export getCoord
#'
#' @examples
#'
#' (coords <- data.frame(decimalLatitude = c(-23.5, -23.475389, NA),
#'                      decimalLongitude = c(-47.166667, -47.123768, NA),
#'                      latitude.gazetteer = c(-23.524052, -23.524052, -23.524052),
#'                      longitude.gazetteer = c(-47.122207, -47.122207, -47.122207),
#'                      resolution.gazetteer = c("county", "county", "county"),
#'                      decimalLatitude.new = c(-23.5, -23.475389, NA),
#'                      decimalLongitude.new = c(-47.166667,-47.123768, NA)
#'                      ))
#'
#' getCoord(coords)
#'
getCoord <- function(x, lat.orig = "decimalLatitude", lon.orig = "decimalLongitude",
                     lat.gazet = "latitude.gazetteer", lon.gazet = "longitude.gazetteer",
                     res.gazet = "resolution.gazetteer", lat.new = "decimalLatitude.new",
                     lon.new = "decimalLongitude.new", rm.gazet = FALSE) {

  ## check input
  if (!class(x) == "data.frame")
    stop("input object needs to be a data frame!")

  ## Filtering the target columns and putting them in the right order
  cols <- c(lat.orig, lon.orig,
            lat.gazet, lon.gazet,
            res.gazet)
  if (any(!cols %in% names(x)))
    stop("One or more column names were not found: please check or specify column names")

  cols <- c(cols, lat.new, lon.new)
  cls <- unique(cols[cols %in% names(x)])
  x1 <- x[, cls]

  # Checking the presence of the working coordinates
  if (!lat.new %in% names(x1))
    x1[, lat.new] <- x1[, lat.orig]

  if (!lon.new %in% names(x1))
    x1[, lon.new] <- x1[, lon.orig]

  # obtaining the columns with the working coordinates
  lati <- x1[, lat.new]
  long <- x1[, lon.new]

  ## Updating the working coodinates and defining the origin of the information
    if (all(is.na(lati)) | all(is.na(long))) {

      x1$origin.coord <- NA
      x1$resolution.coord <- NA

    }	else {

      x1$origin.coord <- NA
      x1$origin.coord[!is.na(lati)] <- "coord_original"

    # Assessing the precision of the original coordinates
      lati3 <- lati[!is.na(lati)]
      long3 <- long[!is.na(long)]
      tmp1 <- sapply(
        strsplit(as.character(lati3), "\\."),
          function(x) x[2])
      tmp2 <- sapply(
        strsplit(as.character(long3), "\\."),
        function(x) x[2])

      # no minutes
      lati3[tmp1 %in% "000000"] <- "degrees_only"
      long3[tmp2 %in% "000000"] <- "degrees_only"

      # no seconds
      mins <- sapply(
        strsplit(format(round(1:59 / 60, 6), nsmall = 6), "\\."),
            function(x) x[2])
      lati3[!lati3 %in% "degrees_only" &
              tmp1 %in% mins] <- "minutes_only"
      long3[!long3 %in% "degrees_only" &
              tmp2 %in% mins] <- "minutes_only"

      # seconds and seconds with rounding issues
      mins <- as.double(mins) / 10 ^ (nchar(mins))
      mins1 <- sapply(
        strsplit(format(round(1:59 / 60, 7), nsmall = 6), "\\."),
        function(x) x[2])
      mins1 <- as.double(mins1) / 10 ^ (nchar(mins1))
      df.secs <- cbind.data.frame(as.double(rep( c(mins, 0), each = 60)),
                       as.double(rep( round(1:60 / 3600, 6), 60 )))
      df.secs1 <- cbind.data.frame(as.double(rep( c(mins1, 0), each = 60)),
                                  as.double(rep( round(1:60 / 3600, 7), 60 )))
      secs <- unique(c(format(apply(df.secs, 1, sum), nsmall = 6),
                       substr(format(apply(df.secs1, 1, sum), nsmall = 7), 1, 8)))
      secs <- substr(secs, 3, 8)
      secs <- secs[!is.na(secs)]
      lati3[!lati3 %in% c("degrees_only", "minutes_only") &
              tmp1 %in% secs] <- "seconds"
      long3[!long3 %in% c("degrees_only", "minutes_only") &
              tmp2 %in% secs] <- "seconds"

      # miliseconds
      lati3[!lati3 %in% c("degrees_only", "minutes_only", "seconds")] <-
        "miliseconds"
      long3[!long3 %in% c("degrees_only", "minutes_only", "seconds")] <-
        "miliseconds"

      # Saving/combining the best precision for both lat and lon
      x1$precision.coord <- NA
      x1[,c("p.lat", "p.lon")] <- NA
      x1[!is.na(lati),"p.lat"] <- lati3
      x1[!is.na(long),"p.lon"] <- long3

      x1$precision.coord[x1$p.lat %in% "degrees_only" &
                           x1$p.lon %in% "degrees_only"] <- "degrees"
      x1$precision.coord[x1$p.lat %in% "minutes_only" &
                           x1$p.lon %in% "minutes_only"] <- "minutes"
      x1$precision.coord[x1$p.lat %in% "seconds" &
                           x1$p.lon %in% "seconds"] <- "seconds"
      x1$precision.coord[x1$p.lat %in% "miliseconds" &
                           x1$p.lon %in% "miliseconds"] <- "miliseconds"
      x1$precision.coord[is.na(x1$precision.coord) & !is.na(x1$p.lat) & !is.na(x1$p.lon) &
                           (x1$p.lat %in% "degrees_only" | x1$p.lon %in% "degrees_only")] <- "degrees"
      x1$precision.coord[is.na(x1$precision.coord) & !is.na(x1$p.lat) & !is.na(x1$p.lon) &
                           (x1$p.lat %in% "minutes_only" | x1$p.lon %in% "minutes_only")] <- "minutes"
      x1$precision.coord[is.na(x1$precision.coord) & !is.na(x1$p.lat) & !is.na(x1$p.lon) &
                           (x1$p.lat %in% "seconds" | x1$p.lon %in% "seconds")] <- "seconds"
      x1$precision.coord[is.na(x1$precision.coord) & !is.na(x1$p.lat) & !is.na(x1$p.lon) &
                           (x1$p.lat %in% "miliseconds" | x1$p.lon %in% "miliseconds")] <- "miliseconds"
    }

  ##Replacing missing coordinates by the county coordinates from the gazetter
  x1$decimalLatitude.new[is.na(lati) | is.na(long)] <-
    x1$latitude.gazetteer[is.na(lati) | is.na(long)]
  x1$decimalLongitude.new[is.na(lati) | is.na(long)] <-
    x1$longitude.gazetteer[is.na(lati) | is.na(long)]
  x1$origin.coord[!is.na(x1$decimalLatitude.new) &
                    is.na(x1$origin.coord)] <- "coord_gazet"
  x1$origin.coord[is.na(x1$origin.coord)] <- "no_coord"
  x1$precision.coord[x1$origin.coord %in% "coord_gazet" &
                       x1$resolution.gazetteer %in% c("country", "state", "county")] <- "seconds_centroid"
  x1$precision.coord[x1$origin.coord %in% "coord_gazet" &
                       x1$resolution.gazetteer %in% c("locality")] <- "seconds"

  ## Preparing the output
  x2 <- x[,!(names(x) %in% c(lat.new, lon.new))]
  if (rm.gazet)
    x2 <- x2[,!(names(x2) %in% c(lat.gazet, lon.gazet))]
  x3 <- cbind.data.frame(x2, x1[, c("decimalLatitude.new",
                                    "decimalLongitude.new",
                                    "origin.coord",
                                    "precision.coord"), ])
  return(x3)
}
