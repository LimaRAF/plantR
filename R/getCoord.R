#' @title Get Working Geographical Coordinates
#'
#' @description This function assess the availability and resolution of
#'   the original geographical coordinates of the occurrence and it replaces
#'   missing coordinates by the coordinates obtained from the gazetteer.
#'
#' @param x a data.frame containing the original coordinates and the coordinates
#'   obtained from a gazetteer.
#'
#' @return The data frame \code{x} with the columns 'decimalLatitude.new' and
#'   'decimalLongitude.new', which contain the geographical coordinates of the
#'   gazetteer at the best resolution (i.e. administrative level) available.
#'
#' @details The function was initially designed as part of a larger routine to
#'   edit and validate geographical coordinates of plant occurrence data. It is
#'   possible to use it separately, but it may be easier to use it under the
#'   workflow presented in `plantR` manual. If used separately, users must
#'   provide a data frame with at least six columns ('decimalLatitude',
#'   'decimalLongitude', 'latitude.gazetteer', 'longitude.gazetteer',
#'   'resolution.gazetteer', 'decimalLatitude.new' and 'decimalLongitude.new).
#'   See the examples below.
#'
#'   (Explain how the resolution of the original coordinate is obtained)
#'
#' @author Renato A. F. de Lima
#'
#' @export getCoord
#'
#' @examples
#'
#' ## To be included ##
#'
#' (coords <- data.frame(decimalLatitude = c(-23.475389, NA),
#'                      decimalLongitude = c(-47.123768, NA),
#'                      latitude.gazetteer = c(-23.524052, -23.524052),
#'                      longitude.gazetteer = c(-47.122207, -47.122207),
#'                      resolution.gazetteer = c("county","county"),
#'                      decimalLatitude.new = c(-23.475389, NA),
#'                      decimalLongitude.new = c(-47.123768, NA)
#'                      ))
#'
#' getCoord(coords)
#'
getCoord <- function(x) {

  ## check input
  if (!class(x) == "data.frame")
    stop("input object needs to be a data frame!")

  if (sum(grepl("decimalLatitude|decimalLongitude|\\.gazetteer$", colnames(x))) < 7) {
                stop("input object needs to have the following columns:
                  decimalLatitude, decimalLongitude,
                  latitude.gazetteer, longitude.gazetteer, resolution.gazetteer,
                  decimalLatitude.new, decimalLongitude.new") }

  ## Filtering the target columns and putting them in the right order
    cols <- c("decimalLatitude", "decimalLongitude",
            "latitude.gazetteer", "longitude.gazetteer", "resolution.gazetteer",
            "decimalLatitude.new", "decimalLongitude.new")
    cls <- unique(cols[cols %in% names(x)])
    x1 <- x[, cls]

    # obtaining the columns with coordinates
    lat <- x1[, "decimalLatitude.new"]
    long <- x1[, "decimalLongitude.new"]

  ## Creating the working coodinates and defining the origin of the information
    if (all(is.na(lat)) | all(is.na(long))) {

      x1$origin.coord <- NA
      x1$resolution.coord <- NA

    }	else {

      x1$origin.coord <- NA
      x1$origin.coord[!is.na(lat)] <- "coord_original"

    # Assessing the resolution of the original coordinates
      x1$resolution.coord <- NA
      lat3 <- lat[!is.na(lat)]
      long3 <- long[!is.na(long)]
      # no minutes
      tmp1 <- strsplit(as.character(lat3), "\\.")
      lat3[sapply(tmp1, length) == 1] <- "degrees_only"
      tmp2 <- strsplit(as.character(long3), "\\.")
      long3[sapply(tmp2, length) == 1] <- "degrees_only"
      # no seconds
      mins <-
        sapply(strsplit(as.character(round(1:59 / 60, 6)), "\\."), function(x) x[2])
      mins1 <- c(sapply(strsplit(as.character(round(1:59 / 60, 6)), "\\."), function(x) x[2]),
                sapply(strsplit(as.character(round(1:59 / 60, 4)), "\\."), function(x) x[2]),
                sapply(strsplit(as.character(round(1:59 / 60, 2)), "\\."), function(x) x[2]))
      lat3[!lat3 %in% "degrees_only" &
             sapply(tmp1, function(x)
               x[2]) %in% c(mins1, 0)] <- "minutes_only"
      long3[!long3 %in% "degrees_only" &
              sapply(tmp2, function(x)
                x[2]) %in% c(mins1, 0)] <- "minutes_only"
      # seconds
      mins <- as.double(mins) / 10 ^ (nchar(mins))
      secs <- as.character(apply(cbind.data.frame(as.double(rep( c(mins, 0), each = 60)),
        as.double(rep( round(1:60 / 3600, 6), 60 ))), 1, sum))
      secs <- sapply(strsplit(secs, "\\."), function(x) x[2])
      secs <- secs[!is.na(secs)]
      lat3[!lat3 %in% c("degrees_only", "minutes_only") &
             sapply(tmp1, function(x)
               x[2]) %in% secs] <- "seconds"
      long3[!lat3 %in% c("degrees_only", "minutes_only") &
              sapply(tmp2, function(x)
                x[2]) %in% secs] <- "seconds"
      # seconds with rounding issues?
      lat3[!lat3 %in% c("degrees_only", "minutes_only", "seconds") &
             nchar(sapply(tmp1, function(x)
               x[2])) >= 6] <- "seconds?"
      long3[!long3 %in% c("degrees_only", "minutes_only", "seconds") &
              nchar(sapply(tmp2, function(x)
                x[2])) >= 6] <- "seconds?"

    # Saving the resolution
      tmp <- x1$resolution.coord[!is.na(lat)]
      tmp[lat3 %in% "degrees_only" & long3 %in% "degrees_only"] <- "degrees_only"
      tmp[lat3 %in% "minutes_only" & long3 %in% "minutes_only"] <- "minutes_only"
      tmp[lat3 %in% "seconds" & long3 %in% "seconds"] <- "seconds"
      tmp[lat3 %in% "degrees_only" & long3 %in% "minutes_only"] <- "minutes_only"
      tmp[long3 %in% "degrees_only" & lat3 %in% "minutes_only"] <- "minutes_only"
      tmp[lat3 %in% "degrees_only" & long3 %in% "seconds"] <- "seconds"
      tmp[long3 %in% "degrees_only" & lat3 %in% "seconds"] <- "seconds"
      tmp[lat3 %in% "minutes_only" & long3 %in% "seconds"] <- "seconds"
      tmp[long3 %in% "minutes_only" & lat3 %in% "seconds"] <- "seconds"
      tmp[lat3 %in% "seconds" & long3 %in% "seconds?"] <- "seconds"
      tmp[long3 %in% "seconds" & lat3 %in% "seconds?"] <- "seconds"
      tmp[lat3 %in% "seconds?" & long3 %in% "seconds?"] <- "seconds?"
      tmp[long3 %in% "seconds?" & lat3 %in% "seconds?"] <- "seconds?"
      x1$resolution.coord[!is.na(lat)] <- tmp

    }

    ##Replacing missing coordinates by the county coordinates from the gazetter
    x1$decimalLatitude.new[is.na(lat)] <-
      x1$latitude.gazetteer[is.na(lat)]
    x1$decimalLongitude.new[is.na(long)] <-
      x1$longitude.gazetteer[is.na(long)]
    x1$origin.coord[!is.na(x1$decimalLatitude.new) &
                      is.na(x1$origin.coord)] <- "coord_gazet"
    x1$origin.coord[is.na(x1$origin.coord)] <- "no_coord"
    x1$resolution.coord[x1$origin.coord %in% "coord_gazet" &
                          x1$resolution.gazetteer %in% c("country", "state", "county")] <- "seconds_centroid"
    x1$resolution.coord[x1$origin.coord %in% "coord_gazet" &
                          x1$resolution.gazetteer %in% c("locality")] <- "seconds"
    # x1$origin.coord[x1$origin.coord %in% "coord_gazet" &
    #                   x1$resolution.gazetteer %in% "country"] <- "ok_country_gazet"
    # x1$origin.coord[x1$origin.coord %in% "coord_gazet" &
    #                   x1$resolution.gazetteer %in% "state"] <- "ok_state_gazet"
    # x1$origin.coord[x1$origin.coord %in% "coord_gazet" &
    #                   x1$resolution.gazetteer %in% "county"] <- "ok_county_gazet"
    # x1$origin.coord[x1$origin.coord %in% "coord_gazet" &
    #                   x1$resolution.gazetteer %in% "locality"] <- "ok_locality_gazet"

    ## Preparing the output
    x2 <- x[,!(names(x) %in% c("decimalLatitude.new", "decimalLongitude.new"))]
    x3 <- cbind.data.frame(x2, x1[, c("decimalLatitude.new",
                                      "decimalLongitude.new",
                                      "origin.coord",
                                      "resolution.coord"), ])
    return(x3)
}
