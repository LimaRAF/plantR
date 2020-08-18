#' @title Format Geographical Coordinates
#'
#' @description Formats geographical coordinates to decimal degrees
#'
#' @param x a data.frame containing the geographical coordinates
#'
#' @return The data frame \code{x} with the edited geographical coordinates, in
#'   decimal degrees
#'
#' @details This is a simple function that works for standardizing common issues
#'  in geographical coordinates, such as format and spacing. It was designed to
#'  process coordinates already in decimal degrees, so, it does not handle all
#'  formats and may return `NAs` even if a coordinates is given and valid.
#'
#' Currently, the function does not differentiate missing coordinates from those
#'  provided in a format that `prepCoord` does not handle (e.g. UTM). It also
#'  does not differentiate original coordinates in decimal degrees in a good
#'  format from those that needed to be formatted (i.e. no flagging is returned).
#'  Function should work fine for in the degrees + decimal minutes format. See
#'  examples below.
#'
#' @author Renato A. F. de Lima
#'
#' @export prepCoord
#'
#' @examples
#'
#' ## Creating some geographical coordinates in different formats
#' (coords <- data.frame(decimalLatitude = c(-23.475389, "-23,475389", "23o 28' 31.40\"S", -93.475389,
#'                                           0, "missing", "blocked", NA, "23° 28.5233'S","283088.52 E"),
#'                       decimalLongitude = c(-47.123768, "-47,123768", "47o 07' 25.56\"W", -47.123768,
#'                                           0, "missing", "blocked", NA, "47° 07.4260'W","7402251.30 S")))
#'
#' ## Formatting the geographical coordinates
#' prepCoord(coords)
#'
prepCoord <- function(x) {

  ## check input
  if (!class(x) == "data.frame")
    stop("input object needs to be a data frame!")

  if (sum(grepl("Latitude|Longitude", colnames(x))) < 2)
    stop("input object needs to have at least: decimalLatitude or verbatimLatitude, decimalLongitude or verbatimLongitude")

  ## Filtering the target columns and putting them in the right order
  cols <- c("decimalLatitude", "decimalLongitude",
            "verbatimLatitude", "verbatimLongitude")
  cls <- unique(cols[cols %in% names(x)])
  x1 <- x[, cls]

    # obtaining the columns with coordinates
    lat <- x1[, 1]
    long <- x1[, 2]
    if (dim(x1)[2] > 2) {
      lat1 <- x1[, 3]
      long1 <- x1[, 4]
    }

  ## Preliminary editing

    # making sure the priority columns have all info contained in the
      if(dim(x1)[2] > 2) {

        lat[is.na(lat) & !is.na(lat1)] <- lat1[is.na(lat) & !is.na(lat1)]
        long[is.na(long) & !is.na(long1)] <- long1[is.na(long) & !is.na(long1)]

      }
    # coordinates without numbers
      lat[!grepl('\\d', lat) | !grepl('\\d', long)] <- NA
      long[!grepl('\\d', lat) | !grepl('\\d', long)] <- NA

    # zero coordinates as missing
      lat[lat %in% 0 & long %in% 0] <- NA
      long[(is.na(lat) | lat %in% 0) & long %in% 0] <- NA

    # one of the coordinates missing as missing
      lat[lat %in% 0 & is.na(long)] <- NA
      long[long %in% 0 & is.na(lat)] <- NA

    # blocked coordinates
      lat[lat %in% c("bloqueada", "Bloqueada", "blocked", "Blocked")] <- NA
      long[long %in% c("bloqueada", "Bloqueada", "blocked", "Blocked")] <- NA

    # possible problems with decimal division
      lat <- gsub(',', '\\.', lat)
      long <- gsub(',', '\\.', long)

  ## Any coordinate on non-decimal degrees format?
    tmp <- suppressWarnings(sum(table(lat)) - sum(table(as.double(lat))))
    if(tmp > 0) {

      lat.ref <- grepl("s|S", lat) & !grepl("n|N", lat)
      long.ref <- grepl("w|W|o|O", long) & !grepl("e|E|l|L", long)
      lat <- gsub('\'|\"|°|º|\\*|\\|', " ", lat)
      long <- gsub('\'|\"|°|º|\\*|\\|', " ", long)
      lat <- gsub('[a-z]', " ", lat, ignore.case = TRUE)
      long <- gsub('[a-z]', " ", long, ignore.case = TRUE)
      lat <- gsub('   ', ' ', lat)
      long <- gsub('   ', ' ', long)
      lat <- gsub('  ', ' ', lat)
      long <- gsub('  ', ' ', long)
      lat <- str_trim(lat)
      long <- str_trim(long)
      lat[!is.na(lat) & lat %in% c("0 0 0", "0 0")] <- NA
      long[!is.na(long) & long %in% c("0 0 0", "0 0")] <- NA
      lat[!is.na(lat) & lat %in% ""] <- NA
      long[!is.na(long) & long %in% ""] <- NA
      lat1 <- cbind.data.frame(grau = as.double(sapply(strsplit(lat, " "), function(x) x[1])),
                              min = as.double(sapply(strsplit(lat, " "), function(x) x[2])) / 60,
                              sec = as.double(sapply(strsplit(lat, " "), function(x) x[3])) / 3600)
      lat2 <- round(apply(lat1, 1, sum, na.rm = TRUE), 8)
      long1 <- cbind.data.frame(grau = as.double(sapply(strsplit(long, " "), function(x) x[1])),
                               min = as.double(sapply(strsplit(long, " "), function(x) x[2])) / 60,
                               sec = as.double(sapply(strsplit(long, " "), function(x) x[3])) / 3600)
      long2 <- round(apply(long1, 1, sum, na.rm = TRUE), 8)
      lat2[lat.ref] <- -lat2[lat.ref]
      long2[long.ref] <- -long2[long.ref]
      lat2[!is.na(lat2) & lat2 == 0] <- NA
      long2[!is.na(long2) & long2 == 0] <- NA
      lat2 <- as.numeric(lat2)
      long2 <- as.numeric(long2)

    } else {

      lat2 <- as.numeric(lat)
      long2 <- as.numeric(long)

    }

  ## Removing problematic coordinates still present
    lat2[!is.na(lat2) & abs(lat2) > 90] <- NA
    long2[!is.na(long2) & abs(lat2) > 90] <- NA
    lat2[!is.na(long2) & abs(long2) > 180] <- NA
    long2[!is.na(long2) & abs(long2) > 180] <- NA

  ## Preparing the output
    new.cls <- paste(cls[1:2], ".new", sep = "")
    result <- cbind.data.frame(x, lat2, long2,
                               stringsAsFactors = FALSE)
    names(result)[dim(x)[2] + c(1:2)] <- new.cls
    return(result)
}
