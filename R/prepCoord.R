#' @title Format Geographical Coordinates
#'
#' @description This function formats geographical coordinates to decimal
#'   degrees
#'
#' @param x a data.frame containing the geographical coordinates
#' @param lat character. The name of the column containing the latitude. Default
#'   to "decimalLatitude".
#' @param lon character. The name of the column containing the longitude.
#'   Default to "decimalLongitude".
#' @param flag logical. Should the differences between the original and edited
#'   coordinates be flagged? Default to TRUE.
#'
#' @return The data frame \code{x} with the edited geographical coordinates, in
#'   decimal degrees. The new columns have the same name followed by the suffix
#'   '.new'.
#'
#' @details This is a simple function that works for standardizing common issues
#'  in geographical coordinates, such as format and spacing. It was designed to
#'  process coordinates already in decimal degrees. So, it does not handle all
#'  formats and may return `NA` even if a coordinates are given and valid.
#'
#' Currently, the function does not differentiate missing coordinates from those
#'  provided in a format that `prepCoord` does not handle (e.g. UTM). Function
#'  should work fine for coordinates in the degrees + decimal minutes format.
#'  See examples below.
#'
#' @author Renato A. F. de Lima
#'
#' @export prepCoord
#'
#' @examples
#'
#' ## Creating some geographical coordinates in different formats
#' coords <- data.frame(
#' decimalLatitude = c(-23.475389, "-23,475389",
#' "23o 28' 31.40\"S", -93.475389, NA, 0, "missing", "blocked", NA,
#' "23\xB0 28.5233'S","283088.52 E","-23,475,389"),
#' decimalLongitude = c(-47.123768, "-47,123768", "47o 07' 25.56\"W",
#' -47.123768, 185.578, 0, "missing","blocked", NA, "47\xBA 07.4260'W",
#' "7402251.30 S","-47,123,768"),
#' stringsAsFactors = FALSE)
#' Encoding(coords[,1]) <- "latin1"
#' Encoding(coords[,2]) <- "latin1"
#' coords
#'
#' ## Formatting the geographical coordinates
#' prepCoord(coords)
#'
#' @export prepCoord
#'
prepCoord <- function(x, lat = "decimalLatitude",
                      lon = "decimalLongitude", flag = TRUE) {

  ## check input
  if (!class(x) == "data.frame")
    stop("input object needs to be a data frame!")

  ## checking and filtering latitude and longitude columns
  if (all(c(lat, lon) %in% names(x))) {
    lati <- as.character(x[, lat])
    long <- as.character(x[, lon])
    # names(x) <- c("lon", "lat")
  } else {
    stop("Coordinate names do not match those of the input object: please rename or specify the correct names")
  }

  # making sure the coordinate columns have all info contained in the verbatim coordinates
  if ("verbatimLatitude" %in% names(x)) {
    lati1 <- x[, "verbatimLatitude"]
    lati[is.na(lati) & !is.na(lati1)] <-
      lati1[is.na(lati) & !is.na(lati1)]
  }
  if ("verbatimLongitude" %in% names(x)) {
    long1 <- x[, "verbatimLongitude"]
    long[is.na(long) & !is.na(long1)] <-
      long1[is.na(long) & !is.na(long1)]
  }

  ## Preliminary editing
    # coordinates without numbers
      no.numb <- !grepl('\\d', lati) | !grepl('\\d', long)
      lati[no.numb] <- long[no.numb] <- NA

    # zero coordinates as missing
      lati[lati %in% 0 & long %in% 0] <- NA
      long[(is.na(lati) | lati %in% 0) & long %in% 0] <- NA

      # one of the coordinates missing as missing
      lati[lati %in% 0 & is.na(long)] <- NA
      long[long %in% 0 & is.na(lati)] <- NA

      # possible problems with decimal division
      lati <- gsub(',', '\\.', lati)
      long <- gsub(',', '\\.', long)

      ## Any coordinate on non-decimal degrees format?
      lati.na <- !is.na(lati)
      lati.na.num <- suppressWarnings(!is.na(as.double(lati)))
      tmp <- sum(lati.na) - sum(lati.na.num)
      # tmp <- suppressWarnings(sum(table(lat)) - sum(table(as.double(lat))))
      if(tmp > 0) {
        ids <- lati.na & !lati.na.num
        lati0 <- lati[ids]
        long0 <- long[ids]

        lati.ref <- grepl("s|S", lati0) & !grepl("n|N", lati0)
        long.ref <- grepl("w|W|o|O", long0) & !grepl("e|E|l|L", long0)
        lati0 <- gsub('\'|\"|\xB0|\xBA|\\*|\\|', " ", lati0)
        long0 <- gsub('\'|\"|\xB0|\xBA|\\*|\\|', " ", long0)
        lati0 <- gsub('[a-z]', " ", lati0, ignore.case = TRUE)
        long0 <- gsub('[a-z]', " ", long0, ignore.case = TRUE)
        lati0 <- gsub('   ', ' ', lati0, perl = TRUE)
        long0 <- gsub('   ', ' ', long0, perl = TRUE)
        lati0 <- gsub('  ', ' ', lati0, perl = TRUE)
        long0 <- gsub('  ', ' ', long0, perl = TRUE)
        lati0 <- stringr::str_trim(lati0)
        long0 <- stringr::str_trim(long0)
        lati0[!is.na(lati0) & lati0 %in% c("0 0 0", "0 0")] <- NA
        long0[!is.na(long0) & long0 %in% c("0 0 0", "0 0")] <- NA
        lati0[!is.na(lati0) & lati0 %in% ""] <- NA
        long0[!is.na(long0) & long0 %in% ""] <- NA
        lati0[grepl("\\.", lati0)] <-
          sub("^([^.]*.[^.]*).", "\\1", lati0[grepl("\\.", lati0)])
        long0[grepl("\\.", long0)] <-
          sub("^([^.]*.[^.]*).", "\\1", long0[grepl("\\.", long0)])
        lati1 <- suppressWarnings(cbind.data.frame(grau = as.double(sapply(strsplit(lati0, " "), function(x) x[1])),
                                                   min = as.double(sapply(strsplit(lati0, " "), function(x) x[2])) / 60,
                                                   sec = as.double(sapply(strsplit(lati0, " "), function(x) x[3])) / 3600))
        lati2 <- apply(lati1, 1, sum, na.rm = TRUE)
        long1 <- suppressWarnings(cbind.data.frame(grau = as.double(sapply(strsplit(long0, " "), function(x) x[1])),
                                                   min = as.double(sapply(strsplit(long0, " "), function(x) x[2])) / 60,
                                                   sec = as.double(sapply(strsplit(long0, " "), function(x) x[3])) / 3600))
        long2 <- apply(long1, 1, sum, na.rm = TRUE)
        lati2[lati.ref] <- -lati2[lati.ref]
        long2[long.ref] <- -long2[long.ref]
        lati2[!is.na(lati2) & lati2 %in% 0] <- NA
        long2[!is.na(long2) & long2 %in% 0] <- NA
        lati[ids] <- as.numeric(lati2)
        long[ids] <- as.numeric(long2)
      }

      ## Removing problematic coordinates still present
      lati <- suppressWarnings(as.numeric(lati))
      long <- suppressWarnings(as.numeric(long))

      long[!is.na(long) & abs(lati) > 90] <- NA
      lati[!is.na(lati) & abs(lati) > 90] <- NA
      lati[!is.na(long) & abs(long) > 180] <- NA
      long[!is.na(long) & abs(long) > 180] <- NA

      ## Preparing the output
      new.cls <- paste(c(lat,lon), ".new", sep = "")
      result <- cbind.data.frame(x, lati, long,
                                 stringsAsFactors = FALSE)
      names(result)[dim(x)[2] + c(1:2)] <- new.cls

      #Flag the coordinates that were changed?
      if (flag){
        result$coord.check <- TRUE
        result$coord.check[!result[,lat] %in% as.character(lati) |
                             !result[,lon] %in% as.character(long)] <- FALSE
        result$coord.check[!is.na(result[,lat]) & is.na(lati) |
                             !is.na(result[,lon]) %in% is.na(long)] <- FALSE
      }
      return(result)
}
