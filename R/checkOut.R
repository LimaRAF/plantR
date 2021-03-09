#' @title Search For Spatial Outliers
#'
#' @description This function searches for spatial outliers, i.e. records too
#'   far away from species core distributions based
#'   on \href{https://en.wikipedia.org/wiki/Mahalanobis_distance}{Mahalanobis
#'   distances}. Spatial outliers can indicate misidentifications or records
#'   obtained from cultivated individuals.
#'
#' @param x a data frame with the species records.
#' @param lon character. Column with the record longitude in decimal degrees.
#' Default to 'decimalLongitude.new'.
#' @param lat character. Column with the record latitude in decimal degrees.
#' Default to 'decimalLatitude.new'
#' @param tax.name character. Name of the columns containing the species name.
#'   Default to "scientificName.new"
#' @param geo.name character. Name of the column containing the validation of
#'   the geographical coodinates. Default to "geo.check"
#' @param cult.name character. Name of the column containing the validation of
#'   records from cultiavted individuals. Default to "cult.check"
#' @param clas.cut numerical. The threshold distance for outlier detection, using
#' classic Mahalanobis distances. Default to 3
#' @param rob.cut numerical. The threshold distance for outlier detection, using
#' classic Mahalanobis distances. Default to 16
#'
#' @inheritParams mahalanobisDist
#'
#' @author Renato A. F. de Lima
#'
#' @return The input data frame with a new column containing the indication of
#'   spatial outliers.
#'
#' @details The function searches for spatial outliers using two different
#'   methods to detect outliers (Liu et al., 2018): the classic and the robust
#'   squared Mahalanobis distances (see help of `mahalanobisDist()` for details).
#'   They can be used separetely or combined (See Examples).
#'
#'   To detect outliers, we need thresholds to be applied to the values of
#'   Mahalanobis distances obtained for each species (arguments `clas.cut` and
#'   `rob.cut`). Ideally these thresholds should be species-specific, but this
#'   is not always possible. Based on the empirical distribution of some
#'   Atlantic Forest species with very different number of occurrences and
#'   spatial distribution patterns, Lima et al. (2020) noted that occurrences
#'   outside the species ranges often had classic and robust Mahalanobis
#'   distances above 3 and 16 (used here as defaults). For cultivated species,
#'   they used more restrictive thresholds of 2.5 and 12, respectively. They
#'   also mentioned that these thresholds are very conservative (i.e. only more
#'   extreme outliers are removed) and so some outliers may remain undetected.
#'
#'   The detection of outliers may depend on the amount of unique coordinates
#'   available. Therefore, the detection of spatial outliers is safer for cases
#'   where many unique coordinates are available. As a rule of thumb, ten
#'   unique coordinates per taxa should avoid possible problems (undetected
#'   true outliers or detection of false outliers). See Examples.
#'
#' @examples
#'
#' # few data and close coordinates (no outliers)
#' lon <- c(-42.2,-42.3,-42.4,-42.3,-42.3)
#' lat <- c(-44.3,-44.2,-44.2,-42.2,-42.2)
#' df <- data.frame(lon = lon, lat = lat)
#' checkOut(df, lon = "lon", lat = "lat", n.min = 4)
#' checkOut(df, lon = "lon", lat = "lat", clas.cut = NULL, n.min = 4)
#'
#' # some data and one outlier
#' lon <- c(runif(5, -45, -41), -12.2)
#' lat <- c(runif(5, -45, -41), -18.2)
#' df <- data.frame(lon = lon, lat = lat)
#'
#' checkOut(df, lon = "lon", lat = "lat")
#' checkOut(df, lon = "lon", lat = "lat", clas.cut = NULL)
#' checkOut(df, lon = "lon", lat = "lat", rob.cut = NULL)
#'
#' # more data and one outlier
#' lon <- c(runif(9, -45, -41), -12.2)
#' lat <- c(runif(9, -45, -41), -18.2)
#' df <- data.frame(lon = lon, lat = lat)
#'
#' checkOut(df, lon = "lon", lat = "lat")
#' checkOut(df, lon = "lon", lat = "lat", clas.cut = NULL)
#' checkOut(df, lon = "lon", lat = "lat", rob.cut = NULL)
#'
#' @references
#' Lima, R.A.F. et al. 2020. Defining endemism levels for biodiversity
#' conservation: Tree species in the Atlantic Forest hotspot. Biological
#' Conservation, 252: 108825.
#'
#' Liu, C., White, M., and Newell, G. 2018. Detecting outliers in species
#' distribution data. Journal of Biogeography, 45(1): 164–176.
#'
#'
#' @seealso
#'  \link[plantR]{checkCoord}, \link[plantR]{getCult},
#'  \link[plantR]{mahalanobisDist}
#'
#' @import data.table
#' @importFrom stringr str_trim str_count
#'
#' @export checkOut
#'
checkOut <- function(x,
                     lon = "decimalLongitude.new",
                     lat = "decimalLatitude.new",
                     tax.name = "scientificName.new",
                     geo.name = "geo.check",
                     cult.name = "cult.check",
                     n.min = 5,
                     center = "median",
                     geo.patt = "ok_",
                     cult.patt = NA,
                     clas.cut = 3, rob.cut = 16) {

  #Escaping R CMD check notes from using data.table syntax
  tmp.order <- lon.wrk <- lat.wrk <- maha.classic <- NULL
  geo.wrk <- cult.wrk <- maha.robust <- classic.cut <- NULL
  robust.cut <- out.check <- NULL

  ## check input
  if (!class(x) == "data.frame")
    stop("Input object needs to be a data frame!")

  if (!all(c(lat, lon) %in% colnames(x)))
    stop("Coordinate column names do not match those of the input object: please rename or specify the correct names")

  if (!tax.name %in% colnames(x)) {
    rm.tax <- TRUE
    x[, tax.name] <- "sp1"
    warning("Column with species name not found: assuming that all records belong to the same taxa", call. = FALSE)
  } else { rm.tax <- FALSE }

  if (!geo.name %in% colnames(x)) {
    rm.geo <- TRUE
    x[, geo.name] <- TRUE
    warning("Column with geographical validation not found: assuming that all coordinates are valid", call. = FALSE)
  } else { rm.geo <- FALSE }

  if (!cult.name %in% colnames(x)) {
    rm.cult <- TRUE
    x[, cult.name] <- TRUE
    warning("Column with cultivated specimen search not found: assuming that all records are not cultivated", call. = FALSE)
  } else { rm.cult <- FALSE }

  # Filtering the dataset and creating the data table
  cols <- c(lat, lon, tax.name, geo.name, cult.name)
  dt <- data.table::as.data.table(x[,cols])
  dt[ , tmp.order := .I,]

  #Getting the classic and robust mahalanobis distances
  dt[, c("lon.wrk", "lat.wrk", "tax.wrk", "geo.wrk", "cult.wrk") := .SD,
     .SDcols =  c(lon, lat, tax.name, geo.name, cult.name)]
  dt[!is.na(lon.wrk) & !is.na(lat.wrk),
     maha.classic := mahalanobisDist(lon.wrk, lat.wrk, n.min = n.min,
                                     method = "classic", center = center,
                                     geo = geo.wrk, cult = cult.wrk,
                                     geo.patt = geo.patt,
                                     cult.patt = cult.patt),
     by = c("tax.wrk")]


  dt[!is.na(lon.wrk) & !is.na(lat.wrk),
     maha.robust := mahalanobisDist(lon.wrk, lat.wrk, n.min = n.min,
                                    method = "robust",
                                    geo = geo.wrk, cult = cult.wrk,
                                    geo.patt = geo.patt,
                                    cult.patt = cult.patt),
     by = c("tax.wrk")]

  ## Flaging the true and probable outliers based the cut-offs available defined above
  if (!is.null(clas.cut) &
      (length(clas.cut) == 1 | length(clas.cut) == dim(dt)[1])) {
    dt[ , classic.cut := clas.cut]
  } else { dt[ , classic.cut := 0] }

  if (!is.null(rob.cut) &
      (length(rob.cut) == 1 | length(rob.cut) == dim(dt)[1])) {
    dt[ , robust.cut := rob.cut]
  } else { dt[ , robust.cut := 0] }

  dt[, out.check := ifelse(maha.classic > classic.cut &
                             maha.robust > robust.cut, TRUE, FALSE), ]

  ## Preparing to return
  if (rm.tax)
    x <- x[ , -which(colnames(x) == tax.name)]
  if (rm.geo)
    x <- x[ , -which(colnames(x) == geo.name)]
  if (rm.cult)
    x <- x[ , -which(colnames(x) == cult.name)]

  data.table::setorder(dt, "tmp.order")
  x$out.check <- dt$out.check
  return(x)
}
