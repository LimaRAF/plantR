#' @title Flag Duplicated Coordinates
#'
#' @param x Data frame with geographical coordinates
#' @param lon character. Column with the record longitude in decimal degrees
#' @param lat character. Column with the record latitude in decimal degrees
#' @param tax.name character. Name of the columns containing the species name.
#'   Default to "scientificName.new"
#' @param type character. Type of approach to search for duplicated coordinates.
#' @param output character. Type of output desired: 'group' and/or 'flag'.
#' @param min.dist numerical. Minimun threshold distance (in kilometers) to be
#'   used to detect duplicated coordinates.
#'
#' @return the input data frame and the new columns with the results from the
#'   'exact' and/or 'minimun distance' approaches to flag duplicated coordinates.
#'
#' @details If `type` is 'exact', only coodinates with the exact same coordinates
#'   are flagged. If `type` is 'dist', coordinates below the minimun threshold
#'   distance defined by `min.dist` are flagged. If both methods are selected,
#'   the function returns the results for both approaches.
#'
#'   If `output` is 'group' (the default), the column returned contains the suffix
#'   '.ID' and it stores a number that can be used as a species-specific ID to group
#'   duplicated coordinates. If `output` is 'flag', then a TRUE/FALSE vector is
#'   returned indicating which coordinates are duplicated within each species.
#'   If both are selected, then both are returned. Note that the ID numbers for
#'   each of the approaches ('exact' and 'dist') are not related.
#'
#'   If no column containing the species names is provided, the function assumes
#'   that all coordinates belong to the same species, with a warning.
#'
#'   The argument `min.dist` 0.001 km (1 meter) is by default and not zero,
#'   because the precision of geographical coordinates are often above 1 meter.
#'   In practice, a `min.dist` of 0.0001 or lower should return the same output
#'   as the exact approach.
#'
#' @import data.table
#' @importFrom dplyr left_join
#'
#' @examples
#' coords <- data.frame(
#' lat = c(-23.475389, -23.475389, -23.475390, -23.485389,
#'         -23.575389, -23.575389, -23.575390, -23.485389),
#' lon = c(-47.123768, -47.123768, -47.123768, -47.113768,
#'         -47.223768, -47.223768, -47.223768, -47.113768))
#' \dontrun{
#' uniqueCoord(coords, lon = "lon", lat = "lat")
#' uniqueCoord(coords, lon = "lon", lat = "lat", type = "dist")
#' uniqueCoord(coords, lon = "lon", lat = "lat", min.dist = 0.0001)
#' uniqueCoord(coords, lon = "lon", lat = "lat", output = "flag")
#' }
#'
#' @seealso
#'  \link[plantR]{geoDist} and \link[plantR]{minDist}.
#'
#' @author Renato A. Ferreira de Lima
#'
#' @keywords internal
#'
uniqueCoord <- function(x,
                         lon = "decimalLongitude.new",
                         lat = "decimalLatitude.new",
                         tax.name = "scientificName.new",
                         type = c("exact", "dist"),
                         output = "group",
                         min.dist = 0.001) {

  #Escaping R CMD check notes from using data.table syntax
  coord.string <- exact.ID <- exact <- coord.ID <- NULL
  lon.wrk <- lat.wrk <- dist.ID <- dists <- NULL

  ## check input
  if (!class(x) == "data.frame")
    stop("Input object needs to be a data frame!")

  if (!all(c(lat, lon) %in% colnames(x)))
    stop("Coordinates column names declared do not match those of the input object: please rename or specify the correct names")

  if (!tax.name %in% colnames(x)) {
    rm.tax <- TRUE
    x[, tax.name] <- "sp1"
    warning("Column with species name not found: assuming that all records belong to the same taxa", call. = FALSE)
  } else { rm.tax <- FALSE }

  if (all(!c("exact", "dist") %in% type))
    stop("Please choose between the 'exact' and/or the 'distance' approaches to flag duplicated coordinates")

  if (all(!c("group", "flag") %in% output))
    stop("Please choose between the output 'group' and/or the 'flag' outputs")

  # Indexing the dataset and setting the columns of interest
  x$tmp.order <- 1:nrow(x)
  cols <- c(lat, lon, tax.name, "tmp.order")

  # Removing missing coordinates
  if (any(is.na(x[, c(lon, lat)]))) {
    antes <- dim(x)[1]
    tmp <- x[!is.na(x[, lon]), ]
    tmp <- tmp[!is.na(tmp[, lat]), ]
    dt <- data.table::as.data.table(tmp[, cols])

    miss.coords <- antes - dim(tmp)[1]
    warning(paste0("Missing coordinates for ", miss.coords," were excluded from the spatial duplicate search"), call. = FALSE)
  } else {
    dt <- data.table::as.data.table(x[, cols])
  }

  # Exact same coordinates
  if (any("exact" %in% type)) {
    dt[, coord.string := do.call(paste, c(.SD, sep = "_")),
       .SDcols = c(lon, lat)]
    data.table::setkeyv(dt, c("coord.string", tax.name))
    dt[, exact.ID := .GRP, by = c("coord.string", tax.name)]
    dt[, exact := duplicated(exact.ID)]
    dt[, c("coord.string") := NULL]
  }

  # By minimun distance
  if (any("dist" %in% type)) {
    dt[, c("lon.wrk", "lat.wrk") := .SD, .SDcols =  c(lon, lat)]
    data.table::setkeyv(dt, c(tax.name))
    dt[, coord.ID := minDist(lon.wrk, lat.wrk, min.dist = min.dist, output = 'group'),
       by = c(tax.name)]
    dt[, dist.ID := .GRP, by = c("coord.ID", tax.name)]
    dt[, dists := duplicated(.SD), by = c("dist.ID"), .SDcols = c("dist.ID")]
    dt[, c("lon.wrk", "lat.wrk", "coord.ID") := NULL]
  }

  # Preparing to return
  dt[, c(lat, lon, tax.name) := NULL]
  if (!"group" %in% output) {
    col.names <- c("exact.ID", "dist.ID")
    col.names <- col.names[col.names %in% names(dt)]
    dt[ , c(col.names) := NULL]
  }

  if (!"flag" %in% output) {
    col.names <- c("exact", "dists")
    col.names <- col.names[col.names %in% names(dt)]
    dt[ , c(col.names) := NULL]
  }

  if (rm.tax)
    x <- x[, -which(names(x) == tax.name)]

  result <- dplyr::left_join(x,
                             dt,
                             by = "tmp.order")
  result <- result[, -which(names(result) == "tmp.order")]
  return(result)
}
