#' @title Format Names, Numbers, Dates and Codes
#'
#' @description This function standardizes collector names, determiner
#'   names, collection number, dates and collection codes from
#'   herbarium occurrences obtained from on-line databases, such as
#'   GBIF or speciesLink.
#'
#' @return The input data frame \code{x}, plus the new columns with
#'   the formatted information. The new columns have the same name as
#'   proposed by the Darwin Core standards followed by the suffix
#'   '.new'.
#'
#' @param x a data frame, containing typical fields from occurrence
#'   records from herbarium specimens
#' @param noNumb character. The standard notation for missing data in
#'   the field 'Number'. Default to "s.n."
#' @param noYear character. The standard notation for missing data in
#'   the field 'Year'. Default to  "n.d."
#' @param noName character. The standard notation for missing data in
#'   the field 'Name'. Default to  "s.n."
#'
#' @details The function works similarly to a wrapper, where many
#'   individual steps of the proposed __plantR__ workflow for editing
#'   collection information are performed altogether (see the
#'   __plantR__ tutorial and the help of each function for details).
#'
#'   Ideally, the input data frame must contain at least the following
#'   fields from the Darwin Core standards (the functions default):
#'   - 'institutionCode' and 'collectionCode' (codes of the institution
#'   and collection);
#'   - 'year' and 'eventDate' (year of the collection);
#'   - 'recordedBy' (collector(s) name(s));
#'   - 'recordNumber' (collector number)
#'   - 'identifiedBy' (identifier name);
#'   - 'yearIdentified' and 'dateIdentified' (year of identification)
#'
#'   Missing year of collection in the field 'year' are internally
#'   replaced by the date stored in the field 'eventDate', if this
#'   field is not empty as well.
#'
#' @seealso
#'  \link[plantR]{getCode}, \link[plantR]{fixName}, \link[plantR]{colNumber},
#'  \link[plantR]{getYear}, \link[plantR]{prepTDWG}, \link[plantR]{prepName},
#'  \link[plantR]{missName} and \link[plantR]{lastName}.
#'
#' @author Renato A. Ferreira de Lima
#'
#' @import data.table
#'
#' @export formatOcc
#'
formatOcc <- function(x,
                      noNumb = "s.n.",
                      noYear = "n.d.",
                      noName = "s.n.") {

  ## Check input
  if (!inherits(x, "data.frame"))
    stop("Input object needs to be a data frame!")

  if (dim(x)[1] == 0)
    stop("Input data frame is empty!")

  # Missing year of collection that may be stored in the field 'eventDate'
  if ("eventDate" %in% names(x) & "year" %in% names(x)) {
    ids <- is.na(x$year) & !is.na(x$eventDate)
    if (any(ids))
      x$year[ids] <- x$eventDate[ids]
  }

  # Missing year of collection that may be stored in the field 'verbatimEventDate'
  if ("verbatimEventDate" %in% names(x) & "year" %in% names(x)) {
    ids <- is.na(x$year) & !is.na(x$verbatimEventDate)
    if (any(ids))
      x$year[ids] <- x$verbatimEventDate[ids]
  }

  # Missing year of identification that may be stored in the field 'dateIdentified'
  if ("dateIdentified" %in% names(x) & "yearIdentified" %in% names(x)) {
    ids <- is.na(x$dateIdentified) & !is.na(x$yearIdentified)
    if (any(ids))
      x$dateIdentified[ids] <- x$yearIdentified[ids]
  }

  # Missing year of identification that may be stored in the field 'dateIdentified'
  if ("dateIdentified" %in% names(x) & "yearIdentified" %in% names(x)) {
    ids <- is.na(x$dateIdentified) & !is.na(x$yearIdentified)
    if (any(ids))
      x$dateIdentified[ids] <- x$yearIdentified[ids]
  }

  # Missing latitudes stored in fields 'latitude' or 'verbatimLatitude'
  if ("decimalLatitude" %in% names(x) & "latitude" %in% names(x)) {
    ids <- is.na(x$decimalLatitude) & !is.na(x$latitude)
    if (any(ids))
      x$decimalLatitude[ids] <- x$latitude[ids]
  }
  if ("decimalLatitude" %in% names(x) & "verbatimLatitude" %in% names(x)) {
    ids <- is.na(x$decimalLatitude) & !is.na(x$verbatimLatitude)
    if (any(ids))
      x$decimalLatitude[ids] <- x$verbatimLatitude[ids]
  }

  # Missing longitudes stored in fields 'longitude' or 'verbatimLongitude'
  if ("decimalLongitude" %in% names(x) & "longitude" %in% names(x)) {
    ids <- is.na(x$decimalLongitude) & !is.na(x$longitude)
    if (any(ids))
      x$decimalLongitude[ids] <- x$longitude[ids]
  }
  if ("decimalLongitude" %in% names(x) & "verbatimLongitude" %in% names(x)) {
    ids <- is.na(x$decimalLongitude) & !is.na(x$verbatimLongitude)
    if (any(ids))
      x$decimalLongitude[ids] <- x$verbatimLongitude[ids]
  }

  # Missing recordedBy and recordNumber stored in fields 'collector' or 'collectornumber'
  if ("recordedBy" %in% names(x) & "collector" %in% names(x)) {
    ids <- is.na(x$recordedBy) & !is.na(x$collector)
    if (any(ids))
      x$recordedBy[ids] <- x$collector[ids]
  }
  if ("recordNumber" %in% names(x) & "collectornumber" %in% names(x)) {
    ids <- is.na(x$recordNumber) & !is.na(x$collectornumber)
    if (any(ids))
      x$recordNumber[ids] <- x$collectornumber[ids]
  }

  ## Checking the presence of reserved columns in the input dataset
  x <- checkColNames(x, group = "format.occs")

  ## Standardizing collection codes
  x <- getCode(x)

  ## First edits to names, numbers and dates
  if ("recordedBy" %in% names(x)) {
    x$recordedBy.new <- fixName(x$recordedBy)
  } else {
    x$recordedBy.new <- noName
  }

  # Collector number
  if ("recordNumber" %in% names(x)) {
    x$recordNumber.new <- colNumber(x$recordNumber, noNumb = noNumb)
  } else {
    x$recordNumber.new <- noNumb
  }

  # Collection year
  if ("year" %in% names(x)) {
    x$year.new <- getYear(x$year, noYear = noYear)
  } else {
    x$year.new <- noYear
  }

  # Identifier name
  if ("identifiedBy" %in% names(x)) {
    x$identifiedBy.new <- fixName(x$identifiedBy)
  } else {
    x$identifiedBy.new <- noName
  }

  # Identification year
  if ("dateIdentified" %in% names(x)) {
    x$yearIdentified.new <- getYear(x$dateIdentified, noYear = noYear)
  } else {
    x$year.new <- noYear
  }

  ## Putting collector's name into the default name notation and
  # separating main and auxiliary names
  if (any(!x$recordedBy.new %in% c("", " ", NA, "NA", noName))) {
    # separating and preping names
    x$recordedBy.aux <- prepName(x$recordedBy.new,
                                 fix.names = FALSE,
                                 sep.out = "; ",
                                 output = "aux")
    x$recordedBy.new <- prepName(x$recordedBy.new,
                                 fix.names = FALSE,
                                 output = "first")
    # standardizing the notation for missing names
    x$recordedBy.new <- missName(x$recordedBy.new,
                                 type = "collector",
                                 noName = noName)
    # extract the last name of the collector
    x$last.name <- lastName(x$recordedBy.new,
                            noName = noName)
  } else {
    x$recordedBy.aux <- NA
    x$last.name <- NA
  }

  ## Putting collector's name into the default name notation and
  # separating main and auxiliary names
  if (any(!x$identifiedBy.new %in% c("", " ", NA, "NA", noName))) {
    x$identifiedBy.aux <- prepName(x$identifiedBy.new,
                                   fix.names = FALSE,
                                   sep.out = "; ",
                                   output = "aux")
    x$identifiedBy.new <- prepName(x$identifiedBy.new,
                                   fix.names = FALSE,
                                   output = "first")
    # standardizing the notation for missing names
    x$identifiedBy.new <- missName(x$identifiedBy.new,
                                   type = "identificator",
                                   noName = noName)
  } else {
    x$identifiedBy.aux <- NA
  }
  return(x)
}
