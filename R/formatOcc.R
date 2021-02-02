#' @title Format Names, Numbers, Dates and Codes
#'
#' @description This function standardizes collector names, determiner names,
#'   collection number, dates and collection codes from herbarium occurrences
#'   obtained from on-line databases, such as GBIF or speciesLink.
#'
#' @return The input data frame \code{x}, plus the new columns with the
#'   formatted information. The new columns have the same name as proposed by
#'   the Darwin Core standards followed by the suffix '.new'.
#'
#' @param x a data frame, containing typical fields from occurrence records from
#'   herbarium specimens
#' @param noNumb character. The standard notation for missing data in the field
#'   'Number'
#' @param noYear character. The standard notation for missing data in the field
#'   'Year'
#' @param noName character. The standard notation for missing data in the field
#'   'Name'
#'
#' @details The function works siyesmilarly to a wrapper, where many individuals
#'   steps of the proposed __plantR__ workflow for editing collection
#'   information are performed altogether (see the __plantR__ tutorial and the
#'   help of each function for details).
#'
#'   Ideally, the input data frame must contain at least the following fields
#'   from the Darwin Core standards (the functions default):
#'   - 'intitutionCode' and 'collectionCode' (codes of the institution and collection);
#'   - 'year' and 'eventDate' (year of the collection);
#'   - 'recordedBy' (collector(s) name(s));
#'   - 'recordNumber' (collector number)
#'   - 'identifiedBy' (identificator name);
#'   - 'yearIdentified' and 'dateIdentified' (year of identification)
#'
#'   Missing year of collection in the field 'year' are internally replaced by
#'   the date stored in the field 'eventDate', if this field is not empty as well.
#'
#'   The function uses the R package __data.table__ internally to speed up the
#'   processing of larger data sets.
#'
#' @seealso
#'  \link[plantR]{getCode}, \link[plantR]{fixName}, \link[plantR]{colNumber},
#'  \link[plantR]{getYear}, \link[plantR]{prepTDWG}, \link[plantR]{prepName},
#'  \link[plantR]{missName} and \link[plantR]{lastName}.
#'
#' @author Renato A. F. de Lima
#'
#' @import data.table
#'
#' @export formatOcc
#'
formatOcc <- function(x, noNumb = "s.n.", noYear = "n.d.", noName = "s.n.") {

  #Escaping R CMD check notes from using data.table syntax
  "recordedBy.new" <- "recordedBy" <- "identifiedBy.new" <- NULL
  "identifiedBy" <- "recordNumber.new" <- "recordNumber" <- NULL
  "year.new" <- "dateIdentified.new" <- "dateIdentified" <- NULL
  "yearIdentified" <- "yearIdentified.new" <- "order" <- "tmp.ordem" <- NULL
  "recordedBy.aux" <- "identifiedBy.aux" <- "last.name" <- NULL

  ## Check input
  if (!class(x) == "data.frame")
    stop("input object needs to be a data frame!")

  # Missing year of collection that may be stored in the field 'eventDate'
  if ("eventDate" %in% names(x) & "year" %in% names(x)) {
    ids <- is.na(x$year) & !is.na(x$eventDate)
    x$year[ids] <- x$eventDate[ids]
  }

  # Missing year of collection that may be stored in the field 'verbatimEventDate'
  if ("verbatimEventDate" %in% names(x) & "year" %in% names(x)) {
    ids <- is.na(x$year) & !is.na(x$verbatimEventDate)
    x$year[ids] <- x$verbatimEventDate[ids]
  }

  # Missing year of identification that may be stored in the field 'dateIdentified'
  if ("dateIdentified" %in% names(x) & "yearIdentified" %in% names(x)) {
    ids <- is.na(x$dateIdentified) & !is.na(x$yearIdentified)
    x$dateIdentified[ids] <- x$yearIdentified[ids]
  }

  ## Standardizing collection codes
  x <- getCode(x)

  ## First edits to names, numbers and dates
  dt <- data.table::data.table(x)
  dt[ , tmp.ordem := .I, ]

  # Collector name
  data.table::setkey(dt, recordedBy)
  dt[, recordedBy.new := fixName(recordedBy),  by = recordedBy]

  # Collector number
  data.table::setkey(dt, recordNumber)
  dt[, recordNumber.new := colNumber(recordNumber, noNumb = noNumb),  by = recordNumber]

  # Collection year
  data.table::setkey(dt, year)
  dt[, year.new := getYear(year, noYear = noYear),  by = year]

  # Identificator name
  data.table::setkey(dt, identifiedBy)
  dt[, identifiedBy.new := fixName(identifiedBy),  by = identifiedBy]

  # Identification year
  data.table::setkey(dt, dateIdentified)
  dt[, yearIdentified.new := getYear(dateIdentified, noYear = noYear),  by = dateIdentified]

  ## Putting people's names into the default name notation and
  #separating main and auxiliary names
  data.table::setkey(dt, recordedBy.new)
  dt[, recordedBy.aux := prepName(recordedBy.new, fix.names = FALSE, sep.out = "; ", output = "aux"),
     by = recordedBy.new]
  dt[, recordedBy.new := prepName(recordedBy.new, fix.names = FALSE, output = "first"),
     by = recordedBy.new]

  data.table::setkey(dt, identifiedBy.new)
  dt[, identifiedBy.aux := prepName(identifiedBy.new, fix.names = FALSE, sep.out = "; ", output = "aux"),
     by = identifiedBy.new]
  dt[, identifiedBy.new := prepName(identifiedBy.new, fix.names = FALSE, sep.out = "; ", output = "first"),
     by = identifiedBy.new]

  ## Standardize the notation for missing names
  data.table::setkey(dt, recordedBy.new)
  dt[, recordedBy.new := missName(recordedBy.new, type = "collector", noName = noName),
     by = recordedBy.new]
  data.table::setkey(dt, identifiedBy.new)
  dt[, identifiedBy.new := missName(identifiedBy.new, type = "identificator", noName = noName),
     by = identifiedBy.new]

  ## Extract the last name of the collector
  data.table::setkey(dt, recordedBy.new)
  dt[, last.name := lastName(recordedBy.new, noName = "s.n."),
     by = recordedBy.new]

  ## Re-ordering and returning
  data.table::setorder(dt, "tmp.ordem")
  dt[, tmp.ordem := NULL,]
  df <- as.data.frame(dt)
  return(df)
}
