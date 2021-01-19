#' @title Format Names, Numbers, Dates and Codes
#'
#' @description This function standardizes collector names, determiner names,
#'   collection number, date and collection codes from herbarium occurrences
#'   obtained from on-line databases, such as GBIF or speciesLink.
#'
#' @return The input data frame \code{x}, plus the new columns with the
#'   formatted information. The new columns have the same name as proposed by
#'   the Darwin Core standards followed by the suffix '.new'.
#'
#' @param x a data frame, containing typical fields from occurrence records from
#'   herbarium specimens
#' @param noNumb character. The standard notation for missing data in the field 'Number'
#' @param noYear character. The standard notation for missing data in the field 'Year'
#' @param noName character. The standard notation for missing data in the field 'Name'
#'
#' @details The function works similarly to a wrapper function, where many
#'   individuals steps of the proposed __plantR__ workflow for editing collection
#'   information are performed altogether (see the __plantR__ tutorial and the
#'   help of each function for details).
#'
#'   Ideally, the input data frame must contain at least the following fields
#'   from the Darwin Core standards (the functions default):
#'   - 'intitutionCode' and 'collectionCode' (codes of the institution and collection);
#'   - 'eventDate' and/or 'year' (date/year of the collection);
#'   - 'recordedBy' (collector(s) name(s));
#'   - 'recordNumber' (collector number)
#'   - 'identifiedBy' (identificator name);
#'   - 'dateIdentified' or 'yearIdentified' (date or year of identification)
#'
#'   Missing year of collection in the field 'year' are internally replaced by
#'   the date stored in the field 'eventDate', if this field is not empty as well.
#'
#'   The function uses the R package `data.table` internally to speed up the
#'   processing of larger data sets
#'
#' @seealso
#'  \link[plantR]{getCode}, \link[plantR]{fixName}, \link[plantR]{colNumber},
#'  \link[plantR]{getYear}, \link[plantR]{tdwgNames}, \link[plantR]{formatName},
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
  "yearIdentified" <- "yearIdentified.new" <- "order" <- NULL
  "recordedBy.aux" <- "identifiedBy.aux" <- "last.name" <- NULL

  ## Check input
  if (!class(x) == "data.frame")
    stop("input object needs to be a data frame!")

  # Missing year of collection that may be stored in the field 'eventDate'
  if ("eventDate" %in% names(x)) {
    ids <- is.na(x$year) & !is.na(x$eventDate)
    x$year[ids] <- x$eventDate[ids]
  }

  # Missing year of identification that may be stored in the field 'eventDate'
  if ("dateIdentified" %in% names(x)) {
    ids <- is.na(x$yearIdentified) & !is.na(x$dateIdentified)
    x$yearIdentified[ids] <- x$dateIdentified[ids]
  }

  ## Standardizing collection codes
  x <- getCode(x)

  ## First edits to names, numbers and dates
  dt <- data.table::data.table(x)
  dt[ , tmp.ordem := .I, ]
  # occs <- data.table::data.table(x)
  # occs[ , order := 1:dim(occs)[1], ]
  # data.table::setkeyv(occs, c("order"))

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
  data.table::setkey(dt, yearIdentified)
  dt[, yearIdentified.new := getYear(yearIdentified, noYear = noYear),  by = yearIdentified]

  # occs[, recordedBy.new := fixName(recordedBy, special.char = FALSE),  by = order]
  # occs[, recordNumber.new := colNumber(recordNumber, noNumb = "s.n."),  by = order]
  # occs[, identifiedBy.new := fixName(identifiedBy, special.char = FALSE),  by = order]
  # occs[, year.new := getYear(year, noYear = "n.d."),  by = order]
  # if ("dateIdentified" %in% names(occs))
  #   occs[, dateIdentified.new := getYear(dateIdentified, noYear = "n.d."),  by = order]
  # if ("yearIdentified" %in% names(occs))
  #   occs[, yearIdentified.new := getYear(yearIdentified, noYear = "n.d."),  by = order]

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

  # occs[, recordedBy.new := formatName(recordedBy.new), by = order]
  # occs[, identifiedBy.new := formatName(identifiedBy.new), by = order]

  # #We then create the auxiliary names list and convert auxiliary and first names to the TDWG format (function `tdwgNames()`)
  # occs[, recordedBy.aux := as.character(tdwgNames(recordedBy.new, out = "aux", sep.out = "; ")), by = order]
  # occs[, identifiedBy.aux := as.character(tdwgNames(identifiedBy.new, out = "aux", sep.out = "; ")), by = order]
  # occs[, recordedBy.new := tdwgNames(recordedBy.new, out = "first"), by = order]
  # occs[, identifiedBy.new := tdwgNames(identifiedBy.new, out = "first"), by = order]

  # #It is also useful for the validation process to standardize the notation for missing collector and identificator name
  # occs[, recordedBy.new := missName(recordedBy.new, type = "collector", noName = "s.n."), by = order]
  # occs[, identifiedBy.new := missName(identifiedBy.new, type = "identificator", noName = "s.n."), by = order]
  #
  # #And to extract the last name of the collector
  # occs[, last.name := lastName(recordedBy.new, noName = "s.n."), by = order]

  ## Re-ordering and returning
  data.table::setorder(dt, "tmp.ordem")
  dt[, tmp.ordem := NULL,]
  df <- as.data.frame(occs)
  return(df)
}
