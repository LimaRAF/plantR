#' @title Format Names, Numbers and Dates
#'
#' @description This function standardizes collector names, determiner names,
#'   collection number, and date from herbarium occurrences obtained from
#'   on-line databases, such as GBIF or speciesLink
#'
#' @return The input data frame \code{x}, plus the new columns with the formatted
#'   fields
#'
#' @param x a data frame, containing typical fields from occurrence records from
#'   herbarium specimens
#' @param noNumb standard notation for missing data in Number
#' @param noYear standard notation for missing data in Year
#' @param noName standard notation for missing data in Name
#'
#' @details The function works similarly to a wrapper function, where many
#'   individuals steps of the proposed `plantR` workflow for editing some
#'   important fields are performed at once (see the Data Editing vignette)
#'
#'   The input data frame must contain at least the following fields:
#'   'eventDate' and/or 'year' (date/year of the collection); 'recordedBy'
#'   (collector(s) name(s)); 'recordNumber'; (collector number) 'identifiedBy'
#'   (identificator name); 'dateIdentified' or 'yearIdentified' (date or year of
#'   identification)
#'
#'   The function uses the R package `data.table` in the attempt to speed up the
#'   processing of larger data sets
#'
#' @seealso
#'  \link[plantR]{fixName}, \link[plantR]{colNumber}, \link[plantR]{getYear},
#'  \link[plantR]{tdwgNames}, \link[plantR]{formatName}, \link[plantR]{missName}
#'  and \link[plantR]{lastName}.
#'
#' @author Renato A. F. de Lima
#'
#' @import data.table
#'
#' @export formatOcc
#'
formatOcc <- function(x,
                      noNumb,
                      noYear,
                      noName) {

  "recordedBy.new" <- "recordedBy" <- "identifiedBy.new" <- NULL
  "identifiedBy" <- "recordNumber.new" <- "recordNumber" <- NULL
  "year.new" <- "dateIdentified.new" <- "dateIdentified" <- NULL
  "yearIdentified" <- "yearIdentified.new" <- NULL
  "recordedBy.aux" <- "identifiedBy.aux" <- "last.name" <- NULL

  x$order <- 1:dim(x)[1]
  occs <- data.table::data.table(x)
  data.table::setkeyv(occs, c("order"))

  #For the year of collection, sometimes the information is stored in the field
  # 'eventDate' and not in the field 'year'
  if ("eventDate" %in% names(occs))
    occs$year[is.na(occs$year) & !is.na(occs$eventDate)] <-
    occs$eventDate[is.na(occs$year) & !is.na(occs$eventDate)]

  #We then prepare the new fields for further processing
  occs[, recordedBy.new := fixName(recordedBy, special.char = FALSE),  by = order]
  occs[, recordNumber.new := colNumber(recordNumber, noNumb = "s.n."),  by = order]
  occs[, identifiedBy.new := fixName(identifiedBy, special.char = FALSE),  by = order]
  occs[, year.new := getYear(year, noYear = "n.d."),  by = order]
  if ("dateIdentified" %in% names(occs))
    occs[, dateIdentified.new := getYear(dateIdentified, noYear = "n.d."),  by = order]
  if ("yearIdentified" %in% names(occs))
    occs[, yearIdentified.new := getYear(yearIdentified, noYear = "n.d."),  by = order]

  #Next, we format the names
  occs[, recordedBy.new := formatName(recordedBy.new), by = order]
  occs[, identifiedBy.new := formatName(identifiedBy.new), by = order]

  #We then create the auxiliary names list and convert auxiliary and first names to the TDWG format (function `tdwgNames()`)
  occs[, recordedBy.aux := as.character(tdwgNames(recordedBy.new, out = "aux", sep.out = "; ")), by = order]
  occs[, identifiedBy.aux := as.character(tdwgNames(identifiedBy.new, out = "aux", sep.out = "; ")), by = order]
  occs[, recordedBy.new := tdwgNames(recordedBy.new, out = "first"), by = order]
  occs[, identifiedBy.new := tdwgNames(identifiedBy.new, out = "first"), by = order]

  #It is also useful for the validation process to standardize the notation for missing collector and identificator name
  occs[, recordedBy.new := missName(recordedBy.new, type = "collector", noName = "s.n."), by = order]
  occs[, identifiedBy.new := missName(identifiedBy.new, type = "identificator", noName = "s.n."), by = order]

  #And to extract the last name of the collector
  occs[, last.name := lastName(recordedBy.new, noName = "s.n."), by = order]

  #Removing the ordering column created
  occs[, order := NULL,]
  df <- as.data.frame(occs)
  return(df)
}
