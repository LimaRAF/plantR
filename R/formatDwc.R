#' @title Formatting and binding databases using DarwinCore fields
#'
#' @description Formats fields in the occurrence data frame -either downloaded from a database or provided by the user- according to the DarwinCore standard. Optionally, drops fields not used in data cleaning performed by plantR. In addition, this functions can bind different sources of data after formatting.
#'
#' @param splink_data A data frame as in the output from `rspeciesLink()`
#' @param gbif_data A data frame as in the output from `rgbif()` or `rgbif2()`
#' @param user_data A data frame provided by the user. Minimum fields are: `c("collectionCode", "catalogNumber", "recordNumber", "recordedBy", "year", "country", "stateProvince", "county", "municipality", "decimalLatitude", "decimalLongitude", "identifiedBy", "dateIdentified", "typeStatus", "scientificName", "scientificNameAuthorship", "institutionCode")`. Fields can be placed at any order and any given name. If using `user_data` argument, the user must indicate the name of the column for each field if not already in DwC standard
#' @param drop Logical. Either to drop fields unused by the data cleaning routine performed by plantR
#' @param bind_data Logical. Either to bind data from differents sources after formatting
#' @param collectionCode The name of the column containing the name, acronym, coden, or initialism identifying the collection or data set from which the record was derived
#' @param catalogNumber The name of the column containing an identifier for the record within the data set or collection
#' @param recordNumber The name of the column containing an identifier given to the Occurrence at the time it was recorded. Often serves as a link between field notes and an Occurrence record, such as a specimen collector's number
#' @param recordedBy The name of the column containing a person, group, or organization responsible for recording the original Occurrence
#' @param year The name of the column containing the four-digit year in which the Event occurred, according to the Common Era Calendar
#' @param country The name of the column containing the name of the country or major administrative unit in which the Location occurs
#' @param stateProvince The name of the column containing the name of the next smaller administrative region than country (state, province, canton, department, region, etc.) in which the Location occurs
#' @param county The name of the column containing the full, unabbreviated name of the next smaller administrative region than stateProvince (county, shire, department, etc.) in which the Location occurs
#' @param municipality The name of the column containing the full, unabbreviated name of the next smaller administrative region than county (city, municipality, etc.) in which the Location occurs. Do not use this term for a nearby named place that does not contain the actual location
#' @param decimalLatitude The name of the column containing the geographic latitude (in decimal degrees, using the spatial reference system given in geodeticDatum) of the geographic center of a Location. Positive values are north of the Equator, negative values are south of it. Legal values lie between -90 and 90, inclusive
#' @param decimalLongitude The name of the column containing the geographic longitude (in decimal degrees, using the spatial reference system given in geodeticDatum) of the geographic center of a Location. Positive values are east of the Greenwich Meridian, negative values are west of it. Legal values lie between -180 and 180, inclusive
#' @param identifiedBy The name of the column containing alist (concatenated and separated) of names of people, groups, or organizations who assigned the Taxon to the subject
#' @param dateIdentified The name of the column containing the date on which the subject was identified as representing the Taxon
#' @param typeStatus The name of the column containing a nomenclatural type (type status, typified scientific name, publication) applied to the subject
#' @param scientificName The name of the column containing the full scientific name, with authorship and date information if known. When forming part of an Identification, this should be the name in lowest level taxonomic rank that can be determined. This term should not contain identification qualifications, which should instead be supplied in the IdentificationQualifier term
#' @param scientificNameAuthorship The name of the column containing the authorship information for the scientificName formatted according to the conventions of the applicable nomenclaturalCode
#' @param institutionCode The name of the column containing The name (or acronym) in use by the institution having custody of the object(s) or information referred to in the record
#'
#' @return Either a data.frame or list with the database fields formatted following DarwinCore standards
#'
#' @author Lima, R. A. F. and Sara Mortara
#'
#' @importFrom countrycode countrycode
#' @importFrom utils read.csv
#'
#' @export formatDwc
#'
formatDwc <- function(
  splink_data = NULL,
  gbif_data = NULL,
  user_data = NULL,
  drop = FALSE,
  bind_data = FALSE,
  collectionCode = "collectionCode",
  catalogNumber = "catalogNumber",
  recordNumber = "recordNumber",
  recordedBy = "recordedBy",
  year = "year",
  country = "country",
  stateProvince = "stateProvince",
  county = "county",
  municipality = "municipality",
  decimalLatitude = "decimalLatitude",
  decimalLongitude = "decimalLongitude",
  identifiedBy = "identifiedBy",
  dateIdentified = "dateIdentified",
  typeStatus = "typeStatus",
  scientificName = "scientificName",
  scientificNameAuthorship = "scientificNameAuthorship",
  institutionCode = "institutionCode",
) {

  # Required fields by plantR
  must <- sort(unique(na.omit(fieldNames$plantr)))

  # formating user data --------------------------------------------------------
  if (!is.null(user_data)) {
    user_colnames <- c(collectionCode, catalogNumber, recordNumber, recordedBy,
                       year, country, stateProvince, county, municipality,
                       decimalLatitude, decimalLongitude, identifiedBy, dateIdentified,
                       typeStatus, scientificName, scientificNameAuthorship, institutionCode)
    names(user_colnames) <- c("collectionCode", "catalogNumber", "recordNumber", "recordedBy",
                              "year", "country", "stateProvince", "county", "municipality",
                              "decimalLatitude", "decimalLongitude", "identifiedBy", "dateIdentified",
                              "typeStatus", "scientificName", "scientificNameAuthorship", "institutionCode")
    if (!names(user_data) %in% user_colnames) {
      stop("You must provide the minimum fields required for data cleaning!")
    }
    # First ordering data frame
    user_data_or <- user_data[, user_colnames]
    # Now applying DwC names
    names(user_data_or) <- names(user_colnames)
  }

  # formating speciesLink data -------------------------------------------------
  if (!is.null(splink_data)) {
    splink_cols <- sort(unique(na.omit(fieldNames$speciesLink)))
    if (!names(splink_data) %in% splink_cols) {
      stop("You must provide the minimum fields required for data cleaning!")
    }
    fieldNames$speciesLink[!is.na(fieldNames$speciesLink)]
  }

  return(data_dwc)
}
