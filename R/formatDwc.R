#' @title Formatting and binding databases using DarwinCore fields
#'
#' @description Formats fields in the occurrence data frame -either downloaded from a database or provided by the user- according to the DarwinCore standard. Optionally, drops fields not used in data cleaning performed by plantR. In addition, this functions can bind different sources of data after formatting.
#'
#' @param splink_data A data frame as in the output from `rspeciesLink()`
#' @param gbif_data A data frame as in the output from `rgbif()` or `rgbif2()`
#' @param user_data A data frame provided by the user. Minimum fields are: `c("collectionCode", "catalogNumber", "recordNumber", "recordedBy", "year", "country", "stateProvince", "county", "municipality", "decimalLatitude", "decimalLongitude", "identifiedBy", "dateIdentified", "typeStatus", "scientificName", "scientificNameAuthorship", "institutionCode")`. Fields can be placed at any order and any given name. If using `user_data` argument, the user must indicate the name of the column for each field if not already in DwC standard
#' @param bind_data Logical. Either to bind data from differents sources after formatting
#' @param drop Logical. Either to drop fields unused by the data cleaning routine performed by plantR
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
#' @importFrom stringr str_trim
#' @importFrom flora remove.authors
#' @importFrom dplyr bind_rows
#'
#' @export formatDwc
#'
#' @author Sara R. Mortara and Andrea Sánchez-Tapia
#'
formatDwc <- function(splink_data = NULL,
                      gbif_data = NULL,
                      user_data = NULL,
                      bind_data = TRUE,
                      drop = FALSE,
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
                      institutionCode = "institutionCode") {

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
    if (any(!user_colnames %in% names(user_data))) {
      stop("user_data does not have the minimum fields required for data cleaning!")
    }
    # First ordering data frame
    user_data_or <- user_data[, user_colnames]
    # Now applying DwC names
    names(user_data_or) <- names(user_colnames)
  }

  # formating speciesLink data -------------------------------------------------
  if (!is.null(splink_data)) {
    splink_cols <- sort(unique(na.omit(fieldNames$speciesLink)))
    if (any(!splink_cols %in% names(splink_data))) {
      stop("splink_data does not follow the speciesLink pattern!")
    }
    # absent fields in speciesLink: municipality and dateIdentified
    # Creating field municipality
    splink_data$municipality <- NA
    # Creating field dateIdentified
    splink_date <- apply(X = splink_data[, c("yearIdentified", "monthIdentified", "dayIdentified")],
                         MARGIN = 1,
                         FUN = function(x) paste(x, collapse = "-"))
    splink_data$dateIdentified <- ifelse(grepl("NA-NA-NA", splink_date), NA, splink_date)
  }

  # formating gbif data --------------------------------------------------------
  if (!is.null(gbif_data)) {
    gbif_cols <- sort(unique(na.omit(fieldNames$gbif)))
    if (any(!gbif_cols %in% names(gbif_data))) {
      stop("gbif_data does not follow the gbif pattern!")
    }
    # absent fields in speciesLink: scientificNameAuthorship and typeStatus
    # Creating field typeStatus
    gbif_data$typeStatus <- NA
    # Creating field scientificNameAuthorship
    author <- stringr::str_trim(sapply(gbif_data$scientificName, function(x) gsub(flora::remove.authors(x), "", x)))
    gbif_data$scientificNameAuthorship <- author
  }

  # removing fields if drop = TRUE
  if (drop) {
    gbif_data <- gbif_data[, names(gbif_data) %in% must]
    splink_data <- splink_data[, names(splink_data) %in% must]
    user_data <- user_data[, names(user_data) %in% must]
    message("Droping fields not used in the data cleaning!")
  }

  # binding data ---------------------------------------------------------------

  if (bind_data) {
    # Forcing numeric columns to be numeric
    numerics <- c("year", "month", "day", "decimalLatitude", "decimalLongitude",
                  "individualCount")
    for (i in numerics) {
      splink_data[, i] <- as.numeric(splink_data[, i])
      gbif_data[, i] <- as.numeric(gbif_data[, i])
    }

    res_list <- list(gbif = gbif_data, speciesLink = splink_data, user = user_data)
    res_list <- dplyr::bind_rows(res_list[sapply(res_list, function(x) !is.null(x))], .id = "data_source")

  } else {
    res_list <- list(gbif = gbif_data, speciesLink = splink_data, user = user_data)
  }
  warning("\n The field 'county' was replaced by 'municipality'")
  return(res_list)
}
