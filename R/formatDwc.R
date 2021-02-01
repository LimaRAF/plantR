#' @title Formatting and binding databases using DarwinCore fields
#'
#' @description Formats fields in the occurrence data frame -either downloaded
#'   from a database or provided by the user- according to the DarwinCore
#'   standard. Optionally, drops fields not used in data cleaning performed by
#'   plantR. In addition, this functions can bind different sources of data
#'   after formatting.
#'
#' @param splink_data A data frame as in the output from `rspeciesLink()`
#' @param gbif_data A data frame as in the output from `rgbif()` or `rgbif2()`
#' @param user_data A data frame provided by the user. Minimum fields are:
#' `c("collectionCode", "catalogNumber", "recordNumber", "recordedBy", "year",
#' "country", "stateProvince", "county", "municipality", "decimalLatitude",
#' "decimalLongitude", "identifiedBy", "dateIdentified", "typeStatus",
#' "scientificName", "scientificNameAuthorship", "institutionCode")`. Fields can
#'  be placed at any order and any given name. If using `user_data` argument,
#'  the user must indicate the name of the column for each field if not already
#'  in DwC standard
#' @param bind_data Logical. Either to bind data from differents sources after
#'   formatting
#' @param drop Logical. Either to drop non-essential fields to the data cleaning
#'   routine performed by plantR
#' @param drop.opt Logical. Either to drop optional fields in the data cleaning
#'   routine performed by plantR
#' @param drop.empty Logical. Either to drop fields where all values are NA
#' @param institutionCode The name of the column containing The name (or
#'   acronym) in use by the institution having custody of the object(s) or
#'   information referred to in the record
#' @param collectionCode The name of the column containing the name, acronym,
#'   coden, or initialism identifying the collection or data set from which the
#'   record was derived
#' @param catalogNumber The name of the column containing an identifier for the
#'   record within the data set or collection
#' @param recordNumber The name of the column containing an identifier given to
#'   the Occurrence at the time it was recorded. Often serves as a link between
#'   field notes and an Occurrence record, such as a specimen collector's number
#' @param recordedBy The name of the column containing a person, group, or
#'   organization responsible for recording the original Occurrence
#' @param year The name of the column containing the four-digit year in which
#'   the Event occurred, according to the Common Era Calendar
#' @param country The name of the column containing the name of the country or
#'   major administrative unit in which the Location occurs
#' @param stateProvince The name of the column containing the name of the next
#'   smaller administrative region than country (state, province, canton,
#'   department, region, etc.) in which the Location occurs
#' @param county The name of the column containing the full, unabbreviated name
#'   of the next smaller administrative region than stateProvince (county,
#'   shire, department, etc.) in which the Location occurs
#' @param municipality The name of the column containing the full, unabbreviated
#'   name of the next smaller administrative region than county (city,
#'   municipality, etc.) in which the Location occurs. Do not use this term for
#'   a nearby named place that does not contain the actual location
#' @param decimalLatitude The name of the column containing the geographic
#'   latitude (in decimal degrees, using the spatial reference system given in
#'   geodeticDatum) of the geographic center of a Location. Positive values are
#'   north of the Equator, negative values are south of it. Legal values lie
#'   between -90 and 90, inclusive
#' @param decimalLongitude The name of the column containing the geographic
#'   longitude (in decimal degrees, using the spatial reference system given in
#'   geodeticDatum) of the geographic center of a Location. Positive values are
#'   east of the Greenwich Meridian, negative values are west of it. Legal
#'   values lie between -180 and 180, inclusive
#' @param identifiedBy The name of the column containing alist (concatenated and
#'   separated) of names of people, groups, or organizations who assigned the
#'   Taxon to the subject
#' @param dateIdentified The name of the column containing the date on which the
#'   subject was identified as representing the Taxon
#' @param typeStatus The name of the column containing a nomenclatural type
#'   (type status, typified scientific name, publication) applied to the subject
#' @param scientificName The name of the column containing the full scientific
#'   name, with authorship and date information if known. When forming part of
#'   an Identification, this should be the name in lowest level taxonomic rank
#'   that can be determined. This term should not contain identification
#'   qualifications, which should instead be supplied in the
#'   IdentificationQualifier term
#' @param scientificNameAuthorship The name of the column containing the
#'   authorship information for the scientificName formatted according to the
#'   conventions of the applicable nomenclaturalCode
#'
#' @return Either a data.frame or list with the database fields formatted
#'   following DarwinCore standards
#'
#' @importFrom stringr str_trim
#' @importFrom flora remove.authors
#' @importFrom dplyr bind_rows
#' @importFrom stats na.omit
#' @import data.table
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
                      drop.opt = FALSE,
                      drop.empty = FALSE,
                      institutionCode = "institutionCode",
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
                      scientificNameAuthorship = "scientificNameAuthorship") {

  # Required fields by plantR
  must <- sort(unique(stats::na.omit(fieldNames$plantr[fieldNames$type %in% "required"])))
  opt <- sort(unique(stats::na.omit(fieldNames$plantr[fieldNames$type %in% "optional"])))

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
    # checking name standards
    splink_cols <- sort(unique(stats::na.omit(fieldNames$speciesLink)))
    if (any(!splink_cols %in% names(splink_data))) {
      stop("splink_data does not follow the speciesLink pattern!")
    }
    # required absent fields in speciesLink: municipality and dateIdentified
    miss.cols <- must[!must %in% names(splink_data)]
    # Creating field municipality
    splink_data$municipality <- NA_character_
    # Creating field dateIdentified
    splink_date <- apply(X = splink_data[, c("yearIdentified", "monthIdentified", "dayIdentified")],
                         MARGIN = 1,
                         FUN = function(x) paste(x, collapse = "-"))
    splink_data$dateIdentified <- ifelse(grepl("NA-NA-NA", splink_date), NA_character_, splink_date)

    # optional absent fields
    if (!drop.opt) {
      miss.cols.opt <- opt[!opt %in% names(splink_data)]
      for (i in 1:length(miss.cols.opt)) splink_data[, miss.cols.opt[i]] <- NA_character_
    }

  }

  # formating gbif data --------------------------------------------------------
  if (!is.null(gbif_data)) {
    # fixing problematic GBIF names
    tmp <- names(gbif_data)[grepl("\\.", names(gbif_data), perl = TRUE)]
    tmp1 <- sapply(tmp,
                   function(x) my.tail(unlist(strsplit(x, "([a-z])\\.(?=[a-zA-Z])", perl = TRUE))))
    tmp[!duplicated(tmp1) & !tmp1 %in% names(gbif_data)] <-
      tmp1[!duplicated(tmp1) & !tmp1 %in% names(gbif_data)]
    #Sara: nem todos os nomes podem ser limpos pois removendo o longo prefixo, alguns campos ficam duplicados.
    #Seria legal entender o que está rolando, pois esses campos as vezes tem informacao tb.
    names(gbif_data)[grepl("\\.", names(gbif_data), perl = TRUE)] <- tmp

    # checking name standards
    gbif_cols <- sort(unique(stats::na.omit(fieldNames$gbif)))
    if (any(!gbif_cols %in% names(gbif_data))) {
      stop("gbif_data does not follow the gbif pattern!")
    }
    # required absent fields in gbif: scientificNameAuthorship
    miss.cols <- must[!must %in% names(gbif_data)]
    # Creating field scientificNameAuthorship
    author <- stringr::str_trim(sapply(gbif_data$scientificName,
                                       function(x) gsub(flora::remove.authors(x), "", x, perl = TRUE)))
    gbif_data$scientificNameAuthorship <- author

    # optional absent fields
    if (!drop.opt) {
      miss.cols.opt <- opt[!opt %in% names(gbif_data)]
      for (i in 1:length(miss.cols.opt)) gbif_data[, miss.cols.opt[i]] <- NA_character_
    }

  }

  # removing fields if drop = TRUE
  if (drop) {
    if (drop.opt) {
      gbif_data <- gbif_data[, names(gbif_data) %in% must]
      splink_data <- splink_data[, names(splink_data) %in% must]
      user_data <- user_data[, names(user_data) %in% must]
      message("Dropping all fields not essential to the data cleaning!")
    } else {
      gbif_data <- gbif_data[, names(gbif_data) %in% sort(unique(c(must, opt)))]
      splink_data <- splink_data[, names(splink_data) %in% sort(unique(c(must, opt)))]
      user_data <- user_data[, names(user_data) %in% sort(unique(c(must, opt)))]
      message("Dropping all fields not essential or recommended to the data cleaning!")
    }
  }

  # binding data ---------------------------------------------------------------

  if (bind_data) {
    # Forcing numeric columns to be characters
    numerics <- c("year", "month", "day", "decimalLatitude", "decimalLongitude", "individualCount")
    numerics <- numerics[numerics %in%
                           unique(c(names(splink_data),
                                    c(names(gbif_data))))]
    for (i in numerics) {
      splink_data[, i] <- as.character(splink_data[, i])
      gbif_data[, i] <- as.character(gbif_data[, i])
      # splink_data[, i] <- as.numeric(splink_data[, i])
      # gbif_data[, i] <- as.numeric(gbif_data[, i])
    }

    res_list <- list(gbif = gbif_data, speciesLink = splink_data, user = user_data)
    res_list <- res_list[sapply(res_list, function(x) !is.null(x))]
    res_list <- dplyr::bind_rows(res_list, .id = "data_source")

    if (drop.empty) { ### ADDED BY RENATO: REMOVE ALL-NAs COLUMNS? NECESSÁRIO? PARA E.EDULIS NÃO, MAS TESTAR COM MULTIPLAS ESPÉCIES...
      DT <- data.table::as.data.table(res_list)
      res_list <- data.frame(
        DT[, which(unlist(lapply(DT, function(x) !all(is.na(x))))), with = FALSE],
        stringsAsFactors = FALSE)
    }


  } else {
    res_list <- list(gbif = gbif_data, speciesLink = splink_data, user = user_data)

    if (drop.empty) { ### ADDED BY RENATO: REMOVE ALL-NAs COLUMNS? NECESSÁRIO? PARA E.EDULIS NÃO, MAS TESTAR COM MULTIPLAS ESPÉCIES...
      for(i in 1:length(list)) {
        DT <- data.table::as.data.table(res_list[[i]])
        res_list[[i]] <- data.frame(
          DT[, which(unlist(lapply(DT, function(x)!all(is.na(x))))), with = FALSE],
          stringsAsFactors = FALSE)
      }
    }
  }
  return(res_list)
}
