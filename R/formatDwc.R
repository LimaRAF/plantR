#' @title Formatting and binding databases using DarwinCore fields
#'
#' @description Formats fields in the occurrence data frame - either downloaded
#'   from a database or provided by the user - according to the DarwinCore
#'   standard. Optionally, drops fields not used in data cleaning performed by
#'   plantR and do some text re-encoding. In addition, this functions can bind
#'   different sources of data after formatting.
#'
#' @param splink_data A data frame as in the output from
#'   `rspeciesLink()`
#' @param gbif_data A data frame as in the output from `rgbif()` or
#'   `rgbif2()`
#' @param bien_data A data frame as in the output from the
#'   `BIEN_occurrence`-function family
#' @param user_data A data frame provided by the user. Minimum fields are:
#' `c("collectionCode", "catalogNumber", "recordNumber", "recordedBy", "year",
#' "country", "stateProvince", "county", "municipality", "decimalLatitude",
#' "decimalLongitude", "identifiedBy", "dateIdentified", "typeStatus",
#' "scientificName", "scientificNameAuthorship", "institutionCode")`. Fields can
#'  be placed at any order and any given name. If using `user_data` argument,
#'  the user must indicate the name of the column for each field if not already
#'  in DwC standard
#' @param bind_data Logical. Either to bind data from different
#'   sources after formatting
#' @param drop Logical. Either to drop non-essential fields to the
#'   data cleaning routine performed by plantR
#' @param drop.opt Logical. Either to drop optional fields in the data
#'   cleaning routine performed by plantR
#' @param drop.empty Logical. Either to drop fields where all values
#'   are NA
#' @param fix.encoding Character. The name of the input data frame
#'   (i.e. 'splink_data', 'gbif_data', 'bien_data', 'user_data') that
#'   common encoding problems from text in 'latin1' should be
#'   re-encoded to UTF-8. Default to NULL (no re-encoding is
#'   performed)
#' @param institutionCode The name of the column containing The name
#'   (or acronym) in use by the institution having custody of the
#'   object(s) or information referred to in the record
#' @param collectionCode The name of the column containing the name,
#'   acronym, coden, or initialism identifying the collection or data
#'   set from which the record was derived
#' @param catalogNumber The name of the column containing an
#'   identifier for the record within the data set or collection
#' @param recordNumber The name of the column containing an identifier
#'   given to the Occurrence at the time it was recorded. Often serves
#'   as a link between field notes and an Occurrence record, such as a
#'   specimen collector's number
#' @param recordedBy The name of the column containing a person,
#'   group, or organization responsible for recording the original
#'   Occurrence
#' @param year The name of the column containing the four-digit year
#'   in which the Event occurred, according to the Common Era Calendar
#' @param country The name of the column containing the name of the
#'   country or major administrative unit in which the Location occurs
#' @param stateProvince The name of the column containing the name of
#'   the next smaller administrative region than country (state,
#'   province, canton, department, region, etc.) in which the Location
#'   occurs
#' @param county The name of the column containing the full,
#'   unabbreviated name of the next smaller administrative region than
#'   stateProvince (county, shire, department, etc.) in which the
#'   Location occurs
#' @param municipality The name of the column containing the full,
#'   unabbreviated name of the next smaller administrative region than
#'   county (city, municipality, etc.) in which the Location occurs.
#'   Do not use this term for a nearby named place that does not
#'   contain the actual location
#' @param locality The name of the column containing the specific
#'   description of the place in which the Location occurs.
#' @param decimalLatitude The name of the column containing the
#'   geographic latitude (in decimal degrees, using the spatial
#'   reference system given in geodeticDatum) of the geographic center
#'   of a Location. Positive values are north of the Equator, negative
#'   values are south of it. Legal values lie between -90 and 90,
#'   inclusive
#' @param decimalLongitude The name of the column containing the
#'   geographic longitude (in decimal degrees, using the spatial
#'   reference system given in geodeticDatum) of the geographic center
#'   of a Location. Positive values are east of the Greenwich
#'   Meridian, negative values are west of it. Legal values lie
#'   between -180 and 180, inclusive
#' @param identifiedBy The name of the column containing a list
#'   (concatenated and separated) of names of people, groups, or
#'   organizations who assigned the Taxon to the subject
#' @param dateIdentified The name of the column containing the date on
#'   which the subject was identified as representing the Taxon
#' @param typeStatus The name of the column containing a nomenclatural
#'   type (type status, typified scientific name, publication) applied
#'   to the subject
#' @param family The name of the column containing the family in which
#'   the taxon is classified.
#' @param scientificName The name of the column containing the full
#'   scientific name, with authorship and date information if known.
#'   When forming part of an Identification, this should be the name
#'   in lowest level taxonomic rank that can be determined. This term
#'   should not contain identification qualifications, which should
#'   instead be supplied in the IdentificationQualifier term
#' @param scientificNameAuthorship The name of the column containing
#'   the authorship information for the scientificName formatted
#'   according to the conventions of the applicable nomenclaturalCode
#'
#' @return Either a data.frame or list with the database fields
#'   formatted following DarwinCore standards
#'
#' @import data.table
#' @importFrom dplyr bind_rows
#' @importFrom stats na.omit
#' @importFrom utils tail
#'
#' @author Sara R. Mortara, Andrea Sánchez-Tapia & Renato A. F. de Lima
#'
#' @encoding UTF-8
#'
#' @export formatDwc
#'
formatDwc <- function(splink_data = NULL,
                      gbif_data = NULL,
                      bien_data = NULL,
                      user_data = NULL,
                      bind_data = TRUE,
                      drop = FALSE,
                      drop.opt = FALSE,
                      drop.empty = FALSE,
                      fix.encoding = NULL,
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
                      locality = "locality",
                      decimalLatitude = "decimalLatitude",
                      decimalLongitude = "decimalLongitude",
                      identifiedBy = "identifiedBy",
                      dateIdentified = "dateIdentified",
                      typeStatus = "typeStatus",
                      family = "family",
                      scientificName = "scientificName",
                      scientificNameAuthorship = "scientificNameAuthorship") {

  # Required fields by plantR
  must <-
    sort(unique(stats::na.omit(fieldNames$plantr[fieldNames$type %in%
                                                   "required"])))
  opt <-
    sort(unique(stats::na.omit(fieldNames$plantr[fieldNames$type %in%
                                                   "optional"])))

  # formating user data --------------------------------------------------------
  if (!is.null(user_data)) {
    user_colnames <- c(institutionCode, collectionCode,
                       catalogNumber,
                       recordNumber, recordedBy,
                       year, country, stateProvince, county,
                       municipality, locality,
                       decimalLatitude, decimalLongitude,
                       identifiedBy, dateIdentified,
                       typeStatus, family, scientificName,
                       scientificNameAuthorship)
    names(user_colnames) <- c("institutionCode", "collectionCode",
                              "catalogNumber",
                              "recordNumber", "recordedBy",
                              "year", "country", "stateProvince", "county",
                              "municipality", "locality",
                              "decimalLatitude", "decimalLongitude",
                              "identifiedBy", "dateIdentified",
                              "typeStatus", "family", "scientificName",
                              "scientificNameAuthorship")
    if (any(!user_colnames %in% names(user_data)))
      stop("user_data does not have the minimum fields required for data cleaning!")

    # Applying DwC names
    names(user_data)[match(user_colnames, names(user_data))] <-
      names(user_colnames)

    if (!is.null(fix.encoding)) {
      fix.cols <- c("recordedBy", "country", "stateProvince", "county",
                    "municipality", "locality", "identifiedBy",
                    "fieldNotes", "occurrenceRemarks", "habitat")
      user.cols <- colnames(user_data)[colnames(user_data) %in% fix.cols]
      if (any(c('user_data', 'user') %in% fix.encoding)) {
        for (i in seq_along(user.cols)) {
          bad_enc <- badEncoding
          replace_these <- grepl(paste0(bad_enc, collapse = "|"),
                                 user_data[, user.cols[i]], perl = TRUE)
          if (any(replace_these))
            user_data[replace_these, user.cols[i]] <-
              fixEncoding(user_data[replace_these, user.cols[i]])
        }
      }
    }

  }

  # formating speciesLink data -------------------------------------------------
  if (!is.null(splink_data)) {

    # Applying DwC names
    splink_equiv <- fieldNames[!is.na(fieldNames$speciesLink) &
                               !is.na(fieldNames$type), ]
    splink_equiv <- splink_equiv[splink_equiv$speciesLink %in% names(splink_data), ]
    names(splink_data)[match(splink_equiv$speciesLink, names(splink_data), nomatch = 0)] <-
      splink_equiv$plantr

    # required absent fields in speciesLink: municipality and dateIdentified
    miss.cols <- must[!must %in% names(splink_data)]

    # Creating field municipality
    if (!"municipality" %in% names(splink_data))
      splink_data$municipality <- NA_character_
    # Creating field dateIdentified
    if (!"monthIdentified" %in% names(splink_data))
      splink_data$monthIdentified <- NA_character_
    if (!"dayIdentified" %in% names(splink_data))
      splink_data$dayIdentified <- NA_character_

    splink_date <- apply(X = splink_data[, c("yearIdentified",
                                             "monthIdentified",
                                             "dayIdentified")],
                         MARGIN = 1,
                         FUN = function(x) paste(x, collapse = "-"))
    splink_data$dateIdentified <-
      ifelse(grepl("NA-NA-NA", splink_date), NA_character_, splink_date)

    if (!"decimalLatitude" %in% names(splink_data))
      splink_data$decimalLatitude <- NA_character_
    if (!"decimalLongitude" %in% names(splink_data))
      splink_data$decimalLongitude <- NA_character_
    if (!"year" %in% names(splink_data))
      splink_data$year <- NA_character_

    # optional absent fields
    if (!drop.opt) {
      miss.cols.opt <- opt[!opt %in% names(splink_data)]
      for (i in seq_along(miss.cols.opt))
        splink_data[, miss.cols.opt[i]] <- NA_character_
    }

    # checking name standards (only for mandatory and optional fields)
    splink_cols <-
      sort(unique(stats::na.omit(fieldNames$plantr[!is.na(fieldNames$type)])))
    if (any(!splink_cols %in% names(splink_data))) {
      # stop("splink_data does not follow the speciesLink pattern")
      colunas <- splink_cols[!splink_cols %in% names(splink_data)]
      aviso <- paste("Important columns were not found in the splink_data: \n",
                     paste(colunas, collapse = ", "))
      warning(aviso, call. = FALSE)
    }

    # Encoding issues
    if (!is.null(fix.encoding)) {
      fix.cols <- c("recordedBy", "country", "stateProvince",
        "locality", "identifiedBy", "county", "verbatimLocality",
        "fieldNotes", "occurrenceRemarks", "habitat", "datasetName")
      splink.cols <- colnames(splink_data)[colnames(splink_data) %in% fix.cols]
      if (any(c('splink_data', 'splink', 'speciesLink') %in% fix.encoding)) {
        for (i in seq_along(splink.cols)) {
          bad_enc <- badEncoding
          replace_these <- grepl(paste0(bad_enc, collapse = "|"),
                                 splink_data[, splink.cols[i]], perl = TRUE)
          if (any(replace_these))
            splink_data[replace_these, splink.cols[i]] <-
              fixEncoding(splink_data[replace_these, splink.cols[i]])
        }
      }
    }

  }

  # formating gbif data --------------------------------------------------------
  if (!is.null(gbif_data)) {
    # fixing problematic GBIF names
    tmp <- names(gbif_data)[grepl("\\.\\.", names(gbif_data), perl = TRUE)]
    tmp1 <- sapply(tmp,
                   function(x)
                     utils::tail(unlist(strsplit(x, "([a-z])\\.(?=[a-zA-Z])",
                                                 perl = TRUE)), n = 1))
    tmp[!duplicated(tmp1) & !tmp1 %in% names(gbif_data)] <-
      tmp1[!duplicated(tmp1) & !tmp1 %in% names(gbif_data)]
    names(gbif_data)[grepl("\\.\\.", names(gbif_data), perl = TRUE)] <- tmp

    # required absent fields in gbif: scientificNameAuthorship
    miss.cols <- must[!must %in% names(gbif_data)]

    # optional absent fields
    if (!drop.opt) {
      miss.cols.opt <- opt[!opt %in% names(gbif_data)]
      for (i in seq_along(miss.cols.opt))
        gbif_data[, miss.cols.opt[i]] <- NA_character_
    }

    # checking name standards (only mandatory and optional fields)
    gbif_cols <-
      sort(unique(stats::na.omit(fieldNames$gbif[!is.na(fieldNames$type)])))
    if (any(!gbif_cols %in% names(gbif_data))) {
      #stop("gbif_data does not follow the gbif pattern!")
      colunas <- gbif_cols[!gbif_cols %in% names(gbif_data)]
      aviso <- paste("Important columns were not found in the gbif_data: \n",
                     paste(colunas, collapse = ", "))
      warning(aviso, call. = FALSE)
    }

    # Encoding issues
    if (!is.null(fix.encoding)) {
      fix.cols <- c("recordedBy", "country", "stateProvince", "municipality",
                    "locality", "identifiedBy", "county", "verbatimLocality",
                    "fieldNotes", "occurrenceRemarks", "habitat", "datasetName")
      gbif.cols <- colnames(gbif_data)[colnames(gbif_data) %in% fix.cols]
      if (any(c('gbif_data', 'gbif', 'GBIF') %in% fix.encoding)) {
        for (i in seq_along(gbif.cols)) {
          bad_enc <- badEncoding
          replace_these <- grepl(paste0(bad_enc, collapse = "|"),
                                 gbif_data[, gbif.cols[i]], perl = TRUE)
          if (any(replace_these))
            gbif_data[replace_these, gbif.cols[i]] <-
              fixEncoding(gbif_data[replace_these, gbif.cols[i]])
        }
      }
    }

  }

  # formating BIEN data --------------------------------------------------------
  if (!is.null(bien_data)) {

    # Applying DwC names
    bien_equiv <- fieldNames[!is.na(fieldNames$bien) &
                               !is.na(fieldNames$type), ]
    bien_equiv <- bien_equiv[bien_equiv$bien %in% names(bien_data), ]
    names(bien_data)[match(bien_equiv$bien, names(bien_data))] <-
      bien_equiv$plantr

    # required absent fields in BIEN:
    miss.cols <- must[!must %in% names(bien_data)]
    # Creating field municipality
    bien_data$municipality <- NA_character_
    # Creating field typeStatus
    bien_data$typeStatus <- NA_character_
    # Creating field year
    bien_data$year <- gsub("-.*", "", bien_data$date_collected, perl = TRUE)
    bien_data$day <- gsub(".*-", "", bien_data$date_collected, perl = TRUE)
    bien_data$month <-
      sub("-.*", "",
          sub("....-*", "", bien_data$date_collected, perl = TRUE),
          perl = TRUE)

    if (!"genus" %in% names(bien_data))
      bien_data$genus <- NA_character_
    if (!"family" %in% names(bien_data))
      bien_data$family <- NA_character_

    # optional absent fields
    if (!drop.opt) {
      miss.cols.opt <- opt[!opt %in% names(bien_data)]
      for (i in seq_along(miss.cols.opt))
        bien_data[, miss.cols.opt[i]] <- NA_character_
    }

    # checking name standards (only for mandatory and optional fields)
    bien_cols <-
      sort(unique(stats::na.omit(fieldNames$plantr[!is.na(fieldNames$type)])))
    if (any(!bien_cols %in% names(bien_data))) {
      # stop("splink_data does not follow the speciesLink pattern")
      colunas <- bien_cols[!bien_cols %in% names(bien_data)]
      aviso <- paste("Important columns were not found in the bien_data: \n",
                     paste(colunas, collapse = ", "))
      warning(aviso, call. = FALSE)
      for (i in seq_along(colunas))
        bien_data[, colunas[i]] <- NA_character_
    }

    # Encoding issues
    if (!is.null(fix.encoding)) {
      fix.cols <- c("recordedBy", "country", "stateProvince",
                    "locality", "identifiedBy", "county",
                    "occurrenceRemarks", "datasetName")
      bien.cols <- colnames(bien_data)[colnames(bien_data) %in% fix.cols]
      if (any(c('bien_data', 'bien', 'BIEN') %in% fix.encoding)) {
        for (i in seq_along(bien.cols)) {
          bad_enc <- badEncoding
          replace_these <- grepl(paste0(bad_enc, collapse = "|"),
                                 bien_data[, bien.cols[i]], perl = TRUE)
          if (any(replace_these))
            bien_data[replace_these, bien.cols[i]] <-
              fixEncoding(bien_data[replace_these, bien.cols[i]])
        }
      }
    }

  }


  # removing fields if drop = TRUE ---------------------------------------------
  if (drop) {
    if (drop.opt) {
      gbif_data <- gbif_data[, names(gbif_data) %in% must]
      splink_data <- splink_data[, names(splink_data) %in% must]
      bien_data <- bien_data[, names(bien_data) %in% must]
      user_data <- user_data[, names(user_data) %in% must]
      message("Dropping fields not essential to the data cleaning!")
    } else {
      gbif_data <- gbif_data[, names(gbif_data) %in% sort(unique(c(must, opt)))]
      splink_data <- splink_data[, names(splink_data) %in% sort(unique(c(must, opt)))]
      bien_data <- bien_data[, names(bien_data) %in% sort(unique(c(must, opt)))]
      user_data <- user_data[, names(user_data) %in% sort(unique(c(must, opt)))]
      message("Dropping fields not essential or recommended to the data cleaning!")
    }
  }

  # binding data ---------------------------------------------------------------

  if (bind_data) {
    # Forcing numeric/date columns to be characters to allow binding
    cols_to_force <- c("numeric", "integer", "Date", "POSIXct", "POSIXt",
                       "eventDate", "verbatimEventDate", "dateIdentified")

    if (!is.null(splink_data)) {
      ids <- which(sapply(splink_data, function(x) class(x)[1]) %in%
                     cols_to_force)
      if (length(ids) > 0)
        for (i in ids) splink_data[, i] <- as.character(splink_data[, i])
    }

    if (!is.null(gbif_data)) {
      ids <- which(sapply(gbif_data, function(x) class(x)[1]) %in%
                     cols_to_force)
      if (length(ids) > 0)
        for (i in ids) gbif_data[, i] <- as.character(gbif_data[, i])
    }

    if (!is.null(bien_data)) {
      ids <- which(sapply(bien_data, function(x) class(x)[1]) %in%
                     cols_to_force)
      if (length(ids) > 0)
        for (i in ids) bien_data[, i] <- as.character(bien_data[, i])
    }

    if (!is.null(user_data)) {
      ids <- which(sapply(user_data, function(x) class(x)[1]) %in%
                     cols_to_force)
      if (length(ids) > 0)
        for (i in ids) user_data[, i] <- as.character(user_data[, i])
    }

    res_list <- list(gbif = gbif_data,
                     speciesLink = splink_data,
                     bien = bien_data,
                     user = user_data)
    res_list <- res_list[sapply(res_list, function(x) !is.null(x))]
    res_list <- dplyr::bind_rows(res_list, .id = "data_source")

    if (drop.empty) {
      DT <- data.table::as.data.table(res_list)
      res_list <- data.frame(
        DT[, which(unlist(lapply(DT, function(x) !all(is.na(x))))),
           with = FALSE],
        stringsAsFactors = FALSE)
    }


  } else {
    res_list <- list(gbif = gbif_data,
                     speciesLink = splink_data,
                     bien = bien_data,
                     user = user_data)

    if (drop.empty) {
      for(i in 1:length(list)) {
        DT <- data.table::as.data.table(res_list[[i]])
        res_list[[i]] <- data.frame(
          DT[, which(unlist(lapply(DT, function(x)!all(is.na(x))))),
             with = FALSE],
          stringsAsFactors = FALSE)
      }
    }
  }

  return(res_list)
}
