#' @title Create Species Checklist
#'
#' @description This function creates a check-list of the species contained
#' in the occurrence data, including a list of voucher specimens
#'
#' @param x a data frame with the occurrence data, generally as the output of the
#'   __plantR__ validation functions.
#' @param fam.order logical. Should taxa be organized in alphabetical order
#'   of families? Defaults to TRUE.
#' @param n.vouch numerical. Maximum number of vouchers to be listed per taxa.
#'   Defaults to 30.
#' @param type character. The type of voucher list desired. Options are: 'short',
#' 'selected' and 'list' (see details below).
#' @param rm.dup logical. Should duplicated specimens be removed prior to the
#'   calculation of species summaries? Defaults to TRUE.
#' @param rank.type numerical. Value of the ranking for type specimens in order
#'   to organize order and filter the voucher list. Defaults to 5.
#' @param date.format The desired format for the dates. Defaults to "%d %b %Y"
#'
#' @details The check-list can be organized in alphabetic order by taxa or in
#'   alphabetic order by family and then by taxa within families (the
#'   default).
#'
#'   By default, the check-list provides the number of records found and the
#'   overall taxonomic and geographic confidence level of the records (columns
#'   'tax.CL' and 'geo.CL'), if available. The taxonomic confidence level is the
#'   percentage of records with the identification flagged as 'high', while the
#'   geographic confidence level is the percentage of records with coordinates
#'   flagged as being validated at municipality or locality levels.
#'
#'   The function also provides a list of vouchers, giving priority to type
#'   specimens and records with higher level of confidence in their
#'   identification. By default, the function provides up to 30 vouchers
#'   per taxa, but this number can be controlled using the argument `n.vouch`.
#'
#'   The voucher list can be provided in the following output formats (the
#'   option 'list' is not implemented yet):
#'
#'   + 'short': Collector name, Collector number (collections of deposit)
#'   + 'selected': COUNTRY, stateProvince: municipality, Date, Collector name,
#'   Collector number (collections of deposit)
#'   + 'list': Collector name, Collector number(s) (species code)
#'
#'   Note 1: although we provide a `date.format` argument, checks of other date
#'   formats other than the default are pending and so they may not work
#'   properly.
#'
#'   Note 2: The columns names of the input data are expected to be in the
#'   DarwinCore format or in the standard output names of the __plantR__ workflow.
#'   Currently, there is no argument to make the equivalency to different column
#'   names, so users need to convert their data into one of these two options. See
#'   function `formatDwc()` for more details.
#'
#' @examples
#' (df <- data.frame(collectionCode = c("CRI","CRI","CRI","CRI"),
#' catalogNumber = c("3565","713","3073","15331"),
#' recordedBy = c("Rebelo, M.C.","Citadini-Zanette, V.",
#' "Santos, R.","Zapelini, I."),
#' recordNumber = c("s.n.","1063","11","s.n."),
#' year = c("1994","1990","1994","2020"),
#' family = c("Salicaceae","Salicaceae","Cannabaceae","Cannabaceae"),
#' scientificName = c("Casearia sylvestris","Casearia sylvestris",
#' "Trema micrantha","Trema micrantha"),
#' country = c("brazil","brazil","brazil","brazil"),
#' stateProvince = c("santa catarina","santa catarina",
#' "santa catarina","santa catarina"),
#' municipality = c("jaguaruna","orleans","icara",NA)))
#'
#' checkList(df, rm.dup = FALSE)
#' checkList(df, rm.dup = FALSE, type = "selected")
#'
#'
#' @import data.table
#' @importFrom stringr str_replace str_trim
#'
#' @export checkList
#'
checkList <- function(x, fam.order = TRUE, n.vouch = 30, type = "short",
                      rm.dup = TRUE, rank.type = 5, date.format = "%d %b %Y") {

  # check input
  if (!class(x) == "data.frame")
    stop("Input object needs to be a data frame!")

  #Escaping R CMD check notes from using data.table syntax
  dup.ID <- N <- N.all <- temp.geo.check <- priority <- NULL
  typeStatus <- temp.rec.numb <- ordem <- temp.pais <- NULL
  coletores <- temp.accession <- numTombo <- vchrs <- NULL
  datas <- datas.tipo <- lista.vouchs <- . <- NULL

  ## PREPARING THE TABLE ##
  # Select which co-variables will be used in the summary (priority to the edited columns)
  covs <- list(collections = c("collectionCode.new", "collectionCode"),
               catalog = c("catalogNumber.new", "catalogNumber"),
               collectors = c("recordedBy.new", "recordedBy"),
               recordNumber = c("recordNumber.new", "recordNumber"),
               colYears = c("year.new", "year"),
               families = c("family.new", "family"),
               species = c("scientificName.new", "scientificName"),
               countries = c("country.new", "country"),
               state = c("stateProvince.new", "stateProvince"),
               county = c("municipality.new", "municipality"),
               locality = c("loc.correct1","loc.correct","loc"),
               coordinates = c("geo.check1", "geo.check"),
               taxonomy = c("tax.check1", "tax.check"))

  #Get only the columns of interest
  covs.present <- lapply(covs, function(z) z[which(z %in% names(x))][1])
  covs.present[sapply(covs.present, identical, character(0))] <- NA
  if (all(sapply(covs.present, nchar)==0))
    stop("The input data frame does not contain at least one of the required columns")

  # Getting the input columns to be used for filtering
  other.cols <- c("dup.ID", "dup.prop","typeStatus", "scientific.name",
                  "numTombo", "temp.accession", "month", "day")
  covs.final <- c(unlist(covs.present), other.cols)

  # Should the duplicates be removed?
  if (rm.dup) {
    if ("numTombo" %in% names(x)) {
      dt <- data.table::data.table(suppressMessages(
                  rmDup(x[, names(x) %in% covs.final])))
    } else {
      dt <- data.table::data.table(x[, names(x) %in% covs.final])
      warning("Duplicated specimens cannot be removed; using the all occurrences instead")
    }
  } else {
    dt <- data.table::data.table(x[, names(x) %in% covs.final])
  }

  #Making sure the data.table does not contains factors
  changeCols <- colnames(dt)[which(as.vector(dt[,lapply(.SD, class)]) == "factor")]
  if (length(changeCols) > 0)
    dt[,(changeCols):= lapply(.SD, as.character), .SDcols = changeCols]

  # getting the list of taxa and the selected columns
  data.table::setindexv(dt, covs.present[["species"]])
  checklist <- data.frame(unique(dt, by= covs.present[["species"]]))
  cols <- c(unlist(covs.present[names(covs.present) %in% c("families", "species")]),
            "scientific.name")
  checklist <- checklist[, names(checklist) %in% cols]
  checklist$records <- NA
  checklist$tax.CL <- NA
  checklist$geo.CL <- NA
  checklist$vouchers <- NA

  ## NUMBER OF RECORS PER SPECIES ##
  records <- dt[, .N , by = c(covs.present[["species"]])]

  if ("dup.ID" %in% names(dt)) {

    unicatas <- dt[is.na(dup.ID), .N , by = c(covs.present[["species"]])]
    unicatas <- merge(records, unicatas, by = c(covs.present[["species"]]),
                      all.x = TRUE, suffixes = c("", ".unis"))
    unicatas[, N := NULL]

    duplicatas <- dt[!is.na(dup.ID), .N, by = c(covs.present[["species"]], "dup.ID")]
    duplicatas <- duplicatas[, .N , by = c(covs.present[["species"]])]
    duplicatas <- data.table::merge.data.table(records, duplicatas, by = c(covs.present[["species"]]),
                                               all.x = TRUE, suffixes = c("", ".dups"))
    duplicatas[, N:= NULL]

    records <- records[unicatas, on = c(covs.present[["species"]])]
    records <- records[duplicatas, on = c(covs.present[["species"]])]
    checklist$records <-
      records$N[match(checklist[, covs.present[["species"]]],
                      data.frame(records)[,1])]

  } else {
    checklist$records <-
      records$N[match(checklist[, covs.present[["species"]]],
                      data.frame(records)[,1])]
  }


  ## TAXONOMIC CONFIDENCE LEVEL  ##

  if (!is.na(covs.present[["taxonomy"]])) {
    # Proportion of validate identifications per species
    colunas = c(covs.present[["taxonomy"]], covs.present[["species"]])
    data.table::setkeyv(dt, cols = colunas)
    taxs <- dt[, .N,
               by = c(covs.present[["species"]], covs.present[["taxonomy"]])]
    #dt[data.table::CJ(tax.check1, scientificName.new, unique = TRUE), .N, by = .EACHI]

    vals <- c("unknown", "low", "medium", "high")
    all.taxs <- data.table::CJ(unique(data.frame(taxs)[,1]),
                               vals, 0, unique = TRUE)
    names(all.taxs) <- names(taxs)
    all.taxs <- merge(all.taxs, taxs, by = c(covs.present[["species"]], covs.present[["taxonomy"]]),
                      all.x = TRUE, suffixes = c(".all", ""))
    all.taxs <- all.taxs[, N.all := NULL]
    all.taxs <- all.taxs[data.frame(all.taxs)[,2] %in% "high", ]
    all.taxs[is.na(N), N := 0]

    # Saving the result
    checklist$tax.CL <- round(100 * all.taxs$N[match(checklist[,1], data.frame(all.taxs)[,1])]/
                                checklist$records, 2)
  }

  ## GEOGRAPHIC CONFIDENCE LEVEL  ##
  if (!is.na(covs.present[["coordinates"]])) {
    # Proportion of valid coordinates (at county or locality levels)
    dt[, temp.geo.check := .SD, .SDcols = c(covs.present[["coordinates"]])]
    dt[, temp.geo.check := gsub("_gazet", "", temp.geo.check)]
    dt[temp.geo.check %in% c("ok_county", "ok_locality"), temp.geo.check := "1"]
    dt[!temp.geo.check %in% "1", temp.geo.check := "0"]

    colunas = c("temp.geo.check", covs.present[["species"]])
    data.table::setkeyv(dt, cols = colunas)
    coords <- dt[, .N, by = colunas]

    coords[is.na(N), N := 0]
    coords <- coords[temp.geo.check %in% "1", ]
    coords <- coords[, temp.geo.check := NULL]
    dt[ , temp.geo.check := NULL]

    checklist$geo.CL <- round(100 * coords$N[match(checklist[,1], data.frame(coords)[,1])]/
                                checklist$records, 2)
  }

  ## SETTING PRIORITIES WITHIN RECORDS OF EACH SPECIES  ##

  # Ranking records according to the completeness of the label information
  dt[ , priority := 0]
  if ("typeStatus" %in% names(dt)) {
    dt[ , typeStatus := tolower(typeStatus), ]
    dt[!is.na(typeStatus) & !grepl("not a type|notatype|probable type|tipo provavel|tipo provavel", typeStatus),
       priority := rank.type, ]
  }

  if (!is.na(covs.present[["collectors"]])) {
    data.table::setkeyv(dt, c(covs.present[["collectors"]]))
    if (dim(dt["s.n.", nomatch = NULL])[1] > 0) {
      suppressWarnings(
        dt[c("s.n."), priority := priority - 3, nomatch = NULL])
    }
    temp <- data.frame(dt[, lapply(.SD, nchar),
                          by = c(covs.present[["collectors"]]), .SDcols = c(covs.present[["collectors"]])])
    dt[ temp[,2] < 4, priority := priority - 3]
  }

  if (!is.na(covs.present[["recordNumber"]])) {
    data.table::setkeyv(dt, c(covs.present[["recordNumber"]]))
    data.table::setnames(dt, covs.present[["recordNumber"]], "temp.rec.numb")
    if (dim(dt[grepl("\\d", temp.rec.numb, perl = TRUE), ])[1] > 0) {
      dt[!grepl("\\d", temp.rec.numb, perl = TRUE), priority := priority - 2]
    }
    data.table::setnames(dt, "temp.rec.numb", covs.present[["recordNumber"]])
  }

  # ranking vouchers in respect to their locality precision
  if (!is.na(covs.present[["county"]])) {
    data.table::setkeyv(dt, c(covs.present[["county"]]))
    dt[.(NA_character_), priority := priority - 1]
  }
  if (!is.na(covs.present[["state"]])) {
    data.table::setkeyv(dt, c(covs.present[["state"]]))
    dt[.(NA_character_), priority := priority - 1]
  }
  if (!is.na(covs.present[["countries"]])) {
    data.table::setkeyv(dt, c(covs.present[["countries"]]))
    dt[.(NA_character_), priority := priority - 2]


    # ranking vouchers in respect to duplicated countries
    dt[, ordem := 1:dim(dt)[1],]
    data.table::setnames(dt, covs.present[["countries"]], "temp.pais")
    temp <- dt[, .(ordem, temp.pais, !duplicated(temp.pais, incomparables = NA_character_)),
               by = c(covs.present[["species"]])]
    temp$V3 <- !unlist(temp$V3)
    # temp$V4 <- !unlist(dt[, .(ordem, temp.pais, !duplicated(temp.pais, incomparables = NA_character_, fromLast = TRUE)),
    #            by = c(covs.present[["species"]])]$V3)
    # temp$check <- temp$V3 | temp$V4
    data.table::setorder(temp, "ordem")
    dt[, ordem := NULL,]
    data.table::setnames(dt, "temp.pais", covs.present[["countries"]])
    dt[temp$V3, priority := priority - 2]
  }

  # ranking vouchers in respect to their taxonomic confidence
  if (!is.na(covs.present[["taxonomy"]])) {
    data.table::setkeyv(dt, c(covs.present[["taxonomy"]]))
    if (dim(dt[.(c("unknown", "low")), ])[1] > 0) {
      dt[ .(c("unknown", "low")), priority := priority - 3]
    }
    if (dim(dt[.(c("medium")), ])[1] > 0) {
      dt[ .(c("medium")), priority := priority - 2]
    }
  }

  # ranking vouchers in respect to the existence of an accession number
  if (!is.na(covs.present[["catalog"]])) {
    data.table::setkeyv(dt, c(covs.present[["catalog"]]))
    dt[.(NA_character_), priority := priority - 3]
  }

  # ranking vouchers in respect to the existence of a collection year
  if (!is.na(covs.present[["colYears"]])) {
    data.table::setkeyv(dt, c(covs.present[["colYears"]]))
    dt[.(c(NA_character_, "n.d.")), priority := priority - 2]
  }

  # Still too many vouchers per species?
  # Add extra steps to downgrad vouchers from the same author,
  #or with bad coordinates for species with too many vouchers of high priority
  #from the same collector or same county?

  # Organizing and filtering records based on the ranks by species
  data.table::setorderv(dt, c(covs.present[["species"]], "priority"), c(1,-1))
  dt1 <- dt[dt[, .I[1:n.vouch],
               by = c(covs.present[["species"]])]$V1]
  dt1 <- dt1[rowSums(is.na(dt1)) < dim(dt1)[2],]

  ## GENERATING THE LIST OF VOUCHERS ##
  # Collector name and number
  dt1[ , coletores := do.call(paste, c(.SD, sep=", ")),
       .SDcols = c(covs.present[["collectors"]], covs.present[["recordNumber"]])]

  if (type == "short") { # Inspired in the Flora do Brail format

    ## Getting more up-to-date accession numbers, if available
    if ("numTombo" %in% names(x))
      dt1[, temp.accession := toupper(gsub("_", " ", numTombo, perl = TRUE))]

    ## Accession numbers
    if (!"temp.accession" %in% names(dt1))
      dt1[ , temp.accession := do.call(paste, c(.SD, sep=", ")),
           .SDcols = c(covs.present[["collections"]], covs.present[["catalog"]])]

    #correcting accessions numbers for duplicates across herbaria
    if (dim(dt1[!is.na(dup.ID)])[1] > 0) {
      getDupIDs <- function(id) {
        id <- toupper(gsub("_", " ", id, perl = TRUE))
        id <- sapply(lapply(strsplit(id, "\\|"), unique), paste, collapse = "|")
        id <- gsub("\\|", ", ", id, perl = TRUE)
        return(id)
      }
      dt1[!is.na(dup.ID) , temp.accession := as.character(lapply(dup.ID, getDupIDs))]
    }
    dt1[ , temp.accession := paste("(", temp.accession, ")", sep="")]

    #getting the voucher vector itslef
    dt1[ , vchrs := do.call(paste, c(.SD, sep=" ")),
         .SDcols = c("coletores", "temp.accession")]

    if ("typeStatus" %in% names(dt)) {
      dt1[!is.na(typeStatus) & !grepl("not a type|notatype|probable type|tipo provavel|tipo provavel", typeStatus),
          vchrs := paste(vchrs, " [",tolower(typeStatus),"]", sep="")]
    }

    #combining all voucher into a single string
    data.table::setorderv(dt1, c(covs.present[["species"]], "vchrs"), c(1,1))
    dt2 <- dt1[ , do.call(paste, c(.SD, collapse= "; ",sep="")),
                by = c(covs.present[["species"]]),
                .SDcols = "vchrs"]

    checklist$vouchers <-
      as.character(dt2$V1[match(checklist[,covs.present[["species"]]],
                                data.frame(dt2)[,1])])
  }

  if (type == "selected") { # From 'species examined' in Flora Neotropica

    # VOUCHERS
    #getting more up-to-date collectionCode, if available
    if ("numTombo" %in% names(x)) {
      dt1[, temp.accession := toupper(gsub("_", " ", numTombo, perl = TRUE))]
      dt1[, temp.accession := lapply(strsplit(temp.accession, " "), my.head)]
    }

    #collectionCode
    if (!"temp.accession" %in% names(dt1))
      dt1[ , temp.accession := .SD,
           .SDcols = c(covs.present[["collections"]])]

    #correcting accessions numbers for duplicates across herbaria
    if ("dup.ID" %in% names(dt1)) {
      getDupIDs1 <- function(id) {
        id <- toupper(gsub("_", " ", id, perl = TRUE))
        id <- sapply(lapply(strsplit(id, "\\|"), strsplit, " ")[[1]], my.head)
        id <- paste0(unique(id), collapse = ", ")
        return(id)
      }
      dt1[!is.na(dup.ID) , temp.accession := lapply(dup.ID, getDupIDs1)]
    }
    dt1[ , temp.accession := paste("(", temp.accession, ")", sep="")]

    #getting the voucher vector itself
    dt1[ , vchrs := do.call(paste, c(.SD, sep=" ")),
         .SDcols = c("coletores", "temp.accession")]

    # if ("typeStatus" %in% names(dt)) {
    #   dt1[!is.na(typeStatus) & !grepl("not a type|notatype|probable type|tipo provavel|tipo provavel", typeStatus),
    #       vchrs := paste(vchrs, " [",tolower(typeStatus),"]", sep="")]
    # }

    # LOCALITY
    locais = NULL
    if (is.na(covs.present[["locality"]]) &
        any(!sapply(covs.present[c("countries","state","county")], is.na))) {
      loc.df <- dt[, .SD, .SDcols = c(unlist(covs.present[c("countries","state","county")]))]
      loc.df <- fixLoc(data.frame(loc.df), scrap = FALSE,
             loc.levels = c("country", "stateProvince", "municipality"))
      loc.df <- strLoc(loc.df)
      loc.df$loc.string  <- prepLoc(loc.df$loc.string)
      loc.df <- getLoc(loc.df)
      locais <- getAdmin(loc.df)
      locais$string <- paste(toupper(locais$NAME_0),", ",
                             locais$NAME_1, ": ",
                             locais$NAME_2, sep="")
      locais$string[locais$string == "NA, NA: NA"] <- "[Locality unknown]"
      locais$string <- gsub(", NA: NA$", "", locais$string, perl = TRUE)
      locais$string <- gsub(": NA$", "", locais$string, perl = TRUE)
    }

    if (!is.na(covs.present[["locality"]])) {
      df <- data.frame(dt1[, .SD, .SDcols = c(covs.present[["locality"]])])
      locais <- getAdmin(df)

      miss.ids <- is.na(locais$NAME_0) & !is.na(locais$loc.correct) & !locais$loc.correct %in% ""
      if (any(miss.ids)) locais[miss.ids,] <- getAdmin(locais[miss.ids,])

      locais$string <- paste(toupper(locais$NAME_0),", ",
                             locais$NAME_1, ": ",
                             locais$NAME_2, sep="")
      locais$string[locais$string == "NA, NA: NA"] <- "[Locality unknown]"
      locais$string <- gsub(", NA: NA$", "", locais$string, perl = TRUE)
      locais$string <- gsub(": NA$", "", locais$string, perl = TRUE)
    }

    if (is.null(locais)) {
      locais <- data.frame(string = rep("[Locality unknown]", dim(dt1)[1]))
    }

    # DATES
    if (all(c("month", "day") %in% names(dt1))) {
      dt1[, datas := do.call(paste, c(.SD, sep = "-")),
         .SDcols = c("day", "month", covs.present[["colYears"]])]
      dt1[, datas.tipo := "full",]
      `%like.ic%` <- function (x, pattern) {
        grepl(pattern, x, perl = TRUE, ignore.case = TRUE)
      }

      dt1[!datas %like.ic% "\\d", c("datas", "datas.tipo") := list("n.d.", "no_date"),]
      dt1[datas %like.ic% "^NA-NA-", datas.tipo := "year_only",]
      dt1[datas %like.ic% "^NA-[0-9]", datas.tipo := "month_year",]
      data.table::setDT(dt1)[!datas.tipo %in% c("no_date", "full"),
                            datas := stringr::str_replace(datas, "^NA-NA-", "01-01-")]
      data.table::setDT(dt1)[!datas.tipo %in% c("no_date", "full"),
                            datas := stringr::str_replace(datas, "^NA-", "01-")]
      dt1[!datas.tipo %in% c("no_date"), datas := format(as.Date(datas, "%d-%m-%Y"), date.format)]
      ### MAKE THE 4 AND 3 FROM THE SUBSTR ADAPTED TO DIFFERENT 'date.format'
      data.table::setDT(dt1)[datas.tipo %in% "year_only",
                            datas := substr(datas, nchar(datas) - 4, nchar(datas))]
      data.table::setDT(dt1)[datas.tipo %in% "month_year",
                            datas := substr(datas, 3, nchar(datas))]
      data.table::setDT(dt1)[!datas.tipo %in% c("no_date", "full"),
                            datas := stringr::str_trim(datas)]
    } else {
      dt1[, datas := .SD,
         .SDcols = c(covs.present[["colYears"]])]
    }

    #COMBINING LOCALITIES, DATES AND VOUCHERS
    dt1[ , locais := locais$string , ]
    dt1[ , lista.vouchs := do.call(paste, c(.SD, sep=", ")), ,
            .SDcols = c("locais", "datas", "vchrs")]

    #combining all voucher into a single string
    data.table::setorderv(dt1, c(covs.present[["species"]], "lista.vouchs"), c(1,1))
    dt2 <- dt1[ , do.call(paste, c(.SD, collapse= "; ",sep="")),
                by = c(covs.present[["species"]]),
                .SDcols = "lista.vouchs"]
    #Saving
    checklist$vouchers <-
      as.character(dt2$V1[match(checklist[, covs.present[["species"]]],
                                data.frame(dt2)[,1])])
  }

  # Organizing and ordering the output
  cols <- as.character(
            c(unlist(covs.present[names(covs.present) %in% c("families", "species")]),
              "scientific.name", "records", "tax.CL", "geo.CL","vouchers"))
  if (all(c(covs.present[["species"]], "scientific.name") %in% names(checklist)))
    cols <- cols[!cols %in% covs.present[["species"]]]

  cols <- cols[cols %in% names(checklist)]
  checklist <- checklist[, cols]

  if (fam.order) {
    checklist <- checklist[order(checklist[,1], checklist[,2], na.last = TRUE),]
    checklist <- checklist[!is.na(checklist[,2]),]
  } else {
    checklist <- checklist[order(checklist[,2], na.last = NA),]
  }

  if (any(apply(checklist, 2, function(x) all(is.na(x)))))
    checklist <- checklist[, !apply(checklist, 2, function(x) all(is.na(x)))]

  return(checklist)
}
