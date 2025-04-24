#' @title Standardize Botanical Family
#'
#' @description Search for valid family names of vascular plants
#'   following the APG IV (2016) and in the PPG I (2016).
#'
#' @param x a data frame containing the names of the vascular plant
#'   families and the corresponding species.
#' @param fam.name character. The name of the column containing the
#'   family names. Defaults to "family".
#' @param gen.name character. The name of the column containing the
#'   genus names. Defaults to "genus"
#' @param spp.name character. The name of the column containing the
#'   species names. Defaults to "scientificName"
#' @param kingdom character. The name of the kingdom that the taxa
#'   belong to names. Defaults to "plantae"
#' @param print logical. Should the automatically replaced family
#'   names be printed? Default to TRUE.
#'
#' @return the data frame \code{x} with an additional column called
#'   'family.new'.
#'
#' @details
#'
#' The data frame \code{x} must contain at least two columns which
#' stores the family and species names. By default, the names of these
#' columns should be `family` and `speciesName`, following Darwin Core
#' field names. Optionally, a column with the genus name can be
#' provided (default column name: `genus`). If it is not provided,
#' this columns is generated internally based on the species names
#' provided.
#'
#' The first search of names is based on the list of accepted family
#' names of vascular plants provided by __plantR__, which was mainly
#' compiled from the \href{http://www.mobot.org/MOBOT/research/APweb/}{APG website},
#' which includes families cited in the APG IV (2016) and in the PPG I
#' (2016).
#'
#' If the family name is not found in this list, a second search is
#' carried based on the genus name in the Brazilian Flora 2020
#' (BF-2020) taxonomic backbone. Family names retrieved from BF-2020
#' are then converted to the names accepted by the APG IV or PPG I.
#'
#' In case there is a conflict in the original family name and the
#' name found based on the genus name, the original name is replaced
#' by the name from the APG IV or PPG I with a warning.
#'
#' @author Renato A. F. de Lima
#'
#' @references
#'
#' Angiosperm Phylogeny Group (2016). An update of the Angiosperm
#' Phylogeny Group classification for the orders and families of flowering
#' plants: APG IV. Bot. J. Linnean Soc. 181: 1-20.
#'
#' PPG I (2016). A community-derived classification for extant lycophytes and
#' ferns. Journal of Systematics and Evolution. 54 (6): 563-603.
#'
#' @import data.table
#' @importFrom knitr kable
#'
#' @export prepFamily
#'
#' @examples
#'
#' df <- data.frame(
#' family = c("Ulmaceae", "Cannabaceae", "Salicaceae", "Flacourtiaceae", "Vivianiaceae", ""),
#' genus = c("Trema", "Trema", "Casearia", "Casearia", "Casearia", ""),
#' scientificName = c("Trema micrantha", "Trema micrantha", "Casearia sylvestris",
#' "Casearia sylvestris","Casearia sylvestris","Casearia sylvestris"))
#' prepFamily(df)
#'
prepFamily <- function(x,
                       fam.name = "family",
                       gen.name = "genus",
                       spp.name = "scientificName",
                       kingdom = "plantae",
                       print = TRUE) {

  #Avoiding warnings in package check when using data.table
  flora.bb <- name.correct <- name.correct.x <- string.plantr <- NULL
  tmp.fam <- tmp.gen <- tmp.spp <- tmp.ordem <- family.new <- NULL
  name.correct.y <- NULL

  ## check input
  if (!inherits(x, "data.frame"))
    stop("Input object needs to be a data frame!")

  if (dim(x)[1] == 0)
    stop("Input data frame is empty!")

  if (!fam.name %in% names(x))
    stop("Input data frame must have a column with family names")

  na.strings <- c("", " ", "NA", NA)
  x[[fam.name]][x[[fam.name]] %in% na.strings] <- NA_character_

  if (!spp.name %in% names(x))
    stop("Input data frame must have a column with species names")

  x[[spp.name]][x[[spp.name]] %in% na.strings] <- NA_character_

  if (gen.name %in% names(x))
    x[[gen.name]][x[[gen.name]] %in% na.strings] <- NA_character_

  dt <- data.table::data.table(x)
  dt[, tmp.ordem := .I,]

  getGenus <- function(x) {
    as.character(strsplit(x, " ", fixed = TRUE)[[1]][1]) }

  if (!gen.name %in% names(dt)) {
    dt[ , tmp.gen := lapply(.SD, getGenus),
        by = c(spp.name),
        .SDcols = c(spp.name)]
  } else {
    dt[ , tmp.gen := .SD, .SDcols = c(gen.name)]
    dt[is.na(tmp.gen) , tmp.gen := lapply(.SD, getGenus),
       by = c(spp.name), .SDcols = c(spp.name)]
  }
  data.table::setnames(dt,
                       c(fam.name, spp.name),
                       c("tmp.fam", "tmp.spp"))

  indets <- c("indet", "indeterminada", "unclassified", "undetermined")
  dt[tolower(tmp.gen) %in% indets, tmp.gen := "Indet.",
     by = "tmp.gen"]
  dt[grepl("indet|unclass|undet", tolower(tmp.gen), perl = TRUE),
     tmp.gen := "Indet.", by = "tmp.gen"]
  dt[grepl("sp\\.|spp\\.", tolower(tmp.gen), perl = TRUE),
     tmp.gen := "Indet.", by = "tmp.gen"]

  if (any(dt[,grepl("cf\\.$|aff\\.$", tmp.gen, perl = TRUE,
                ignore.case = TRUE),])) {
    dt[grepl("cf\\.$|aff\\.$", tmp.gen, perl = TRUE, ignore.case = TRUE),
       tmp.gen := gsub("cf\\.$|aff\\.$", "", tmp.gen, ignore.case = TRUE,
                       perl = TRUE),
       by = tmp.spp]
  }


  kingdons <- unique(familiesSynonyms$kingdom)

  if (tolower(kingdom) %in% kingdons) {

    all.families <- familiesSynonyms[familiesSynonyms$kingdom %in%
                                       tolower(kingdom), ]
  } else {
    all.families <- familiesSynonyms
  }

  # Getting the list of families and their respective genera
  data.table::setkeyv(dt, "tmp.fam")
  families.data <- dt[, unique(.SD), by = tmp.fam, .SDcol = "tmp.gen"]
  families.data <-
    data.table::merge.data.table(families.data,
                                 all.families[, c("name", "name.correct")],
                                 by.x = "tmp.fam",
                                 by.y = "name", all.x = TRUE)

  # Getting missing family names from Brazilian Flora 2020
  if (any(is.na(families.data$name.correct))) {
    families.data[, flora.bb := getFamily(.SD), .SDcol = "tmp.gen"]
    families.data[, flora.bb := lapply(.SD, getFamily),
                  .SDcols = c("tmp.gen")]
    families.data <-
      data.table::merge.data.table(families.data,
                                   all.families[, c("name", "name.correct")],
                                   by.x = "flora.bb",
                                   by.y = "name", all.x = TRUE,
                                   suffixes = c(".x", "")
      )
    families.data[(is.na(tmp.fam) | is.na(name.correct) & !is.na(flora.bb)),
                  name.correct :=  flora.bb, ]
  } else {
    families.data[, flora.bb := name.correct]
  }

  # Any conflicts between original names and the ones retrieved?
  print.problems <- unique(families.data[!is.na(flora.bb) &
                                           tmp.fam != flora.bb, , ])
  print.problems <- print.problems[order(tmp.fam), ]
  print.problems <- print.problems[order(tmp.gen), ]
  print.problems <- print.problems[, .SD,
                                   .SDcols = c("tmp.fam", "tmp.gen",
                                               "name.correct")]
  if (print) {
    if (dim(print.problems)[1] > 0) {
      cat("The following family names were automatically replaced:\n",
          knitr::kable(print.problems[,c(2,1,3)],
                       col.names = c("Genus", "Old fam.", "New fam.")),"",
          sep="\n")
    }
  }
  families.data[name.correct.x != flora.bb, name.correct.x := flora.bb, ]

  # Any missing family names?
  if (families.data[, any(is.na(name.correct))]) {
    miss.families <- families.data[is.na(name.correct),]
    fbo.families <- getFamily(miss.families$tmp.gen,
                              fuzzy.match = TRUE)
    families.data[is.na(name.correct),
                  name.correct := fbo.families, ]
  }

  # Double checking if all names are in the APG dictionaire
  families.data <- merge(families.data,
                         all.families[, c("name", "name.correct")],
                         by.x = "name.correct", by.y = "name", all.x = TRUE)

  # If nothing was found, keep the original family
  families.data[is.na(name.correct), name.correct := name.correct.y ]
  families.data[is.na(name.correct), name.correct := tmp.fam ]

  # Merging the results by family X genus with the occurrence data
  dt[, string.plantr := paste(tmp.fam, tmp.gen, sep="_"), ]
  families.data[, string.plantr := paste(tmp.fam, tmp.gen, sep="_"), ]
  families.data[string.plantr == "_NA", string.plantr := NA_character_, ]
  dt <- data.table::merge.data.table(dt,
                                     families.data[,c("string.plantr","name.correct")],
                                     by = "string.plantr", all.x = TRUE)
  # Preparing to return
  dt[, string.plantr := NULL, ]
  data.table::setkeyv(dt, c("tmp.ordem"))
  dt[, tmp.ordem := NULL, ]

  data.table::setnames(dt,
                       c("tmp.fam", "tmp.spp"),
                       c(fam.name, spp.name))
  data.table::setnames(dt, dim(dt)[2], 'family.new')
  data.table::setnames(dt, 'tmp.gen', 'genus.new')

  return(data.frame(dt))
}
