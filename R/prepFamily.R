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
#'   belong to names. Defaults to "Plantae"
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
#' provided (default column name: 'genus'). If it is not provided,
#' this columns is generated internally based on the species names
#' provided.
#'
#' The first search of names is based on the list of families and
#' accepted synonyms for vascular plants provided by __plantR__, which
#' was mainly compiled from the
#' \href{http://www.mobot.org/MOBOT/research/APweb/}{APG website},
#' which includes families cited in the APG IV (2016) and in the PPG I
#' (2016).
#'
#' If the family name is not found in this list, a second search is
#' carried in the Brazilian Flora 2020 (BF-2020), using package
#' `flora` (Carvalho 2019). Family names retrieved from BF-2020 are
#' then converted to the names accepted by the APG IV or PPG I.
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
#' Gustavo Carvalho (2019). flora: Tools for Interacting with the Brazilian
#' Flora 2020. R package version 0.3.1. http://www.github.com/gustavobio/flora
#'
#' PPG I (2016). A community-derived classification for extant lycophytes and
#' ferns. Journal of Systematics and Evolution. 54 (6): 563-603.
#'
#' @import data.table
#' @importFrom flora get.taxa
#' @importFrom knitr kable
#'
#' @export prepFamily
#'
#' @examples
#'
#' df <- data.frame(
#' family = c("Ulmaceae", "Cannabaceae", "Salicaceae", "Flacourtiaceae", "Vivianiaceae", ""),
#' genus = c("Trema", "Trema", "Casearia", "Casearia", "Casearia", ""),
#' speciesName = c("Trema micrantha", "Trema micrantha", "Casearia sylvestris",
#' "Casearia sylvestris","Casearia sylvestris","Casearia sylvestris"),
#' stringsAsFactors = FALSE)
#' prepFamily(df, spp.name = "speciesName")
#'
prepFamily <- function(x,
                       fam.name = "family",
                       gen.name = "genus",
                       spp.name = "scientificName",
                       kingdom = "Plantae",
                       print = TRUE) {

  #Avoiding warnings in package check when using data.table
  flora.br <- name.correct <- name.correct.y <- string.plantr <- NULL
  tmp.fam <- tmp.gen <- tmp.spp <- tmp.ordem <- family.new <- NULL

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
    data.table::setnames(dt, "tmp.gen", gen.name)
  }

  data.table::setnames(dt,
                       c(fam.name, gen.name, spp.name),
                       c("tmp.fam", "tmp.gen", "tmp.spp"))

  if (any(dt[ , is.na(tmp.gen)])) {
    dt[is.na(tmp.gen), tmp.gen := lapply(tmp.spp, getGenus),
       by = tmp.spp]
  }

  if (any(dt[,grepl("cf\\.$|aff\\.$", tmp.gen, perl = TRUE,
                ignore.case = TRUE),])) {
    dt[grepl("cf\\.$|aff\\.$", tmp.gen, perl = TRUE, ignore.case = TRUE),
       tmp.gen := gsub("cf\\.$|aff\\.$", "", tmp.gen, ignore.case = TRUE,
                       perl = TRUE),
       by = tmp.spp]
  }

  # Codes for plants
  if (tolower(kingdom) == "plantae") {
    # Getting the dictionaries
    families.apg <- familiesSynonyms

    # Getting the list of families and their respective genera
    data.table::setkeyv(dt, "tmp.fam")
    families.data <- dt[, unique(.SD), by = tmp.fam, .SDcol = "tmp.gen"]
    families.data <- data.table::merge.data.table(families.data,
                                                  families.apg[, c("name", "name.correct")],
                                                  by.x = "tmp.fam", by.y = "name", all.x = TRUE)

    # Getting missing family names from Brazilian Flora 2020
    families.data[, flora.br := as.character(flora::get.taxa(tmp.gen,
                                                             suggestion.distance = 0.9,
                                                             drop = NULL)$family),
                  by = "tmp.gen"]
    families.data[(is.na(tmp.fam) | is.na(name.correct) & !is.na(flora.br)),
                  name.correct :=  flora.br, ]

    # Any conflicts between original names and the ones retrieved in BF-2020?
    print.problems <- unique(families.data[!is.na(flora.br) & name.correct != flora.br, , ])
    print.problems <- print.problems[order(print.problems[, 1]), ]
    print.problems <- print.problems[order(print.problems[, 2]), ]

    if (print) {
      if (dim(print.problems)[1] > 0) {
        cat("The following family names were automatically replaced:\n",
            knitr::kable(print.problems[,c(2,1,4)],
                         col.names = c("Genus", "Old fam.", "New fam.")),"",
            sep="\n")
      }
    }
    families.data[name.correct != flora.br, name.correct := flora.br, ]

    # Any missing family names?
    if (families.data[, any(is.na(name.correct))]) {
      miss.families <- families.data[is.na(name.correct),]
      fbo.families <- flora::get.taxa(miss.families$tmp.fam,
                                      replace.synonyms = FALSE)
      check_these <- is.na(fbo.families$family) &
        !miss.families$tmp.gen %in% "Indet."
      if (any(check_these)) {
        fbo.families1 <- flora::get.taxa(miss.families$tmp.gen[check_these],
                                         replace.synonyms = FALSE)
        replace_these <- !is.na(fbo.families1$family)
        if (any(replace_these))
          fbo.families$family[check_these] <- fbo.families1$family
      }
      families.data[is.na(name.correct),
                    name.correct := fbo.families$family, ]
    }

    # Double checking if all names are in the APG dictionaire
    families.data <- merge(families.data,
                           families.apg[, c("name", "name.correct")],
                           by.x = "name.correct", by.y = "name", all.x = TRUE)

    # If nothing was found, keep the original family
    families.data[is.na(name.correct.y), name.correct.y := name.correct ]
    families.data[is.na(name.correct.y), name.correct.y := tmp.fam ]

    # Merging the results by family X genus with the occurrence data
    dt[, string.plantr := paste(tmp.fam, tmp.gen, sep="_"), ]
    families.data[, string.plantr := paste(tmp.fam, tmp.gen, sep="_"), ]
    families.data[string.plantr == "_NA", string.plantr := NA_character_, ]
    dt <- data.table::merge.data.table(dt,
                                       families.data[,c("string.plantr","name.correct.y")],
                                       by = "string.plantr", all.x = TRUE)
    # Preparing to return
    dt[, string.plantr := NULL, ]
    data.table::setnames(dt,
                         c("tmp.fam", "tmp.gen", "tmp.spp"),
                         c(fam.name, gen.name, spp.name))
    data.table::setnames(dt, dim(dt)[2], 'family.new')

    data.table::setkeyv(dt, c("tmp.ordem")) #re-ordering the data.table
  }

  # Codes for animals, fungi, algae and bryophytes
  if (!tolower(kingdom) == "plantae") {
    dt[, family.new := tmp.fam]
    data.table::setkeyv(dt, c("tmp.ordem")) #re-ordering the data.table
    warning("Standardization of family names is currently only available for vascular plants")
  }

  dt[, tmp.ordem := NULL, ]

  return(data.frame(dt))
}
