#' @title Standardize Botanical Family
#'
#' @description The functions use the ...
#'
#' @param x a data frame containing at least the columns `family`, `genus` and
#'   `speciesName`.
#'
#' @return the data frame \code{x} with an additional column called 'family.new'.
#'
#' @details First search of names is based on the list of families and accepted
#'   synonyms for vascular plants obtained from the
#'   \href{http://www.mobot.org/MOBOT/research/APweb/}{APG website}, which
#'   includes families cited in the APG IV (2016) and in the PPG I (2016). If
#'   the family name is not found in the APG list, a search is carried in the
#'   Brazilian Flora 2020 (BF-2020), using package `flora`. If the family name
#'   is still not found in BF-2020, then a final try is performed in The Plant
#'   List (TPL), using package `Taxonstand`. Family names retrieved from BF-2020
#'   and TPL are finally converted to the names accepted by the APG IV or PPG I.
#'
#' In case there is a conflict in the original family name and the name found
#' based on the genus name, the original name is replaced by the name from the
#' APG IV or PPG I with a warning.
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
#' ferns. Journal of Systematics and Evolution. 54 (6): 563–603.
#'
#' @import data.table
#' @importFrom flora get.taxa
#' @importFrom Taxonstand TPL
#'
#' @export formatFamily
#'
#' @examples
#'
#' occs <- data.frame(family = c("Ulmaceae", "Cannabaceae", "Salicaceae",
#' "Flacourtiaceae", "Vivianiaceae"), genus = c("Trema", "Trema", "Casearia",
#' "Casearia", "Casearia"), speciesName = c("Trema micrantha", "Trema
#' micrantha", "Casearia sylvestris","Casearia sylvestris","Casearia
#' sylvestris"), stringsAsFactor = FALSE)
#' formatFamily(occs)
#'
formatFamily <- function(x) {

  #Avoiding warnings in package check when using data.table
  "family" <- "flora.br" <- "genus" <- NULL
  "name.correct" <- "name.correct.y" <- NULL
  "scientificName" <- "string.plantr" <- NULL

  # Getting the dictionaries
  families.apg <- families_synonyms

  # Getting the list of families and their respective genera
  dt <- data.table::data.table(x)
  data.table::setkeyv(dt, c("family"))
  families.data <- dt[, list(genus = unique(genus)), by = "family"]
  families.data <- merge(families.data,
                          families.apg[, c("name", "name.correct")],
                         by.x = "family", by.y = "name", all.x = TRUE)

  # Getting missing family names from Brazilian Flora 2020
  data.table::setkeyv(dt, c("genus"))
  families.data[, flora.br := flora::get.taxa(genus, suggestion.distance = 0.95, drop <-NULL)$family,
                by = "genus"]
  families.data$flora.br[5] <- NA
  families.data[(is.na(family) | is.na(name.correct) & !is.na(flora.br)),
                name.correct :=  flora.br, ]

  # Any conflicts between original names and the ones retrieved in BF-2020?
  print.problems <- unique(families.data[!is.na(flora.br) & name.correct != flora.br, , ])

  if (dim(print.problems)[1] > 0) {
    warning("The following family names are problematic and were automatically replaced:",
            call. = FALSE)
    warning(paste0(print.problems$genus,": ", print.problems$family, " -> ", print.problems$flora,"\n"),
            call. = FALSE)
  }
  families.data[name.correct != flora.br, name.correct := flora.br, ]

  if (families.data[, any(is.na(name.correct))]) {
    miss.families <- families.data[is.na(name.correct), family, F]
    genus.data <- dt[family %in% miss.families,
                     list(genus = unique(genus), species = unique(scientificName)),
                     by = "family"]
    families.data[family %in% miss.families,
                     name.correct := Taxonstand::TPL(genus.data$species, corr = FALSE, drop.lower.level = TRUE)$Family, ]
    #Trying to solve remaining problems internally
    #families.data[family %in% miss.families, lapply(.SD, na.omit), by = list(genus)]
  }

  # Double checking if all names are in the APG dictionaire
  families.data <- merge(families.data,
                         families.apg[, c("name", "name.correct")],
                         by.x = "name.correct", by.y = "name", all.x = TRUE)

  # If nothing was found, keep the original family
  families.data[is.na(name.correct.y), name.correct.y := name.correct ]
  families.data[is.na(name.correct.y), name.correct.y := family ]

  # Merging the results by family X genus with the occurrence data
  dt[, string.plantr := paste(family, genus, sep="_"), ]
  families.data[, string.plantr := paste(family, genus, sep="_"), ]
  dt <- merge(dt,
               families.data[,c("string.plantr","name.correct.y")],
               by = "string.plantr", all.x = TRUE)
  dt[, string.plantr := NULL, ]
  data.table::setnames(dt, dim(dt)[2], 'family.new')

  return(data.frame(dt))
}
