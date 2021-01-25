#' @title Flag Occurrences From Cultivated Individuals
#'
#' @description This function ...
#'
#' @param x a data frame with the occurrence data.
#' @param cc_inst logical. Should the identification of occurrences in the
#'   vicinity of biodiversity institutions (package `coordinateCleaner`) be performed?
#'   Default to FALSE.
#'
#' @details The input data frame \code{x} must contain at least the columns
#'   containing the description of the occurrence (i.e. 'occurrenceRemarks') or
#'   the locality description (e.g. 'locality').
#'
#' @importFrom CoordinateCleaner cc_inst
#'
#' @export getCult
#'
getCult <- function(x, cc_inst = FALSE) {

  #Avoiding warnings in package check when using data.table
  "cult.check" <- "locality" <- "occurrenceRemarks" <- NULL
  "decimalLatitude" <- "decimalLongitude" <- NULL

  #Checking the input
  if (!class(x) == "data.frame")
    stop("input object needs to be a data frame!")

  if (!("occurrenceRemarks" %in% names(x) |
        "locality" %in% names(x)))
    stop("input data frame needs to have at least on of the following columns: 'occurrenceRemarks' and 'locality'")

  #Objects and function needed for the search for cultivated specimens
  cult <- cultivated
  pat.cult <- paste(cult, collapse = "|")
  not.cult <- notCultivated
  pat.not.cult <- paste(not.cult, collapse = "|")
  `%like.ic%` <- function (x, pattern) {
    grepl(pattern, x, perl = TRUE, ignore.case = TRUE)
  }

  ##Flaging records of true and probable cultivated specimens (based on the locality descriptions)
  x$cult.check <- NA_character_
  dt <- data.table::data.table(x)

  if("occurrenceRemarks" %in% names(x)) {
    dt[occurrenceRemarks %in% cult, cult.check := "cultivated", ]
    dt[is.na(cult.check) &
         occurrenceRemarks %like.ic% pat.cult &
         !occurrenceRemarks %like.ic% pat.not.cult, cult.check := "prob_cultivated",]
  }

  if("locality" %in% names(x)) {
    dt[locality %in% cult, cult.check := "cultivated", ]
    dt[is.na(cult.check) &
         locality %like.ic% pat.cult &
         !locality %like.ic% pat.not.cult, cult.check := "prob_cultivated",]
  }

  #Flaging probable records of cultivated specimens (based on the coordinates, package coordinateCleaner)
  if(cc_inst) {
    x1 <- data.frame(dt[is.na(cult.check) & !is.na(decimalLatitude) & !is.na(decimalLongitude),])
    x1$decimalLongitude = as.double(x1$decimalLongitude)
    x1$decimalLatitude = as.double(x1$decimalLatitude)
    x1$cc_inst <- CoordinateCleaner::cc_inst(x1,
                                             lon = "decimalLongitude",
                                             lat = "decimalLatitude",
                                             species = "scientificName",
                                             value = "flagged",
                                             geod = TRUE,
                                             buffer = 100)
    x1$cc_inst[!x1$cc_inst] <- "prob_cultivated_inst"
    dt[is.na(cult.check) & !is.na(decimalLatitude) & !is.na(decimalLongitude),
       cult.check := x1$cc_inst]
    rm(x1)
  }

  #Some botanical gardens with many of cultivated specimens from neotropical tree species
  # "Atherton" or  "Atherton And Tyldesley Botanical Gardens" or "SFR 191, Barron"
  # "TBG": Toronto BG?
  # "Fairchild Tropical Garden"
  # "Royal Botanic Gardens" or "Royal Botanic Garden"
  # "Pazo de Castrelos" or "Parque de Castrelos" or "Castrelos"
  # "Boettcher Memorial Tropical Conservatory"
  # "Royal Botanic Gardens Melbourne" or "Royal Botanic Gardens, Melbourne" or "Melbourne Botanic Gardens"
  # "Foster Gardens" or "Foster Botanic Garden"
  # "Coconut Grove Introduction Garden"
  # "Botanic Garden Production Facility"
  # "US Botanic Garden"
  # "Singapore Botanic Garden"
  # "SFR 191, Barron" (Australia)

  return(data.frame(dt))
}

