#' @title Validate Species Indentification
#'
#' @description This function assigns different categories of confidence level
#' (i.e. high, medium, low or unknown) to the identification of species records.
#'
#' @param x a data frame with the occurrence data.
#' @param special.collector Logical. Specimens collected by the family
#'   specialist but with empty determiner field, should be classified as high
#'   confidence level? Default to TRUE.
#' @param generalist Logical. Should family generalists be considered for
#'   taxonomic validation? Default to FALSE.
#' @param generalist.class Character. Confidence level to be assigned to family
#'   generalists. Default to "medium".
#' @param miss.taxonomist Vector. Any missing combination of family x taxonomist
#'   that should be added to the validation?
#' @param taxonomist.list a data.frame containing the list of taxonomist names. The
#'   default is "plantR", the internal `plantR` global database of plant
#'   taxonomists (see Details).
#' @param top.det numerical. How many of the top missing identifiers should be printed?
#'   Default to 10.
#'
#' @details
#'
#' The input data frame \code{x} must contain at least the columns 'family.new'
#' and 'identifiedBy.new'. Preferably, this data frame should also contain the
#' columns 'typeStatus' and 'recordedBy.new'.
#'
#' By default, the function classifies as high confidence level all species
#' identifications performed by the family specialist or any type specimens
#' (isotype, paratypes, etc).
#'
#' Some specimens are collected by a specialist of the family, but the
#' identifier information is missing. By default, we assume the same confidence
#' level for these specimens as that assigned for specimens where the identifier
#' is the family specialist. But users can choose otherwise by setting the
#' argument `special.collector` to FALSE.
#'
#' The arguments `generalist` and `generalist.class` define if taxonomists that
#' provide identifications for many different families outside his specialty,
#' often referred to as a generalist, should be considered in the validation and
#' under which confidence level. There are some names of generalists in the
#' __plantR__ default taxonomist database; however, this list of generalist
#' names is currently biased towards South America, particularly Brazil.
#'
#' If you miss the validation from one or more taxonomists, you can provide one
#' or more taxonomists using the argument `miss.taxonomist`. The format that should
#' be provided is the name of her/his family of specialty followed by an
#' underscore and then his name on the TDWG format (e.g. "Bignoniaceae_Gentry,
#' A.H.").
#'
#' A database of taxonomists different than the `plantR` default can be used.
#' This database must be provided using the argument `taxonomist.list` and it
#' must contain the columns 'family' and 'tdwg.name'. The first column is the
#' family of specialty of the taxonomist and the second one is her/his name in
#' the TDWG format. See `plantR` function tdwgName or tdwgNames on how to get
#' names in the TDWG format from a list of people's names.
#'
#' @importFrom stringr str_trim
#'
#' @export validateTax
#'
validateTax <- function(x,
                       special.collector = TRUE,
                       generalist = FALSE,
                       generalist.class = "medium",
                       miss.taxonomist = NULL,
                       taxonomist.list = "plantR",
                       top.det = 10)
{

  ### INCLUDE STEP TO VALIDATE OCCURRENCES THAT ARE NOT FROM CLASS 'PreservedSpecimen'

  #Checking the input
  if (!class(x) == "data.frame")
    stop("input object needs to be a data frame!")

  #Select which co-variables will be used in the summary (priority to the edited columns)
  covs <- list(collectors = c("recordedBy.new", "recordedBy"),
               families = c("family.new", "family"),
               identifiers = c("identifiedBy.new", "identifiedBy"))

  #Get only the columns of interest
  covs.present <- lapply(covs, function(z) head(z[which(z %in% names(x))], 1))
  covs.present[sapply(covs.present, identical, character(0))] <- NA

  if(is.na(covs.present[["families"]]) |
     is.na(covs.present[["identifiers"]]))
    stop("input data frame needs at least the following columns: family and identifiedBy")

  #Getting the dictionaries
  families.apg <- familiesSynonyms
  if (all(taxonomist.list %in% c("plantR", "plantr"))) {

    autores <- taxonomists
    autores <- merge(autores,
                     families.apg[, c("name", "name.correct")],
                     by.x = "family", by.y = "name", all.x = TRUE)
    autores <- autores[order(autores$order),]

  } else {

    if(!class(taxonomist.list) == "data.frame")
      stop("The list of taxonomists must be provided as a data frame")
    if(!all(c("family", "tdwg.name") %in% names(taxonomist.list)))
      stop("The list of taxonomists must contain at least two columns: 'family' and 'tdwg.name'")
    autores <- taxonomist.list
    autores <- merge(autores,
                     families.apg[, c("name", "name.correct")],
                     by.x = "family", by.y = "name", all.x = TRUE)
  }

  if(!generalist) {

    autores <- autores[!grepl('Generalist', autores$family),]

  } else {

    generalists <- autores[grepl('Generalist', autores$family),]
    autores <- autores[!grepl('Generalist', autores$family),]

  }

  #Getting the unique family-specialist combinations from the reference list
  combo <- unique(paste(autores$family, autores$tdwg.name, sep = "_"))
  if (!is.null(miss.taxonomist))
    combo <- c(combo, miss.taxonomist)

  if (all(taxonomist.list %in% c("plantR", "plantr"))) {
    tmp <- unique(paste(autores$name.correct[!is.na(autores$name.correct)],
                   autores$tdwg.name[!is.na(autores$name.correct)],
                   sep = "_"))
    tmp <- tmp [!tmp %in% combo]
    combo <- c(combo, tmp)
  }
  combo <- stringr::str_trim(combo)

  #Getting the unique family-specialist combinations for each occurrence
  #dt <- data.table::data.table(x) # not using data.table for now
  combo.occs <- paste(x[,covs.present[["families"]]],
                      x[,covs.present[["identifiers"]]], sep = "_")
  combo.occs <- stringr::str_trim(combo.occs)

  #Crossing the occurrence and reference family-specialist combinations
  x$tax.check <- combo.occs %in% combo

  #Validating all type specimens (isotype, paratypes, etc) but not the "not a type"
  if("typeStatus" %in% names(x))
    x$tax.check[!is.na(x$typeStatus) &
                  !grepl("not a type|notatype|probable type|tipo provavel|tipo provável",
                         x$typeStatus, ignore.case = TRUE)] <- TRUE

  #Specifying occurrences with unkown determiner name
  x$tax.check[x$tax.check == FALSE & x[,covs.present[["identifiers"]]] %in% c(
                  "Semdeterminador",
                  "SemDeterminador",
                  "Anonymus",
                  "Anonymous",
                  "Anonimo",
                  "Incognito",
                  "Unknown",
                  "s.d.",
                  "s.n."
                )] <- "unknown"

  #Validating all specimens collected by the family specialist but with the determiner field empty
  if(special.collector) {

    if(!is.na(covs.present[["collectors"]])) {

      combo2 <- paste(x[,covs.present[["families"]]],
                      x[,covs.present[["collectors"]]], sep = "_")
      combo2 <- stringr::str_trim(combo2)
      #Crossing the occurrence and reference family-specialist combinations
      tax.check1 <- combo2 %in% combo
      x$tax.check[x$tax.check %in% c("unknown") &
                    tax.check1 %in% TRUE] <- TRUE

    } else {
        warning("Argument 'special.collector' set to TRUE but the column 'recordedBy' is missing from the input data")
    }
  }

  if(generalist) {

    #Crossing the occurrences with the names of the generalists
    tax.check2 <- x[,covs.present[["identifiers"]]] %in% generalists$tdwg.name
    x$tax.check[x$tax.check %in% c("FALSE") &
                  tax.check2 %in% TRUE] <- generalist.class
  }

  x$tax.check[x$tax.check %in% "FALSE"] <- "low"
  x$tax.check[x$tax.check %in% "TRUE"] <- "high"

  if ("basisOfRecord" %in% names(x))
    x$tax.check[is.na(x$basisOfRecord) | !x$basisOfRecord %in%
                  c("PreservedSpecimen","preservedspecimen","PRESERVEDSPECIMEN", "S",
                    "Exsicata de planta","Esicata de planta","Exsicata")] <- "low"


  #Any potential specialists missing form the taxonomist list?
  non.tax.det <- sort(table(x[,covs.present[["identifiers"]]][x$tax.check %in% "low"]))
  non.tax.det.df <- data.frame(names(non.tax.det), as.double(non.tax.det))
  row.names(non.tax.det.df) <- NULL
  non.tax.det.df <- non.tax.det.df[order(non.tax.det.df[,2], decreasing = TRUE),]
    cat("Top people with many determinations but not in the taxonomist list: \n",
        knitr::kable(head(non.tax.det.df, top.det), row.names = FALSE, col.names = c("Identifier", "Records")), sep="\n")

  return(x)
}
