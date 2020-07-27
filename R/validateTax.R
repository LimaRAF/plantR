#' @title Validate Species Indentification
#'
#' @description This function ...
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
#'
#' @details The input data frame \code{x} must contain at least the columns
#' 'family.new' and 'identifiedBy.new'. Preferably, this data frame should also contain
#' the columns 'typeStatus' and 'recordedBy.new'.
#'
#' By default the function classifies as high confidence level all
#'   species identifications performed by the family specialist or any type
#'   specimens (isotype, paratypes, etc).
#'
#' (DESCRIBE ARGUMENT special.collector)
#'
#' (DESCRIBE ARGUMENT generalist and generalist.class)
#'
#'
#' If you miss the validation from one or more taxonomists, you can provide one
#'   or more taxonomists using argument `miss.taxonomist`. The format that should
#'   be provided is the name of her/his family of speciality followed by an
#'   undersoce and then his name on the TDWG format (e.g. "Bignoniaceae_Gentry,
#'   A.H.").
#'
#' A database of taxonomists different than the `plantR` default can be used.
#'  This database must be provided using the argument `taxonomist.list` and it must
#'  contain the columns 'family' and 'tdwg.name'. The first column is the family
#'  of speciality of the taxonomist and the second one is her/his name in the TDWG
#'  format. See `plantR` function tdwgName or tdwgNames on how to get names in
#'  the TDWG format from a list of people's name.
#'
#' @importFrom stringr str_trim
#'
#' @export validateTax
#'
validateTax = function(x,
                       special.collector = TRUE,
                       generalist = FALSE,
                       generalist.class = "medium",
                       miss.taxonomist = NULL,
                       taxonomist.list = "plantR")
{

  ### INCLUDE STEP TO VALIDATE OCCURRENCES THAT ARE NOT FROM CLASS 'PreservedSpecimen'

  #Checking the input
  if (!class(x) == "data.frame")
    stop("input object needs to be a data frame!")

  if (!("family.new" %in% names(x) &
        "identifiedBy.new" %in% names(x)))
    stop("input data frame needs to have at least the following columns: family.new and identifiedBy.new")

  ## THINK OF A SOLUTION HERE TO CHOOSE BETWEEN THE RAW OR THE EDIT COLUMNS...
  #putting the input data in the right order for validation
  # cls.nms <- c("family.new","identifiedBy.new","family","identifiedBy")
  # cls.nms <- cls.nms[cls.nms %in% names(x)]
  # x1 <- x[which(colnames(x) %in% cls.nms)]

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

    #### REMOVE THIS STEP FROM THE SYSDATA CODE !!!! ####
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
  combo.occs <- paste(x$family.new, x$identifiedBy.new, sep = "_")
  combo.occs <- stringr::str_trim(combo.occs)

  #Crossing the occurrence and reference family-specialist combinations
  x$tax.check <- combo.occs %in% combo

  #Validating all type specimens (isotype, paratypes, etc) but not the "not a type"
  if("typeStatus" %in% names(x))
    x$tax.check[!is.na(x$typeStatus) &
                  !grepl("not a type|notatype|probable type|tipo provavel|tipo provável",
                         x$typeStatus, ignore.case = TRUE)] <- TRUE

  #Specifying occurrences with unkown determiner name
  x$tax.check[x$tax.check == FALSE & x$identifiedBy.new %in% c(
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

    if("recordedBy.new" %in% names(x)) {

      combo2 <- paste(x$family.new, x$recordedBy.new, sep = "_")
      combo2 <- stringr::str_trim(combo2)
      #Crossing the occurrence and reference family-specialist combinations
      tax.check1 <- combo2 %in% combo
      x$tax.check[x$tax.check %in% c("unknown") &
                    tax.check1 %in% TRUE] <- TRUE

    } else {
        warning("Argument 'special.collector' set to TRUE but the column 'recordedBy.new' is missing from the input data")
    }
  }

  if(generalist) {

    #Crossing the occurrences with the names of the generalists
    tax.check2 <- x$identifiedBy.new %in% generalists$tdwg.name
    x$tax.check[x$tax.check %in% c("FALSE") &
                  tax.check2 %in% TRUE] <- generalist.class
  }

  x$tax.check[x$tax.check %in% "FALSE"] <- "low"
  x$tax.check[x$tax.check %in% "TRUE"] <- "high"

    non.tax.det <- sort(table(x$identifiedBy.new[x$tax.check %in% "low"]))
    cat("People with many of determinations but not in the taxonomist list: \n",
        paste(tail(paste(names(non.tax.det), non.tax.det, sep = ": "),10)), sep="\n")

  return(x)
}
