#' @title Confidence on Species Identification
#'
#' @description This function assigns different categories of confidence level
#'   (i.e. high, medium, low or unknown) to the identification of species
#'   records, based on the name of the person who provided the species
#'   identification and on type specimens.
#'
#' @param x a data frame with the species records.
#' @param col.names vector. A named vector containing the names of columns in
#'   the input data frame for each of the information needed to assign
#'   confidence levels to species identifications. Default to the __plantR__
#'   output column names.
#' @param special.collector Logical. Specimens collected by the family
#'   specialist but with empty determiner field, should be classified as high
#'   confidence level? Default to TRUE.
#' @param generalist Logical. Should family generalists be considered for
#'   taxonomic validation? Default to FALSE.
#' @param generalist.class Character. Confidence level to be assigned to family
#'   generalists. Default to "medium".
#' @param other.records Character or Integer. The Confidence level (if
#'   character) or the number of downgrading steps to be assigned to records
#'   which are not preserved specimens. Default to NULL (all record types are
#'   treated the same).
#' @param miss.taxonomist Vector. Any missing combination of family x taxonomist
#'   that should be added to the validation?
#' @param taxonomist.list a data.frame containing the list of taxonomist names. The
#'   default is "plantR", the internal `plantR` global database of plant
#'   taxonomists (see Details).
#' @param voucher.list Vector. One or more unique record identifiers (i.e.
#'   combination of collection code and number) that should be flagged with a
#'   high confidence level? Default to NULL.
#' @param rec.ID Character. The name of the columns containing the unique record
#' identifier (see function `getTombo()`). Default to 'numTombo'.
#' @param noName Vector. One or more characters (in lower cases) with the
#'   standard notation for missing data in the field 'det.name'. Default to some
#'   typical notation found in herbarium data.
#' @param top.det Numerical. How many of the top missing identifiers should be printed?
#'   Default to 10.
#'
#' @details
#' The input data frame \code{x} must contain at least the columns with the
#' information on the record family and the name of the person that provided the
#' species identification. Preferably, this data frame should also contain
#' information on type specimens and collectors names. If the user provide a
#' list of records to be flagged as having a high confidence level in the
#' identification, the user must also provide the column where the record unique
#' identifiers are stored. The names of these columns should be provided as a
#' named vector to the argument `col.names`, as follows:
#'   - 'family': the botanical family (default: 'family.new')
#'   - 'det.name': the identifier name (default: 'identifiedBy.new')
#'   - 'col.name': the collector name (default: 'recordedBy.new')
#'   - 'types': type specimens (default: 'typeStatus')
#'   - 'rec.ID': the collector serial number (default: 'numTombo')
#'   - 'rec.type': the type of record (default: 'basisOfRecord')
#'
#' As for other functions in __plantR__, using a data frame \code{x} that has
#' already passed by the editing steps of the __plantR__ workflow should result
#' in more accurate outputs.
#'
#' The function classifies as high confidence level all records whose species
#' identifications were performed by a family specialist or any type specimens
#' (isotype, paratypes, etc). By default, the names of family specialists are
#' obtained from a global list of about 8,500 plant taxonomists names
#' constructed by Lima et al. (2020) and provided with __plantR__. This
#' list was built based on information from the
#' [Harvard University Herbaria](http://kiki.huh.harvard.edu/databases), the
#' [Brazilian Herbaria Network](http://www.botanica.org.br/rbh) and the
#' [American Society of Plant Taxonomists](https://members.aspt.net).
#' The dictionary was manually complemented for missing names of taxonomists and
#' it includes common variants of taxonomists names (e.g., missing initials,
#' typos, married or maiden names).
#'
#' If a column containing the Darwin Core field 'basisOfRecord' or equivalent is
#' provided ('rec.type' in argument `col.names`), then by default, __all
#' occurrences that are not preserved specimens (i.e. human/machine
#' observations, photos, living specimens, etc.) are classified as having a low
#' confidence level__.
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
#' names is currently biased towards plant collectors in South America,
#' particularly in Brazil.
#'
#' The argument `other.records` controls what to do with types of records which
#' are not preserved specimens (Darwin Core field
#' [basisOfRecord](http://rs.tdwg.org/dwc/terms/basisOfRecord). If the argument
#' is NULL (default), all record types are treated the same. Users can set the
#' argument to one of the confidence levels (i.e. 'unknown', 'low', 'medium' or
#' 'high') to assign the same class for all non preserved specimens or to a
#' value (i.e. 1 or 2), which correspond to the number of downgrading steps
#' among levels. For instance, if `other.records` is one, the 'high' level
#' becomes 'medium' and the 'medium' level becomes 'low' ('unknown' and 'low'
#' levels remain the same).
#'
#' If you miss the validation from one or more taxonomists, you can include them
#' using the argument `miss.taxonomist`. The format should be:
#' the name of the family of specialty followed by an underscore and then
#' the taxonomist name in the TDWG format (e.g. "Bignoniaceae_Gentry, A.H.").
#'
#' A database of taxonomists different than the `plantR` default can be used.
#' This database must be provided using the argument `taxonomist.list` and it
#' must contain the columns 'family' and 'tdwg.name'. The first column is the
#' family of specialty of the taxonomist and the second one is her/his name in
#' the TDWG format. See `plantR` function `prepName()` on how to get names in
#' the TDWG format from a list of people's names.
#'
#' Finally, the user can provide a list of records that should be flagged as
#' having a high confidence level on their identification. This list should be
#' provided using the argument `voucher.list` and the information that should be
#' provided is the record unique identifier (i.e. combination of collection code
#' and number). It is important that the way in which the list of unique
#' identifiers was generated matches the one used to construct the the
#' identifiers in the input data frame \code{x} (see help of function
#' `getTombo()`). If a list of records is provided, the user must also
#' provide a valid column name in \code{x} containing the unique record
#' identifiers in `col.names`.
#'
#' @return The input data frame \code{x}, plus a new column 'tax.check'
#'   containing the classes of confidence in species identifications.
#'
#' @seealso
#'  \link[plantR]{prepName} and \link[plantR]{getTombo}.
#'
#' @references
#' Lima, R.A.F. et al. 2020. Defining endemism levels for biodiversity
#' conservation: Tree species in the Atlantic Forest hotspot. Biological
#' Conservation, 252: 108825.
#'
#' @importFrom stringr str_trim str_replace_all
#' @importFrom utils head
#'
#' @export validateTax
#'
#' @examples
#' (df <- data.frame(
#' family.new = c("Bignoniaceae", "Bignoniaceae","Bignoniaceae",
#' "Bignoniaceae","Bignoniaceae","Bignoniaceae"),
#' identifiedBy.new = c("Gentry, A.H.", "Hatschbach, G.", NA, NA, NA, "Hatschbach, G."),
#' recordedBy.new = c(NA, NA, NA, "Gentry, A.H.", NA, NA),
#' typeStatus = c(NA, NA, "isotype", NA, NA, NA),
#' numTombo = c("a_1","b_3","c_7","d_5","e_3","f_4"),
#' stringsAsFactors = FALSE))
#'
#' validateTax(df)
#' validateTax(df, generalist = TRUE)
#' validateTax(df, voucher.list = "f_4")
#'
validateTax <- function(x, col.names = c(family = "family.new",
                                         det.name = "identifiedBy.new",
                                         col.name = "recordedBy.new",
                                         types = "typeStatus",
                                         rec.ID = "numTombo",
                                         rec.type = "basisOfRecord"),
                        special.collector = TRUE,
                        generalist = FALSE,
                        generalist.class = "medium",
                        other.records = NULL,
                        miss.taxonomist = NULL,
                        taxonomist.list = "plantR",
                        voucher.list = NULL,
                        noName = c("semdeterminador",
                                   "anonymus",
                                   "anonymous",
                                   "anonimo",
                                   "incognito",
                                   "unknown",
                                   "s.d.",
                                   "s.n."),
                        top.det = 10) {

  #Checking the input
  if (!class(x) == "data.frame")
    stop("Input object needs to be a data frame!")

  #list of column names in the data
  id.cols <- col.names %in% names(x)

  #second try without the '.new' suffix
  if (any(!id.cols)) {
    col.names.sub <- gsub("\\.new", "", col.names)
    id.cols.sub <- col.names.sub %in% names(x)
    col.names[!id.cols & id.cols.sub] <-
      col.names.sub[!id.cols & id.cols.sub]
    id.cols[!id.cols & id.cols.sub] <- TRUE
  }

  if (all(!id.cols))
    stop("Please provide the necessary columns names to assess the confidence in species identifications")

  #filtering only the columns found in the data
  cols <- col.names[id.cols]
  cols.miss <- col.names[!id.cols]

  if (length(cols.miss) > 0 ) {
    if ("family" %in% names(cols.miss) | "det.name" %in% names(cols.miss))
      stop("Input data frame needs at least the columns with the record botanical family and identifier")

    if (!is.null(voucher.list) & "rec.ID" %in% names(cols.miss))
      stop("Please provide a valid column name containing the record unique IDs")
  }

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

    autores <- autores[!grepl('Generalist', autores$family, fixed = TRUE),]

  } else {

    generalists <- autores[grepl('Generalist', autores$family, fixed = TRUE),]
    autores <- autores[!grepl('Generalist', autores$family, fixed = TRUE),]

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
  combo.occs <- paste(x[ ,cols["family"]],
                      x[ ,cols["det.name"]], sep = "_")
  combo.occs <- stringr::str_trim(combo.occs)

  #Crossing the occurrence and reference family-specialist combinations
  x$tax.check <- combo.occs %in% combo

  #Validating all type specimens (isotype, paratypes, etc) but not the "not a type"
  if ("types" %in% names(cols))
    x$tax.check[!is.na(x[, cols["types"]]) &
                  !grepl("not a type|notatype|probable type|tipo provavel|tipo prov\u00e1vel",
                         x[, cols["types"]], perl = TRUE, ignore.case = TRUE)] <- TRUE

  #Specifying occurrences with unknown determiner name
  x$tax.check[x$tax.check == FALSE &
                tolower(x[, cols["det.name"]]) %in% noName] <- "unknown"
  x$tax.check[x$tax.check %in% FALSE &
                is.na(x[, cols["det.name"]])] <- "unknown"

  #Validating all specimens collected by the family specialist but with the determiner field empty
  if (special.collector) {

    # if (!is.na(covs.present[["collectors"]])) {
    if ("col.name" %in% names(cols)) {

      combo2 <- paste(x[, cols["family"]],
                      x[, cols["col.name"]], sep = "_")
      combo2 <- stringr::str_trim(combo2)
      #Crossing the occurrence and reference family-specialist combinations
      tax.check1 <- combo2 %in% combo
      x$tax.check[x$tax.check %in% c("unknown") &
                    tax.check1 %in% TRUE] <- TRUE

    } else {
      warning("Argument 'special.collector' set to TRUE but collector name is missing")
    }
  }

  if (generalist) {

    #Crossing the occurrences with the names of the generalists
    tax.check2 <- x[, cols["det.name"]] %in% generalists$tdwg.name
    x$tax.check[x$tax.check %in% c("FALSE") &
                  tax.check2 %in% TRUE] <- generalist.class
  }

  x$tax.check[x$tax.check %in% "FALSE"] <- "low"
  x$tax.check[x$tax.check %in% "TRUE"] <- "high"

  #Any potential specialists missing form the taxonomist list?
  non.tax.det <- sort(table(x[,cols["det.name"]][x$tax.check %in% "low"]))
  if (length(non.tax.det) > 0) {
    non.tax.det.df <- data.frame(names(non.tax.det), as.double(non.tax.det))
    row.names(non.tax.det.df) <- NULL
    non.tax.det.df <- non.tax.det.df[order(non.tax.det.df[,2], decreasing = TRUE),]
    cat("Top people with many determinations but not in the taxonomist list: \n",
        knitr::kable(utils::head(non.tax.det.df, top.det),
        # knitr::kable(my.head.df(non.tax.det.df, top.det),
                     row.names = FALSE, col.names = c("Identifier", "Records")),"", sep = "\n")
  }

  #Assigning different levels to non preserved specimens
  if ("rec.type" %in% names(cols) & !is.null(other.records)) {
    PS <- c("preservedspecimen","preserved_specimen","s","exsicata de planta",
            "esicata de planta","exsicata")

    if (class(other.records) == "character")
      if (other.records %in% c("unknown", "low", "medium", "high")) {
        x$tax.check[!is.na(x[, cols["rec.type"]]) |
                    !tolower(x[, cols["rec.type"]]) %in% PS] <- other.records
      } else {
        warning("If a character, argument `other.records` must be 'unknown', 'low', 'medium' or 'high') (no changes performed)")
      }

    if (class(other.records) == "numeric") {
      if (is.integer(other.records)) {
      current <- x$tax.check[!is.na(x[, cols["rec.type"]]) |
                    !tolower(x[, cols["rec.type"]]) %in% PS]
      rpl1 <- c("unknown" = "-1", "low" = "0", "medium" = "1", "high" = "2")
      current <- stringr::str_replace_all(current, rpl1)
      current <- as.double(current) - as.double(other.records)

      rpl2 <- c("unknown", "low", "medium", "high")
      names(rpl2) <- as.character(c(-1, 0, 1, 2) - as.double(other.records))
      rpl2[-1][as.double(names(rpl2)[-1]) <= 0] <- "low"
      rpl2[-1][as.double(names(rpl2)[-1]) == 1] <- "medium"
      rpl2[-1][as.double(names(rpl2)[-1]) == 2] <- "high"
      current <- stringr::str_replace_all(current, rpl2)
      x$tax.check[!is.na(x[, cols["rec.type"]]) |
                    !tolower(x[, cols["rec.type"]]) %in% PS] <- current

      } else {
        warning("If a number, argument `other.records` must be an integer (no changes performed)")
      }
    }
  }

  #Assigning high confidence levels to the user-provided voucher list
  if (!is.null(voucher.list))
    x$tax.check[x[, cols["rec.ID"]] %in% voucher.list] <- "high"

  return(x)
}
