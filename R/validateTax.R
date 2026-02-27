#' @title Confidence on Species Identification
#'
#' @description This function assigns different categories of
#'   confidence level (i.e. high, medium, low or unknown) to the
#'   identification of species records, based on the name of the
#'   person who provided the species identification and on type
#'   specimens.
#'
#' @param x a data frame with the species records.
#' @param col.names vector. A named vector containing the names of
#'   columns in the input data frame for each of the information
#'   needed to assign confidence levels to species identifications.
#'   Default to the __plantR__ output column names.
#' @param special.collector Logical. Specimens collected by the taxon
#'   specialist but with empty determiner field, should be classified
#'   as high confidence level? Default to TRUE.
#' @param aux.identifier Logical. Should the names other than the
#'   first person that did the identification be considered for
#'   taxonomic validation? Default to TRUE.
#' @param generalist Logical. Should taxon generalists be considered
#'   for taxonomic validation? Default to FALSE.
#' @param generalist.class Character. Confidence level to be assigned
#'   to taxon generalists. Default to "medium". Can be "high",
#'   "medium" or "low".
#' @param other.records Character or Integer. The Confidence level (if
#'   character) or the number of downgrading steps to be assigned to
#'   records which are not preserved specimens. Default to NULL (all
#'   record types are treated the same).
#' @param miss.taxonomist Vector. Any missing combination of taxon and
#'   taxonomist that should be added to the validation?
#' @param taxonomist.list a data.frame containing the list of
#'   taxonomist names. The default is "plantR", the internal __plantR__
#'   global database of plant taxonomists (see Details).
#' @param kingdom character. The name of the kingdom or phyllum that
#'   the taxa belong. Defaults to "plantae" which refers to vascular
#'   plants (i.e. Tracheophyta).
#' @param voucher.list Vector. One or more unique record identifiers
#'   (i.e. combination of collection code and number) that should be
#'   flagged with a high confidence level? Default to NULL.
#' @param rec.ID Character. The name of the columns containing the
#'   unique record identifier (see function `getTombo()`). Default to
#'   'numTombo'.
#' @param noName Vector. One or more characters (in lower cases) with
#'   the standard notation for missing data in the field 'det.name'.
#'   Default to some typical notation found in herbarium data.
#' @param top.det Numerical. How many of the top missing identifiers
#'   should be printed? Default to 10.
#' @param print logical. Should the table of missing identifiers be
#'   printed? Default to TRUE.
#'
#' @details
#' The input data frame \code{x} must contain at least the columns
#' with the information on the family, order or class of the species
#' and the names of who provided the species identification.
#' Preferably, this data frame should also contain information on type
#' specimens and collectors names. If the user provide a list of
#' records to be flagged as having a high confidence level in the
#' identification, the user must also provide the column where the
#' record unique identifiers are stored. The names of these columns
#' should be provided as a named vector to the argument `col.names`,
#' as follows:
#'   - 'class': the taxonomic class (default: 'class')
#'   - 'order': the taxonomic order (default: 'order')
#'   - 'family': the taxonomic family (default: 'family.new')
#'   - 'det.name': the identifier name (default: 'identifiedBy.new')
#'   - 'det.name.aux': the other identifier name (default: 'identifiedBy.aux')
#'   - 'col.name': the collector name (default: 'recordedBy.new')
#'   - 'types': type specimens (default: 'typeStatus')
#'   - 'rec.ID': the collector serial number (default: 'numTombo')
#'   - 'rec.type': the type of record (default: 'basisOfRecord')
#'
#' As for other functions in __plantR__, using a data frame \code{x}
#' that has already passed by the editing steps of the __plantR__
#' workflow should result in more accurate outputs.
#'
#' The output of the function contains a new column called 'tax.check'
#' with the categories of confidence level in species identifications,
#' which are 'high', 'medium', 'low' or 'unknown' (in a decreasing
#' order of confidence). The category 'unknown' means that the
#' assignment of the confidence levels could not be done because the
#' information in the column 'det.name' in argument `col.names` was
#' either empty or equal to what is defined by the argument `noName`
#' (e.g. "anonymous", "incognito", "unknown", "s.d.", "s.n.").
#'
#' The function classifies as high confidence level all records whose
#' species identifications were performed by a family specialist or
#' any type specimens (isotype, paratypes, etc). By default, the names
#' of taxonomic specialists are obtained from a global list of
#' taxonomists names initially constructed by Lima et al. (2020) and
#' provided internally by __plantR__. This list was mainly built based
#' on information from the [Harvard University Herbaria](http://kiki.huh.harvard.edu/databases),
#' the [Brazilian Herbaria Network](http://www.botanica.org.br/rbh),
#' the [American Society of Plant Taxonomists](https://members.aspt.net),
#' the [Contributors to the WFO TENs](https://about.worldfloraonline.org/tens),
#' the [POWO Reviewers](https://powo.science.kew.org/compilers-and-reviewers)
#' and the [Taxonomic Catalog of the Brazilian Fauna](http://fauna.jbrj.gov.br/).
#' The dictionary was manually complemented for missing names of
#' taxonomists and it includes common variants of taxonomists names
#' (e.g., missing initials, typos, married or maiden names).
#'
#' If a column containing the Darwin Core field 'basisOfRecord' or
#' equivalent is provided ('rec.type' in argument `col.names`), then
#' by default, __all occurrences that are not preserved specimens
#' (i.e. human/machine observations, photos, living specimens, etc.)
#' are classified as having a low confidence level__.
#'
#' Some records are collected by a specialist of a given taxon, but the
#' the name of the person that provided the species identification is
#' missing. By default, we assume the same confidence level for these
#' records as that assigned for records where the identifier is
#' the taxonomic specialist. But users can choose otherwise by setting
#' the argument `special.collector` to FALSE.
#'
#' Some records are identified by more than one person (e.g. 'Junikka,
#' L. & Maas, P.J.M.'). In previous versions of __plantR__ (<0.2) only
#' the first name was considered to validate the species
#' identification. Now, by default, the name stored in the field
#' 'identifiedBy.aux', generated by the __plantR__ workflow, is also
#' used for the validation. But users can choose otherwise by setting
#' the argument `aux.identifier` to FALSE.
#'
#' The arguments `generalist` and `generalist.class` define if
#' taxonomists that provide identifications at taxonomic levels above
#' the family-level (i.e. order or class), sometimes referred to as a
#' generalist, should be considered in the validation and under which
#' confidence level. This is done independently if the taxonomist is
#' an specialist of one or more families within his group of study,
#' which will render a high-level of taxonomic confidence. There are
#' some names of generalists in the __plantR__ default taxonomist
#' database; however, this list of generalist names is currently
#' biased towards plant collectors in South America, particularly in
#' Brazil.
#'
#' The argument `other.records` controls what to do with types of
#' records which are not preserved specimens (Darwin Core field
#' [basisOfRecord](http://rs.tdwg.org/dwc/terms/basisOfRecord)). If
#' the argument is NULL (default), all record types are treated the
#' same, that is they receive a low confidence level. But users can
#' set the argument to any of the confidence levels (i.e. 'unknown',
#' 'low', 'medium' or 'high') to assign the same class for all non
#' preserved specimens or to a value (i.e., 1 or 2), which correspond
#' to the number of downgrading steps among levels. For instance, if
#' `other.records` is one, the 'high' level becomes 'medium' and the
#' 'medium' level becomes 'low' ('unknown' and 'low' levels remain the
#' same).
#'
#' If you miss the validation from one or more taxonomists, you can
#' include them using the argument `miss.taxonomist`. The format
#' should be: the name of the taxon of specialty followed by an
#' underscore and then the taxonomist name in the TDWG format (e.g.
#' "Bignoniaceae_Gentry, A.H.").
#'
#' A database of taxonomists different than the __plantR__ default can
#' be used. This database must be provided using the argument
#' `taxonomist.list` and it must contain at least the columns 'tax'
#' and 'tdwg.name'. The first column is the taxon of specialty of the
#' taxonomist and the second one is her/his name in the TDWG standard
#' format. If available the taxonomic class and order are also used,
#' which is a more common case for animal taxonomists. See __plantR__
#' function `prepName()` on how to get names in the TDWG format from a
#' list of people's names.
#'
#' The user can provide a list of records that should be flagged as
#' having a high confidence level on their identification. This list
#' should be provided using the argument `voucher.list` and the
#' information that should be provided is the record unique identifier
#' (i.e. combination of collection code and number). It is important
#' that the way in which the list of unique identifiers was generated
#' matches the one used to construct the the identifiers in the input
#' data frame \code{x} (see help of function `getTombo()`). If a list
#' of records is provided, the user must also provide a valid column
#' name in \code{x} containing the unique record identifiers in
#' `col.names`.
#'
#'   The output of this function contains columns which are reserved
#'   within the __plantR__ workflow. These columns cannot be present
#'   in the input data frame. The full list of reserved columns is
#'   stored in the internal object `reservedColNames`.
#'
#' @return The input data frame \code{x}, plus a new column 'tax.check'
#'   containing the classes of confidence in species identifications.
#'
#' @author Renato A. Ferreira de Lima
#'
#' @seealso
#'  \link[plantR]{prepName} and \link[plantR]{getTombo}.
#'
#' @references
#' Lima, R.A.F. et al. 2020. Defining endemism levels for biodiversity
#' conservation: Tree species in the Atlantic Forest hotspot. Biological
#' Conservation, 252: 108825.
#'
#' @importFrom stringr str_replace_all
#' @importFrom utils head
#' @importFrom dplyr left_join
#' @importFrom knitr kable
#'
#' @export validateTax
#'
#' @examples
#' df <- data.frame(
#' family.new = c("Bignoniaceae", "Bignoniaceae","Bignoniaceae",
#' "Bignoniaceae","Bignoniaceae","Bignoniaceae","Bignoniaceae"),
#' identifiedBy.new = c("Gentry, A.H.", "Hatschbach, G.", NA, NA, NA,
#' "Hatschbach, G.", "Hatschbach, G."),
#' identifiedBy.aux = c(NA, NA, NA, NA, NA, NA, "Gentry, A.H."),
#' recordedBy.new = c(NA, NA, NA, "Gentry, A.H.", NA, NA, NA),
#' typeStatus = c(NA, NA, "isotype", NA, NA, NA, NA),
#' numTombo = c("a_1","b_3","c_7","d_5","e_3","f_4","h_8"))
#'
#' validateTax(df)
#' validateTax(df, generalist = TRUE)
#' validateTax(df, aux.identifier = FALSE)
#' validateTax(df, voucher.list = "f_4")
#'
validateTax <- function(x, col.names = c(class = "class",
                                         order = "order",
                                         family = "family.new",
                                         det.name = "identifiedBy.new",
                                         det.name.aux = "identifiedBy.aux",
                                         col.name = "recordedBy.new",
                                         types = "typeStatus",
                                         rec.ID = "numTombo",
                                         rec.type = "basisOfRecord"),
                        special.collector = TRUE,
                        aux.identifier = TRUE,
                        generalist = FALSE,
                        generalist.class = "medium",
                        other.records = NULL,
                        miss.taxonomist = NULL,
                        taxonomist.list = "plantR",
                        kingdom = "plantae",
                        voucher.list = NULL,
                        noName = c("semdeterminador",
                                   "anonymus",
                                   "anonymous",
                                   "anonimo",
                                   "incognito",
                                   "unknown",
                                   "s.d.",
                                   "s.d",
                                   "s.n.",
                                   "s.n"),
                        top.det = 10,
                        print = TRUE) {

  #Checking the input
  if (!inherits(x, "data.frame"))
    stop("Input object needs to be a data frame!")

  if (!inherits(x, "tbl"))
    x <- as.data.frame(x)

  if (dim(x)[1] == 0)
    stop("Input data frame is empty!")

  #Checking the presence of reserved columns in the input dataset
  x <- checkColNames(x, group = "validate.tax")

  #List of column names in the data
  id.cols <- col.names %in% names(x)

  #Second try without the '.new' suffix
  if (any(!id.cols)) {
    col.names.sub <- gsub("\\.new", "", col.names)
    id.cols.sub <- col.names.sub %in% names(x)
    col.names[!id.cols & id.cols.sub] <-
      col.names.sub[!id.cols & id.cols.sub]
    id.cols[!id.cols & id.cols.sub] <- TRUE
  }

  if (all(!id.cols))
    stop("Please provide the necessary columns names to assess the confidence in species identifications")

  #Filtering only the columns found in the data
  cols <- col.names[id.cols]
  cols.miss <- col.names[!id.cols]

  if (length(cols.miss) > 0 ) {
    if ("family" %in% names(cols.miss) | "det.name" %in% names(cols.miss))
      stop("Input data frame needs at least the columns with the record taxonomy and identifier")

    if (!is.null(voucher.list) & "rec.ID" %in% names(cols.miss))
      stop("Please provide a valid column name containing the record unique IDs")
  }

  #Getting the dictionaries
  families.syn <- familiesSynonyms
  if (tolower(kingdom) %in% unique(tolower(families.syn$kingdom))) {
    families.syn <-
      families.syn[tolower(families.syn$kingdom) %in% tolower(kingdom), ]
  } else {
    warning("Kingdom not found or currently not available.")
  }

  if (all(taxonomist.list %in% c("plantR", "plantr"))) {

    autores <- taxonomists
    if (tolower(kingdom) %in% unique(tolower(autores$tax.kingdom))) {
      autores <-
        autores[tolower(autores$tax.kingdom) %in% tolower(kingdom), ]
    } else {
      warning("Kingdom not found or currently not available.")
    }
    autores$order <- 1:dim(autores)[1]
    autores <- merge(autores,
                     families.syn[, c("name", "name.correct")],
                     by.x = "tax", by.y = "name", all.x = TRUE)
    autores <- autores[order(autores$order),]
    autores$order <- NULL

  } else {

    if (!inherits(taxonomist.list, "data.frame"))
      stop("The list of taxonomists must be provided as a data frame")

    if (!all(c("tax", "tdwg.name") %in% names(taxonomist.list)))
      stop("The list of taxonomists must contain at least two columns: 'tax' and 'tdwg.name'")

    autores <- taxonomist.list
    autores <- merge(autores,
                     families.syn[, c("name", "name.correct")],
                     by.x = "tax", by.y = "name", all.x = TRUE)
  }

  if(!generalist) {

    # autores <-
    #   autores[!grepl('Generalist', autores$tax, fixed = TRUE),]
    autores <-
      autores[autores$tax.rank %in% "family",]

  } else {

    # generalists <-
    #   autores[grepl('Generalist', autores$tax, fixed = TRUE),]
    # autores <-
    #   autores[!grepl('Generalist', autores$tax, fixed = TRUE),]
    generalists <-
      autores[!autores$tax.rank %in% "family",]
    autores <-
      autores[autores$tax.rank %in% "family",]

    #Checking confidence classes for generalists
    if (is.null(generalist.class))
      stop("Please provide a confidence level to be assigned to generalists")

    if (is.null(generalist.class))
      stop("Please provide a confidence level to be assigned to generalists")

    if (!generalist.class %in% c("high", "medium", "low"))
      stop("Please provide a valid name for the confidence level assigned to generalists")

    if (col.names["class"] %in% names(cols.miss) |
          col.names["order"] %in% names(cols.miss)) {

      tmp <- dplyr::left_join(x, families.syn,
                              by = setNames("name", cols["family"]))

      if (col.names["class"] %in% names(cols.miss)) {
        x[[col.names["class"]]] <- tmp$class.correct
      } else {
        rep_these <- is.na(x[[col["class"]]]) & !is.na(tmp$class.correct)
        if (any(rep_these))
          x[[col["class"]]][rep_these] <- tmp$class.correct[rep_these]
      }
      if ("order" %in% names(cols.miss)) {
        x[[col.names["order"]]] <- tmp$order.correct
      } else {
        rep_these <- is.na(x[[col["order"]]]) & !is.na(tmp$order.correct)
        if (any(rep_these))
          x[[col["order"]]][rep_these] <- tmp$order.correct[rep_these]
      }
      message("Columns with the taxonomic class or order not found. Info added to input based on family names")
    }
  }

  #Getting the unique taxon-specialist combinations from the reference list
  combo <- unique(paste(autores$tax, autores$tdwg.name, sep = "_"))
  if (!is.null(miss.taxonomist))
    combo <- c(combo, miss.taxonomist)

  if (all(taxonomist.list %in% c("plantR", "plantr"))) {
    tmp <- unique(paste(autores$name.correct[!is.na(autores$name.correct)],
                        autores$tdwg.name[!is.na(autores$name.correct)],
                        sep = "_"))
    tmp <- tmp [!tmp %in% combo]
    combo <- c(combo, tmp)
  }
  combo <- squish(combo)

  #Getting the unique family-specialist combinations for each occurrence
  combo.occs <- paste(x[[cols["family"]]],
                      x[[cols["det.name"]]], sep = "_")
  combo.occs <- squish(combo.occs)

  #Crossing the occurrence and reference family-specialist combinations
  x$tax.check <- combo.occs %in% combo

  #Validating all type specimens (isotype, paratypes, etc) but not the "not a type"
  if ("types" %in% names(cols)) {
    not.type <- "not a type|notatype|probable type|tipo provavel|tipo prov\u00e1vel"
    x$tax.check[!x[[cols["types"]]] %in% c(NA, "") &
                  !grepl(not.type, x[[cols["types"]]],
                         perl = TRUE, ignore.case = TRUE)] <- TRUE
  }

  #Specifying occurrences with unknown determiner name
  rep_these <- x$tax.check == FALSE &
                tolower(x[[cols["det.name"]]]) %in% c(NA, noName)
  if (any(rep_these))
    x$tax.check[rep_these] <- "unknown"
    # x$tax.check[x$tax.check %in% FALSE &
    #             is.na(x[[cols["det.name"]]])] <- "unknown"

  #Validating all specimens collected by the family specialist but with the determiner field empty
  if (special.collector) {

    # if (!is.na(covs.present[["collectors"]])) {
    if ("col.name" %in% names(cols)) {

      combo2 <- paste(x[[cols["family"]]],
                      x[[cols["col.name"]]], sep = "_")
      combo2 <- squish(combo2)

      #Crossing the occurrence and reference family-specialist combinations
      tax.check1 <- combo2 %in% combo
      rep.id <- x$tax.check %in% c("unknown") & tax.check1 %in% TRUE
      if (any(rep.id))
        x$tax.check[rep.id] <- TRUE

    } else {
      warning("Argument 'special.collector' set to TRUE but collector name is missing")
    }
  }

  #Validating all specimens taking into account the others identifiers
  if (aux.identifier) {

    if ("det.name.aux" %in% names(cols)) {

      if (any(!is.na(x[[cols["det.name.aux"]]]))) {
        other.names <- as.character(x[[cols["det.name.aux"]]])
        lista <- lapply(strsplit(other.names, ";|\\||&", perl = TRUE),
                        squish)
        combo3 <- mapply(paste, x[[cols["family"]]], lista, sep = "_")
        # other.names <- gsub(";.*|\\|.*", "", other.names, perl = TRUE)
        #
        # combo3 <- paste(x[[cols["family"]]], other.names, sep = "_")
        # combo3 <- squish(combo3)

        #Crossing the occurrence and reference family-specialist combinations
        tax.check2 <-
          as.logical(sapply(combo3, function(x) any(x %in% combo)))
        # tax.check2 <- combo3 %in% combo
        rep.id <- x$tax.check %in% c("unknown", "FALSE", FALSE) &
                    tax.check2 %in% TRUE
        if (any(rep.id))
          x$tax.check[rep.id] <- TRUE
      }

    } else {
      warning("Argument 'aux.identifier' set to TRUE but name of the auxiliary identifier is missing")
    }
  }

  if (any(!rep_these)) {

    #Validating specimens taking into account generalist identifiers
    if (generalist) {

      combo.gen <- unique(paste(generalists$tax, generalists$tdwg.name,
                                sep = "_"))

      #Getting generalist groups
      if (special.collector) {
        groups <- data.frame(name = x[[cols["family"]]],
                             id.By = x[[cols["det.name"]]],
                             col.By = x[[cols["col.name"]]])
        rep_these1 <- is.na(groups$id.By) & !is.na(groups$col.By)
        if (any(rep_these1))
          groups$id.By[rep_these1] <- groups$col.By[rep_these1]
      } else {
        groups <- data.frame(name = x[[cols["family"]]],
                             id.By = x[[cols["det.name"]]])
      }
      families.syn1 <- families.syn[!duplicated(families.syn$name), ]
      groups <- dplyr::left_join(groups, families.syn1, by = "name")

      #Getting the generalist combos
      combo.occs.order <- paste(groups[["order.correct"]],
                                groups[["id.By"]], sep = "_")
      combo.occs.class <- paste(groups[["class.correct"]],
                                groups[["id.By"]], sep = "_")

      #Crossing the occurrences with the names of the generalists
      # tax.check2 <- x[, cols["det.name"]] %in% generalists$tdwg.name
      tax.check2 <- combo.occs.order %in% combo.gen |
                      combo.occs.class %in% combo.gen
      if (any(tax.check2))
        x$tax.check[x$tax.check %in% c(FALSE, "FALSE") &
                      tax.check2 %in% TRUE] <- generalist.class

      x[[col.names["class"]]] <- NULL
      x[[col.names["order"]]] <- NULL
    }

    x$tax.check <- as.character(x$tax.check)
    x$tax.check[x$tax.check %in% "FALSE"] <- "low"
    x$tax.check[x$tax.check %in% "TRUE"] <- "high"

    #Any potential specialists missing from the taxonomist list?
    non.tax.det <- sort(table(x[[cols["det.name"]]][x$tax.check %in% "low"]))
    if (length(non.tax.det) > 0) {
      non.tax.det.df <- data.frame(names(non.tax.det), as.double(non.tax.det))
      row.names(non.tax.det.df) <- NULL
      non.tax.det.df <- non.tax.det.df[order(non.tax.det.df[,2], decreasing = TRUE),]
      if (print) {
        cat("Top people with many determinations but not in the plantR taxonomist list: \n",
            knitr::kable(utils::head(non.tax.det.df, top.det),
                         # knitr::kable(my.head.df(non.tax.det.df, top.det),
                         row.names = FALSE, col.names = c("Identifier", "Records")),"", sep = "\n")
      }
    }
  }


  #Assigning different levels to non preserved specimens
  if ("rec.type" %in% names(cols) & !is.null(other.records)) {
    PS <- c("preservedspecimen", "preserved_specimen", "s",
            "exsicata de planta", "esicata de planta", "exsicata")

    if (is.character(other.records))
      if (other.records %in% c("unknown", "low", "medium", "high")) {
        check_these <- !x[[cols["rec.type"]]] %in% c("", " ", NA) &
                          !tolower(x[[cols["rec.type"]]]) %in% PS
        if (any(check_these))
          x$tax.check[check_these] <- other.records
      } else {
        warning("If a character, 'other.records' must be 'unknown', 'low', 'medium' or 'high') (no changes performed)")
      }

    if (is.numeric(other.records)) {

      if (!is.integer(other.records)) {
        other.records <- as.integer(round(other.records, 0))
      }

      if (is.integer(other.records)) {
        check_these <- !x[[cols["rec.type"]]] %in% c("", " ", NA) &
                          !tolower(x[[cols["rec.type"]]]) %in% PS
        if (any(check_these)) {
          current <- x$tax.check[check_these]
          rpl1 <- c("unknown" = "-1", "low" = "0", "medium" = "1", "high" = "2")
          current <- stringr::str_replace_all(current, rpl1)
          current <- as.double(current) - as.double(other.records)

          rpl2 <- c("unknown", "low", "medium", "high")
          names(rpl2) <- as.character(c(-1, 0, 1, 2) - as.double(other.records))
          rpl2[-1][as.double(names(rpl2)[-1]) <= 0] <- "low"
          rpl2[-1][as.double(names(rpl2)[-1]) == 1] <- "medium"
          rpl2[-1][as.double(names(rpl2)[-1]) == 2] <- "high"
          current <- stringr::str_replace_all(current, rpl2)
          x$tax.check[check_these] <- current
        }
      } else {
        warning("If a number, argument 'other.records' must be an integer (no changes performed)")
      }
    }
  }

  #Assigning high confidence levels to the user-provided voucher list
  if (!is.null(voucher.list))
    x$tax.check[x[[cols["rec.ID"]]] %in% voucher.list] <- "high"

  return(x)
}
