#' @title Standardize Herbarium Codes
#'
#' @description Search for variants of collection and institution codes and
#'   return their standard codes based on the Index Herbariorum or the Index
#'   Xylariorum, if available.
#'
#' @param x a data frame containing the collection codes.
#' @param inst.code character. The column name containing the institution codes.
#' @param col.code character. The column name containing the collection codes.
#' @param drop character. List of columns names that should be droped from the
#'   output.
#' @param print.miss logical.
#'
#' @return the data frame \code{x} with (at least) the additional columns called
#'   'collectionCode.new' and 'collectionObs'.
#'
#' @details
#'
#' The information on collection codes and names stored within __plantR__ is
#' currently biased towards plant-related collections. The main sources
#' of information used to construct the database of collection codes were the
#' \href{http://sweetgum.nybg.org/science/ih/}{Index Herbariorum} (until 2019), the
#' \href{https://www.botanica.org.br/a-rede-brasileira-de-herbarios/}{Brazilian
#' Herbaria Network}, the
#' \href{https://globaltimbertrackingnetwork.org/products/iawa-index-xylariorum/}{Index
#' Xylariorum} v4.1 and \href{https://www.gbif.org/}{GBIF}.
#'
#' There is variation in the notation of institutions and collection codes in
#' GBIF and other data repositories. Sometimes, the same collection is referred
#' differently between these repositories. Thus, this function tried to provide
#' a common code for each institution. To avoid mismatches the collection codes
#' are compared to the list of collections using a string which combines both the
#' institution and collection code.
#'
#' If the collection code in not found in the Index Herbariorum, the same
#' collection code is returned without modifications. A mention if the code is
#' not found is stored in the 'collectionObs'. The argument `print.miss` can be
#' set to TRUE if the user wants to print the table of collections not found.
#'
#' @author Renato A. F. de Lima
#'
#' @import data.table
#' @importFrom stringr str_trim
#'
#' @export getCode
#'
#' @examples
#'
#' df <- data.frame(institutionCode = c("ASU", "UNEMAT", "MOBOT", "NYBG"),
#' collectionCode = c("ASU-PLANTS", "NX-FANEROGAMAS", "MO", "NY"),
#' stringsAsFactors = FALSE)
#' getCode(df)
#'
getCode <- function(x,
                    inst.code = "institutionCode",
                    col.code = "collectionCode",
                    drop = c("ordem.colecao","collectioncode.gbif",
                             "institutioncode.gbif", "organization",
                             "collection.string"),
                    print.miss = FALSE) {

  #Escaping R CMD check notes from using data.table syntax
  cod.inst.tmp <- cod.coll.tmp <- ordem.dados <- collection.string <- NULL
  index.herbariorum.or.working.code <- col.OBS <- NULL

  ## check input
  if (!class(x) == "data.frame")
    stop("input object needs to be a data frame!")

  if (!inst.code %in% names(x))
    stop("Input data frame must have a column with the institution codes")

  if (!col.code %in% names(x))
    stop("Input data frame must have a column with the collection codes")

  # Missing collection code that may be stored in the field 'institutionCode'
  if ("collectionCode" %in% names(x) & "institutionCode" %in% names(x)) {
    ids <- is.na(x$collectionCode) & !is.na(x$institutionCode)
    x$collectionCode[ids] <- x$institutionCode[ids]
  }

  dt <- data.table::data.table(x)
  dt[, cod.inst.tmp := .SD, .SDcols = c(inst.code)]
  dt[, cod.coll.tmp := .SD, .SDcols = c(col.code)]
  dt[ , ordem.dados := .I, ]

  ### Editing institution codes ###
  data.table::setkeyv(dt, "cod.inst.tmp")
  #removing numbers from the institution codes
  dt[ , cod.inst.tmp := gsub('[0-9]', '', cod.inst.tmp, perl = TRUE) , by = "cod.inst.tmp"]

  #Editing some collection codes with numbers
  data.table::setkeyv(dt, "cod.coll.tmp")
  dt[ , cod.coll.tmp := gsub('[0-9]', '', cod.coll.tmp, perl = TRUE) , by = "cod.coll.tmp"]

  ### Crossing with the herbaria list ###
  data.table::setkeyv(dt, "cod.coll.tmp")

  # Getting the search string for the data
  dt[ , collection.string := do.call(paste, c(.SD, sep="_")),
      .SDcols = c("cod.coll.tmp","cod.inst.tmp")]
  dt[ , collection.string := stringr::str_trim(collection.string)]

  # Getting the collection list
  ih.list <- collectionCodes
  ih.list <- data.table::data.table(ih.list)
  dt <- data.table::merge.data.table(dt, ih.list,
                                      by = "collection.string", all.x = TRUE)

  # Getting the list of missing collection codes and instituions
  miss.coll <- unique(dt[is.na(index.herbariorum.or.working.code), .SD,
                  .SDcols = c("collection.string", "cod.coll.tmp", "cod.inst.tmp")])
  names(miss.coll) <- c("string", "collectionCode","institutionCode")

  if (print.miss)
    cat("The following collections were not found:\n",
        knitr::kable(as.data.frame(miss.coll[,c(2,3)])),"",
        sep = "\n")


  # Replacing collection codes not found by the original ones
  dt[is.na(index.herbariorum.or.working.code),
     col.OBS := "code not found"]
  dt[is.na(index.herbariorum.or.working.code),
     index.herbariorum.or.working.code := cod.coll.tmp]

  #Making sure data is in the good order ad removing unecessary columns
  data.table::setorder(dt, "ordem.dados")
  dt[ , c("ordem.dados", "cod.inst.tmp", "cod.coll.tmp") := NULL]
  if (!is.null(drop)) {
    drop <- drop[drop %in% names(dt)]
    dt[ , c(drop) := NULL]
  }
  data.table::setnames(dt, c("index.herbariorum.or.working.code","col.OBS"),
                       c("collectionCode.new","collectionObs"))

  return(data.frame(dt))
}
