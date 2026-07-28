#' @title Standardize Herbarium Codes
#'
#' @description Search for variants of collection and institution
#'   codes and return their standard codes based on the Index
#'   Herbariorum or the Index Xylariorum, if available.
#'
#' @param x a data frame containing the collection codes.
#' @param inst.code character. The column name containing the
#'   institution codes.
#' @param col.code character. The column name containing the
#'   collection codes.
#' @param drop character. List of columns names that should be dropped
#'   from the output.
#' @param print.miss logical.
#'
#' @return the data frame \code{x} with (at least) the additional
#'   columns called 'collectionCode.new' and 'collectionObs'.
#'
#' @details
#'
#' The information on collection codes and names stored within
#' __plantR__ is currently biased towards plant-related collections.
#' The main sources of information used to construct the database of
#' collection codes were the
#' \href{http://sweetgum.nybg.org/science/ih/}{Index Herbariorum} (until 2025),
#' the
#' \href{https://www.botanica.org.br/a-rede-brasileira-de-herbarios/}{Brazilian
#' Herbaria Network}, the
#' \href{https://globaltimbertrackingnetwork.org/products/iawa-index-xylariorum/}{Index
#' Xylariorum} v4.1 and \href{https://www.gbif.org/}{GBIF}.
#'
#' There is variation in the notation of institutions and collection
#' codes in GBIF and other data repositories. Sometimes, the same
#' collection is referred to differently between these repositories.
#' Thus, this function tried to provide a common code for each
#' institution across collections.
#'
#' To avoid mismatches, the collection codes are first compared to the
#' list of collections using a string that combines both the
#' institution and collection codes. If a match is found using this
#' string, no observation is made and the valid collection code is
#' returned.
#'
#' If the institution code is not provided, a second comparison is
#' made using only the collection code. If a match is found, an
#' observation 'code partially found' is added to the column
#' 'collectionObs'.
#'
#' If no match is found after the two rounds of comparison, the input
#' collection code is returned without modifications. In this case, an
#' observation 'code not found' is provided in the column
#' 'collectionObs'. For missing or generic code information, the
#' observation 'cannot check' is returned. See examples below.
#'
#' Please note that there is a collection code called "NA" in the Index
#' Herbariorum. So, be cautious of potential false matches with the
#' R reserved code `NA` (i.e., not available).
#'
#' The argument `print.miss` can be set to TRUE if the user wants to
#' print the table of collections not found.
#'
#' The output of this function contains columns which are reserved
#' within the __plantR__ workflow. These columns cannot be present in
#' the input data frame. The full list of reserved columns is stored
#' in the internal object `reservedColNames`.
#'
#' @author Renato A. Ferreira de Lima
#'
#' @importFrom data.table as.data.table data.table setkeyv merge.data.table setorder setnames
#' @importFrom knitr kable
#'
#' @export getCode
#'
#' @examples
#'
#' inst <- c("ASU", "UNEMAT", "MOBOT", "NYBG", "", "")
#' coll <- c("ASU-PLANTS", "NX-FANEROGAMAS", "MO", "NY", "RB", "XX")
#' df <- data.frame(institutionCode = inst, collectionCode = coll)
#' getCode(df)
#'
getCode <- function(x,
                    inst.code = "institutionCode",
                    col.code = "collectionCode",
                    drop = c("collectioncode.gbif", "institutioncode.gbif",
                             "organization", "collection.string"),
                    print.miss = FALSE) {

  #Escaping R CMD check notes from using data.table syntax
  cod.inst.tmp <- cod.coll.tmp <- ordem..dados <- collection.string <- NULL
  index.herbariorum.or.working.code <- col.OBS <- NULL

  ## check input
  if (!inherits(x, "data.frame"))
    stop("input object needs to be a data frame!")

  if (dim(x)[1] == 0)
    stop("input data frame cannot be empty!")

  if (!inst.code %in% names(x))
    stop("Input data frame must have a column with the institution codes")

  if (!col.code %in% names(x))
    stop("Input data frame must have a column with the collection codes")

  # Detecting NAs
  na.col <- x[[col.code]] %in% c("", " ", NA)
  x[[col.code]][na.col] <- NA_character_
  na.inst <- x[[inst.code]] %in% c("", " ", NA)
  x[[inst.code]][na.inst] <- NA_character_

  # Missing collection codes stored in the field 'institutionCode'
  ids <- is.na(x[[col.code]]) & !is.na(x[[inst.code]])
  if (any(ids))
    x[[col.code]][ids] <- x[[inst.code]][ids]

  x$temp..order <- 1:dim(x)[1]
  dt <- data.table::data.table(x[!is.na(x[[col.code]]), ])
  dt[, cod.inst.tmp := .SD, .SDcols = c(inst.code)]
  dt[, cod.coll.tmp := .SD, .SDcols = c(col.code)]
  dt[ , ordem..dados := .I, ]

  ### Editing codes
  #Removing numbers from the institution codes
  data.table::setkeyv(dt, "cod.inst.tmp")
  dt[ , cod.inst.tmp := gsub('[0-9]', '', cod.inst.tmp, perl = TRUE) ,
      by = "cod.inst.tmp"]

  #Editing some collection codes with numbers and trailing symbols
  data.table::setkeyv(dt, "cod.coll.tmp")
  dt[ , cod.coll.tmp := gsub('[0-9]', '', cod.coll.tmp, perl = TRUE) ,
      by = "cod.coll.tmp"]
  dt[ , cod.coll.tmp := sub("-$|_$", "", cod.coll.tmp, perl = TRUE) ,
      by = "cod.coll.tmp"]

  dt[cod.inst.tmp == '', cod.inst.tmp := NA_character_]
  dt[cod.coll.tmp == '', cod.coll.tmp := NA_character_]

  ### Crossing with the herbaria list ###
  data.table::setkeyv(dt, "cod.coll.tmp")

  # Getting the search string for the data
  dt[ , collection.string := do.call(paste, c(.SD, sep="_")),
      .SDcols = c("cod.coll.tmp","cod.inst.tmp")]
  dt[ , collection.string := squish(collection.string)]

  # Getting the collection list
  ih.list <- data.table::data.table(collectionCodes)
  dt <- data.table::merge.data.table(dt, ih.list,
                                     by = "collection.string",
                                     all.x = TRUE)

  miss.string <-
    dt[is.na(index.herbariorum.or.working.code), collection.string]
  if (length(miss.string) > 0) {
    # Partial matches based only on collection codes
    cols2rep <- c("collectioncode.gbif", "institutioncode.gbif",
                  "index.herbariorum.or.working.code", "organization",
                  "col.OBS")
    ih.list1 <- ih.list[!is.na(index.herbariorum.or.working.code) &
                                is.na(col.OBS), .SD,
                               .SDcols = cols2rep]
    ih.list1 <-
      ih.list1[!duplicated(index.herbariorum.or.working.code), ]
    dt.miss <- data.table::merge.data.table(dt, ih.list1,
                                            by.x = "cod.coll.tmp",
                                            by.y = "index.herbariorum.or.working.code",
                                            all.x = TRUE,
                                            suffixes = c(".x", ""),
                                            sort = FALSE)
    rep_these <- dt$collection.string %in% miss.string &
                  is.na(dt.miss$organization.x) &
                  !is.na(dt.miss$organization)
    if (any(rep_these)) {
      cols2rep1 <- cols2rep
      cols2rep1[cols2rep1 %in% "index.herbariorum.or.working.code"] <-
        "cod.coll.tmp"
      dt[rep_these,
         (cols2rep) := dt.miss[rep_these, .SD, .SDcols = cols2rep1],]
      dt[rep_these, col.OBS := "code partially found", ]
    }

    # List of missing collection codes and instituions
    miss.coll <- unique(dt[is.na(index.herbariorum.or.working.code),
                           .SD,
                           .SDcols = c("collection.string",
                                       "cod.coll.tmp",
                                       "cod.inst.tmp")])
    names(miss.coll) <- c("string", "collectionCode",
                          "institutionCode")

    if (print.miss)
      cat("The following collections were not found:\n",
          knitr::kable(as.data.frame(miss.coll[,c(2,3)])),"",
          sep = "\n")

  }


  # Replacing collection codes not found by the original ones
  dt[is.na(index.herbariorum.or.working.code),
     col.OBS := "code not found"]
  dt[is.na(index.herbariorum.or.working.code),
     index.herbariorum.or.working.code := cod.coll.tmp]

  dt1 <- data.table::as.data.table(
          data.table::merge.data.table(
            x[, "temp..order", drop = FALSE], dt,
            by = "temp..order", all.x = TRUE, suffixes = c(".x", "")))
  dt1[is.na(ordem..dados), col.OBS := "cannot check"]

  #Making sure data is in the good order ad removing unecessary columns
  data.table::setorder(dt1, "temp..order")
  dt1[ , c("temp..order", "ordem..dados", "cod.inst.tmp",
           "cod.coll.tmp") := NULL]
  if (!is.null(drop)) {
    drop <- drop[drop %in% names(dt1)]
    dt1[ , c(drop) := NULL]
  }
  data.table::setnames(dt1,
                       c("index.herbariorum.or.working.code","col.OBS"),
                       c("collectionCode.new","collectionObs"))
  x[["temp..order"]] <- NULL
  drop1 <- names(dt1)[names(dt1) %in% names(x)]
  dt1[ , c(drop1) := NULL]

  dt2 <- cbind.data.frame(x, dt1)
  return(dt2)
}
