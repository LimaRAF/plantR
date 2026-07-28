#' @title Remove Duplicates
#'
#' @description This function keeps one and removes all other
#'   specimens within groups of duplicate specimens.
#'
#' @param df the input data frame.
#' @param dup.name character. The name of column in the input data
#'   frame with the duplicate group ID. Default to the __plantR__
#'   output 'dup.ID'.
#' @param prop.name character. The name of column in the input data
#'   frame with the proportion of duplicates found within the group
#'   ID. Default to the __plantR__ output 'dup.prop'.
#' @param rec.ID character. The name of the columns containing the
#'   unique record identifier (see function `getTombo()`). Default to
#'   'numTombo'.
#' @param prop numerical. The threshold value of proportion of
#'   duplicated values retrieved (i.e. dup.prop) to enter the merging
#'   routine. Should be between zero and one. Default to 0.75.
#' @param order.by character. Column name(s) used to order records
#'   within groups of duplicates.
#' @param rm.all logical. Should all duplicates be removed or only the
#'   duplicated entries from the same collection but different
#'   sources? Default to FALSE.
#' @param  print.rm logical. Should the number of records removed be
#'   printed? Default to TRUE.
#'
#' @author Renato A. F. de Lima
#'
#' @details The input data frame \code{df} must contain the typical
#'   columns resulting from __plantR__ workflow and functions.
#'   Otherwise, the names of these columns should be provided using
#'   arguments `dup.name` (i.e. characters used to aggregate records
#'   into groups of duplicates) and `prop.name` (i.e. proportion of
#'   duplicated records).
#'
#'   Since only one record is kept per group of duplicates, this
#'   procedure should preferably be carried after the homogenization
#'   of the specimens informations (see function `mergeDup()`).
#'   Otherwise, important information on the removed records may be
#'   lost.
#'
#'   In addition, since not all columns are merged within duplicates
#'   (only the columns related to the taxonomic, geographical and
#'   location validation procedures), all other information contained
#'   in the removed records are lost. Therefore, make sure that this
#'   information in unnecessary for your specific purposes before
#'   using this function.
#'
#'   By default, the record retained for each group of duplicates is
#'   determined by the proportion of duplicates the record has within
#'   the group (argument `prop.name`) and by the original order of the
#'   input data frame \code{df}. So, the first record with the highest
#'   proportion of duplicates will be the record retained. But the
#'   user can use the argument `order.by` if the data should be order
#'   by any of the columns in the input data. This column will be used
#'   to create the 'key' within the `data.table` parlance and order
#'   the data accordingly.
#'
#'   Finally, users can choose between removing all but one record
#'   within each group of duplicated records, or to remove only those
#'   records with duplicated entries from the same collection in
#'   different sources (i.e. virtual duplicates), using the argument
#'   `rm.all`. This option can be useful if the same collection has
#'   its database in two or more repositories (e.g. speciesLink and
#'   GBIF). It is important to note that this removal is dependent on
#'   the duplicate group ID found for each record. So, if the
#'   information was entered differently in the different sources, it
#'   is not guaranteed that they will be grouped under the same
#'   duplicate group ID, and thus be excluded from the data.
#'
#' @import data.table
#'
#' @examples
#' (df <- data.frame(numTombo = c("a1","b2","c3","c3","d5","d5","e7","f4","g9"),
#'                   dup.ID = c("a1|b2","a1|b2","c3|c3","c3|c3","d5|d5|e7",
#'                              "d5|d5|e7","d5|d5|e7","f4",NA),
#'                   dup.prop = c(1, 1, 1, 1, 0.5, 0.5, 1, 1, NA)))
#' rmDup(df)
#' rmDup(df, rm.all = TRUE)
#' rmDup(df, rm.all = TRUE, print.rm = FALSE)
#'
#' @export rmDup
#'
rmDup <- function(df,
                  dup.name = "dup.ID",
                  prop.name = "dup.prop",
                  rec.ID = "numTombo",
                  prop = 0.75,
                  order.by = NULL,
                  rm.all = FALSE,
                  print.rm = TRUE) {

  ## check input
  if (!inherits(df, "data.frame"))
    stop ("Input object needs to be a data frame!")

  if (dim(df)[1] == 0)
    stop("Input data frame is empty!")

  #Escaping R CMD check notes from using data.table syntax
  dup.IDs <- tmp.ordem <- temp.dup.prop <- dup.entries <- NULL
  dup.merge <- temp.rec.ID <- rename.IDs <- NULL

  #Checking essential columns
  if (!dup.name %in% names(df))
    stop(paste0("The input data frame does not contains a column with the duplicate IDs"))

  if (all(is.na(df[[dup.name]]))) {
    print("No duplicate IDs available. Returning the same input data frame",
            call. = FALSE)
    return(df)
  }

  # creating the unique accession number and multiple-acession number for each specimen
  dt <- data.table::data.table(df)
  dt[ , dup.IDs := .SD, .SDcols = c(dup.name)]
  dt[dup.IDs %in% c("", " "), dup.IDs := NA_character_, ]

  # Vector to keep the original data order
  dt[, tmp.ordem := .I, ]

  # Creating the data.table key
  data.table::setkey(dt, dup.IDs)

  #Checking the non-essential columns
  if (!prop.name %in% names(dt)) {
    warning("The input data has no column with the proportion of duplicates. Assuming to be 1",
            call. = FALSE)
    dt[, temp.dup.prop := 1]
  } else {
    dt[ , temp.dup.prop := .SD, .SDcols = c(prop.name)]
  }

  if (!rec.ID %in% names(dt)) {
    if (rm.all) {
      warning("No unique record ID provided. Creating one",
              call. = FALSE)
      dt[, c(rec.ID) := tmp.ordem]
    } else {
      stop("Removal of duplicated entries from the same collection needs a column with the unique record ID")
    }
  }

  dt <- getMergeCat(dt, dup.name = dup.name, prop.name = prop.name,
                    prop = prop, rec.ID = rec.ID)
  dt[dup.merge == TRUE & temp.dup.prop %in% "cc", dup.merge := FALSE]
  dt[dup.merge == TRUE & is.na(dup.IDs), dup.merge := FALSE]
  if (any(!dt$dup.merge, na.rm = T))
    dt[dup.merge == FALSE, dup.IDs := .SD, .SDcols = c(rec.ID)]

  if (any(dt$dup.merge, na.rm = T)) {
    data.table::setkeyv(dt, rec.ID)
    dt[dup.merge == TRUE, dup.IDs := lapply(unique(.SD), paste0, collapse = "|"),
       .SDcols = c(rec.ID), by = dup.IDs]
    data.table::setkey(dt, dup.IDs)
  }
  dt[, dup.merge := NULL]

  # Making sure all records have a non duplicated dup.ID and non-missing duplicate proportion
  dt[is.na(dup.IDs), temp.dup.prop := 1]
  dt[is.na(dup.IDs), dup.IDs := .SD, .SDcols = c(rec.ID)]
  dt[dup.IDs %in% c("", " ", NA) , dup.IDs := paste("NA", .SD, sep = "_"), by = tmp.ordem,
     .SDcols = c("tmp.ordem")]

  if (is.null(order.by)) {
    # re-ordering the data.table, using setorder()
    data.table::setkeyv(dt, c("dup.IDs", "temp.dup.prop", "tmp.ordem"))
    data.table::setorderv(dt,  cols = c("dup.IDs", "temp.dup.prop", "tmp.ordem"),
                          order = c(1, -1, 1))
  } else {
    # re-ordering the data.table, using setorder()
    data.table::setorderv(dt,  c("dup.IDs", order.by))
  }

  #removing the duplicates and putting it back into the original order
  if (rm.all) {

    dt1 <- unique(dt, by = "dup.IDs")

  } else {
    dt[, temp.rec.ID := .SD, .SDcols = c(rec.ID)]
    dt[temp.rec.ID %in% c("", " ", NA), temp.rec.ID := NA_character_]
    dt[, dup.entries := duplicated(.SD),
       by = dup.IDs, .SDcols = c("temp.rec.ID")]

    if (any(is.na(dt$temp.rec.ID)))
      dt[is.na(temp.rec.ID), dup.entries := FALSE]

    if (any(!is.na(dt$temp.rec.ID) & duplicated(dt$temp.rec.ID)))
      dt[!is.na(temp.rec.ID) & duplicated(temp.rec.ID),
         dup.entries := TRUE]

    dt[, rename.IDs := any(dup.entries), by = dup.IDs]
    dt[rename.IDs == TRUE,
       dup.IDs := as.character(paste0(sort(unique(temp.rec.ID)),
                                      collapse = "|")),
       by = dup.IDs]
    dt1 <- dt[dup.entries == FALSE, ]
    dt1[, c("dup.entries", "rename.IDs", "temp.rec.ID") := NULL, ]
  }
  data.table::setorder(dt1, tmp.ordem)
  #removing the extra column created for ranking
  dt1[, c("dup.IDs", "temp.dup.prop", "tmp.ordem") := NULL]

  if (print.rm) {

    antes <- dim(dt)[1]
    depois <- dim(dt1)[1]
    if (rm.all) {
      cat(antes - depois, "true or probable duplicate records were removed from the data\n")
    } else {
      cat(antes - depois, "true duplicate records (same record in different sources) were removed from the data\n")
    }
  }

  return(data.frame(dt1))
}
