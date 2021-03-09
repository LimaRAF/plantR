#' @title Remove Duplicates
#'
#' @description This function keeps one and removes all other specimens within
#'   groups of duplicate specimens.
#'
#' @param df the input data frame.
#' @param dup.name character. The name of column in the input data frame with
#'   the duplicate group ID. Default to the __plantR__ output 'dup.ID'.
#' @param prop.name character. The name of column in the input data frame with
#'   the proportion of duplicates found within the group ID. Default to the
#'   __plantR__ output 'dup.prop'.
#' @param rec.ID character. The name of the columns containing the unique record
#' identifier (see function `getTombo()`). Default to 'numTombo'.
#' @param order.by character. Column name(s) used to order records within
#'   groups of duplicates.
#' @param rm.all logical. Should all duplicates be removed or only the
#'   duplicated entries from the same collection but different sources?
#'   Default to FALSE.
#' @param  print.rm logical. Should the number of records removed be printed?
#'   Default to TRUE.
#'
#' @author Renato A. F. de Lima
#'
#' @details The input data frame \code{df} must contain the typicall columns
#'   resulting from __plantR__ workflow and functions. Otherwise, the names
#'   of these columns should be provided using arguments `dup.name` (i.e.
#'   characters used to aggregate records into groups of duplicates) and
#'   `prop.name` (i.e. proportion of duplicated records).
#'
#'   Since only one record is kept per group of duplicates, this procedure
#'   should preferably be carried after the homogenization of the specimens
#'   informations (see function `mergeDup()`). Otherwise, important information
#'   on the removed records may be lost.
#'
#'   In addition, since not all columns are merged within duplicates (only the
#'   columns related to the taxonomic, geographical and location validation
#'   procedures), all other information contained in the removed records are
#'   lost. Therefore, make sure that this information in unnecessary for your
#'   specific purposes before using this function.
#'
#'   By default, the record retained for each group of duplicates is determined
#'   by the proportion of duplicates the record has within the group (argument
#'   `prop.name`) and by the original order of the input data frame \code{df}. So,
#'   the first record with the highest proportion of duplicates will be the record
#'   retained. But the user can use the argument `order.by` if the data should
#'   be order by any of the columns in the input data. This column will be used to
#'   create the 'key' within the `data.table` parlance and order the data accordingly.
#'
#'   Finally, users can choose between removing all but one records within each
#'   group of duplicate, or to remove only those records with duplicated entries
#'   from the same collection in different sources using the argument `rm.all`.
#'   This option can be useful if the same collection has its database in two or
#'   more repositories (e.g. speciesLink and GBIF). It is important to note that
#'   this removal is dependent on the duplicate group ID found for each record.
#'   So, if the information was entered differently in the different sources, it
#'   is not guaranteed that they will be grouped under the same duplicate group
#'   ID, and thus be excluded from the data.
#'
#' @import data.table
#'
#' @examples
#' (df <- data.frame(numTombo = c("a1", "b2", "c3", "c3", "d5", "d5", "e7", "f4", "g9"),
#'                   dup.ID = c("a1|b2", "a1|b2", "c3|c3", "c3|c3", "d5|d5|e7",
#'                              "d5|d5|e7", "d5|d5|e7", "f4", NA),
#'                   dup.prop = c(1, 1, 1, 1, 0.5, 0.5, 1, 1, NA),
#'                   stringsAsFactors = FALSE))
#' rmDup(df)
#' rmDup(df, rm.all = TRUE)
#' rmDup(df, rm.all = TRUE, print.rm = FALSE)
#'
#' @export rmDup
#'
rmDup <- function(df, dup.name = "dup.ID", prop.name = "dup.prop",
                  rec.ID = "numTombo", order.by = NULL, rm.all = FALSE,
                  print.rm = TRUE) {

  ## check input
  if (!class(df) == "data.frame")
    stop ("Input object needs to be a data frame!")

  #Escaping R CMD check notes from using data.table syntax
  dup.IDs <- tmp.ordem <- temp.dup.prop <- dup.entries <- NULL
  temp.rec.ID <- rename.IDs <- NULL

  #Checking essential columns
  if (!dup.name %in% names(df))
    stop(paste0("Removal is only possible if the input data contain a column with the duplicate IDs"))

  # creating the unique accession number and multiple-acession number for each specimen
  dt <- data.table::data.table(df)
  dt[ , dup.IDs := .SD, .SDcols = c(dup.name)]

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
      stop("Removal of duplicated entries from the same collection needs a column unique record ID provided")
    }
  }

  # Making sure all records have a non duplicated dup.ID and non-missing duplicate proportion
  dt[is.na(dup.IDs), temp.dup.prop := 1]
  dt[is.na(dup.IDs), dup.IDs := .SD, .SDcols = c(rec.ID)]

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
    data.table::setorder(dt1, tmp.ordem)
  } else {
    data.table::setnames(dt, rec.ID, "temp.rec.ID")
    dt[, dup.entries := duplicated(.SD),
       by = dup.IDs, .SDcols = c("temp.rec.ID")]
    dt[is.na(temp.rec.ID), dup.entries := FALSE]
    dt[, rename.IDs := any(dup.entries), by = dup.IDs]
    dt[rename.IDs == TRUE,
       dup.IDs := as.character(paste0(sort(unique(temp.rec.ID)), collapse = "|")),
       by = dup.IDs]
    dt1 <- dt[dup.entries == FALSE, ]
    dt1[, c("dup.entries", "rename.IDs") := NULL, ]
    data.table::setnames(dt1, "temp.rec.ID", rec.ID)
    data.table::setorder(dt1, tmp.ordem)
  }
  #removing the extra column created for ranking
  dt1[, c("dup.IDs", "temp.dup.prop", "tmp.ordem") := NULL]

  if (print.rm) {
    antes <- dim(dt)[1]
    depois <- dim(dt1)[1]
    cat(antes - depois, "truly duplicated records (same collection in different sources) were removed from the data\n")
  }

  return(data.frame(dt1))
}
