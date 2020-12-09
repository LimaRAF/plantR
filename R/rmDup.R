#' @title Remove Duplicates
#'
#' @description This function keeps one and removes all other specimens within
#'   groups of duplicate specimens.
#'
#' @param df the input data frame.
#' @param order.by character. Column name(s) used to order specimens within
#'   groups of duplicates.
#'
#' @author Renato A. F. de Lima
#'
#' @details The input data frame \code{df} must contain the typicall columns
#'   resulting from __plantR__ workflow and functions. The characters that
#'   aggregate the occurrences into duplicates must be named 'dup.ID'.
#'
#'   Since only one specimen is kept per group of duplicates, this procedure
#'   should be carried after the homogenization of the specimens informations
#'   (see function `mergeDup`). Otherwise, important information may be lost.
#'
#'   In addition, since not all columns are merged within duplicates (only the
#'   columns related to the taxonomic, geographical and location validation
#'   procedures), all other information contained in the removed specimens are
#'   lost. Therefore, make sure that this information in unnecessary for your
#'   specific purposes before using this function.
#'
#'   By default, the specimen retained for each group of duplicate follow the
#'   original order of the input data frame \code{df}. But the user can use the
#'   argument `order.by` if the data should be order by any of the columns of
#'   \code{df}. This column will be used to create the 'key' within the
#'   `data.table` parlance.
#'
#' @import data.table
#'
#' @export rmDup
#'
rmDup <- function(df, order.by = NULL, ...) {

  ## check input
  if (!class(df) == "data.frame")
    stop("Input object needs to be a data frame!")

  #Escaping R CMD check notes from using data.table syntax
  dup.ID <- dup.ID1 <- dup.prop <- numTombo <- ordem <- NULL

  #Checking
  if(!"dup.ID" %in% names(df))
    stop(paste0("Removal is only possible if the input data frame contain a columns named 'dup.ID'"))

  # transforming the data frame into a data.table
  df$dup.ID <- as.factor(df$dup.ID)
  dt <- data.table::data.table(df)

  # creating the unique accession number and multiple-acession number for each specimen
  dt[, dup.ID1 := dup.ID]
  dt[is.na(dup.ID1), dup.ID1 := numTombo]

  # Vector to keep the original data order
  dt[,ordem := 1:dim(dt)[1],]

  if (is.null(order.by)) {
    # re-ordering the data.table, using setorder()
    data.table::setorder(dt,  dup.ID, -dup.prop, ordem)
  } else {
    # re-ordering the data.table, using setorder()
    data.table::setorderv(dt,  c("dup.ID", order.by))
  }

  #removing the duplicates and putting it back into the orinal order
  dt1 <- unique(dt, by = "dup.ID1")
  data.table::setorder(dt1, ordem)

  #removing the extra column created for ranking
  dt1[, c("dup.ID1","ordem") := NULL]

  return(data.frame(dt1))
}
