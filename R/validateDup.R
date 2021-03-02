#' @title Prepare, Search and Merge Duplicate Specimens
#'
#' @description This function search for duplicated specimens within and across
#'   collections and it can be used to homogenize the information of
#'   different groups of fields and to remove duplicates, leaving only one
#'   occurrence for each group of duplicata.
#'
#' @return The input data frame, plus the new columns with the formatted
#'   fields.
#'
#' @param occ.df a data frame, containing typical fields from occurrence records from
#'   herbarium specimens
#' @param cat.code character. The name of the column containing the code of the
#'   collection. Default to the __plantR__ output column "collectionCode.new".
#' @param cat.numb character. The name of the column containing the catalog
#'   number (a.k.a. accession number) of the record. Default to "catalogNumber".
#' @param merge logical. Should duplicates be merged? Default to TRUE.
#' @param remove logical. Should all duplicates be removed or only the
#'   duplicated entries from the same collection? Default to FALSE.
#'
#' @inheritParams prepDup
#' @inheritParams mergeDup
#'
#' @details The function works similarly to a wrapper function, where the
#'   individuals steps of the proposed __plantR__ workflow for preparing,
#'   searching, merging and removal of duplicates are performed altogether (see
#'   the __plantR__ tutorial for details).
#'
#' @seealso
#'  \link[plantR]{prepDup}, \link[plantR]{getDup}, \link[plantR]{mergeDup},
#'  \link[plantR]{rmDup}
#'
#' @author Renato A. F. de Lima
#'
#' @export validateDup
#'
validateDup <- function(occ.df,
                        cat.code = "collectionCode.new",
                        cat.numb = "catalogNumber",
                        merge = TRUE,
                        remove = FALSE,
                        ...) {

  # check input:
  if (!class(occ.df) == "data.frame")
    stop("input object needs to be a data frame!")

  # getTombo
  occ.df$numTombo <- getTombo(occ.df[, cat.code],
                              occ.df[, cat.numb])

  # prepDup
  # dups <- prepDup(occ.df, noYear, noName, noNumb, comb.fields, ignore.miss, ignore.na)
  dups <- prepDup(occ.df, ...)

  # getDup
  dups <- getDup(dups)
  occ.df <- cbind.data.frame(occ.df,
                             dups[, c("dup.ID", "dup.numb", "dup.prop")],
                             stringsAsFactors = FALSE)
  # mergeDup
  if (merge) {
    occ.df1 <- mergeDup(occ.df, ...)
  }

  # rmDup
    occ.df2 <- rmDup(occ.df1, rm.all = remove)

  return(occ.df2)
}
