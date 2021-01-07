#' @title Prepare, Search and Merge Duplicate Specimens
#'
#' @description This function search for duplicated specimens within and across
#'   collections and it can be used to homogenize the information of
#'   different groups of fields and to remove duplicates, leaving only one
#'   occurrence for each group of duplicata.
#'
#' @return The input data frame \code{x}, plus the new columns with the formatted
#'   fields.
#'
#' @param occ.df a data frame, containing typical fields from occurrence records from
#'   herbarium specimens
#' @param merge logical. Should duplicates be merged? Default to TRUE.
#' @param remove logical. Should duplicates be removed? Default to FALSE.
#' @param ... Parameters from mergeDup
#'
#' @inherit prepDup params
#' @inherit getDup params
#' @inherit mergeDup params
#' @inherit rmDup params
#'
#' @details The function works similarly to a wrapper function, where the
#'   individuals steps of the proposed __plantR__ workflow for preparing,
#'   searching, merging and removal of duplicates are performed altogether (see
#'   the __plantR__ tutorial for details).
#'
#' @seealso
#'  \link[plantR]{prepDup}, \link[plantR]{getDup}, \link[plantR]{mergeDup},
#'  and \link[plantR]{rmDup}.
#'
#' @author Renato A. F. de Lima
#'
#' @export validateDup
#'
validateDup <- function(occ.df, merge = TRUE, remove = FALSE, ...) {

  # check input:
  if (!class(occ.df) == "data.frame")
    stop("input object needs to be a data frame!")

  # prepDup
  dups <- prepDup(occ.df, noYear, noName, noNumb, comb.fields, ignore.miss, ignore.na)

  # getDup
  dups <- getDup(dups)
  occ.df <- cbind.data.frame(occ.df,
                           dups[,c("numTombo","dup.ID","dup.numb","dup.prop")],
                           stringsAsFactors = FALSE)
  # mergeDup
  if (merge) {
    occ.df <- mergeDup(occ.df, ...)
  }

  # rmDup
  if (remove) {
    antes <- dim(occ.df)[1]
    occ.df <- rmDup(occ.df)
    depois <- dim(occ.df)[1]
    cat(antes - depois,"duplicated records were from the data.")
  }

  return(occ.df)
}
