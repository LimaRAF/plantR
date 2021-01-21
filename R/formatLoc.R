#' @title Format Locality Information
#'
#' @description This function standardizes names of administrative levels from
#'   species occurrences obtained from on-line databases, such as GBIF or
#'   speciesLink
#'
#' @return The input data frame \code{x}, plus the new columns with the formatted
#'   information.
#'
#' @param x a data frame, containing typical fields from occurrence records from
#'   herbarium specimens
#' @param select.cols a vector with the column names that should be added to the
#'   input data.frame. By default only the additional columns retrieved from the
#'   gazetteer are returned and the locality strings used in the researched are
#'   discarded.
#'
#' @inheritParams fixLoc
#' @inheritParams getLoc
#'
#' @details The function works as a wrapper, where the individuals steps of the
#'   proposed __plantR__ workflow for editing locality information are performed
#'   altogether (see the __plantR__ tutorial for details).
#'
#'   The input data frame usually contains the following locality fields:
#'   "country", "stateProvince", "municipality" and "locality".
#'
#' @seealso
#'  \link[plantR]{fixLoc}, \link[plantR]{strLoc}, \link[plantR]{prepLoc},
#'  and \link[plantR]{getLoc}.
#'
#' @export formatLoc
#'
formatLoc <- function(x, select.cols = c("loc", "loc.correct", "latitude.gazetteer", "longitude.gazetteer",
                                         "resolution.gazetteer"), ...)
{

  # check input:
  if (!class(x) == "data.frame")
    stop("input object needs to be a data frame!")

  # fixLoc
  x1 <- fixLoc(x, ...)

  # strLoc
  locs <- strLoc(x1)
  locs$loc.string <- prepLoc(locs$loc.string) # priority string
  if ("loc.string1" %in% names(locs))
    locs$loc.string1 <- prepLoc(locs$loc.string1) # alternative string 1
  if ("loc.string2" %in% names(locs))
    locs$loc.string2 <- prepLoc(locs$loc.string2) # alternative string 2

  # getLoc
  locs <- getLoc(locs, ...)
  colunas <- select.cols
  colunas <- colunas[colunas %in% names(locs)]
  x1 <- cbind.data.frame(x1,
                         locs[, colunas], stringsAsFactors = FALSE)
  return(x1)
}
