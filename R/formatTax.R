#' @title Format Taxonomic Information
#'
#' @description This function edit and standardize the names of plant
#'   species and families.
#'
#' @return The input data frame \code{tax}, plus the new columns with the
#'   formatted information. The new columns have the same name of the
#'   Darwin Core standards, but followed by the suffix '.new'.
#'
#' @param tax a data.frame containing the taxonomic information to be edited.
#'
#' @inheritParams prepSpecies
#' @inheritParams formatSpecies
#' @inheritParams prepFamily
#'
#' @details The function works as a wrapper, where the individuals steps of the
#'   proposed __plantR__ workflow for editing taxonomic information are
#'   performed altogether (see the __plantR__ tutorial and the help of each
#'   function for details).
#'
#'   The input data frame usually contains the following taxonomic fields:
#'   "family", "genus", "scientificName" and "scientificNameAuthorship". But
#'   users can define the names for their own data.
#'
#'
#' @seealso
#'  \link[plantR]{prepSpecies}, \link[plantR]{formatSpecies} and
#'  \link[plantR]{prepFamily}.
#'
#' @export formatTax
#'
formatTax <- function(tax, use.suggestion = TRUE, ...) {

  # check input:
  if (!class(tax) == "data.frame")
    stop("input object needs to be a data frame!")

  # prepSpecies
  tax1 <- prepSpecies(tax, ...)

  # formatSpecies
  tax1 <- formatSpecies(tax1, ...)
  if (use.suggestion) {
    tax1$scientificName.new <- tax1$suggestedName
  }

  # prepFamily
  tax1 <- prepFamily(tax1, ...)

  return(tax1)
}
