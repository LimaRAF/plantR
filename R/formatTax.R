#' @title Format Taxonomic Information
#'
#' @description This function edits and standardizes the names of plant
#'   species and families
#'
#' @return The input data frame \code{tax}, plus the new columns with the
#'   formatted information. The new columns have the same name of the
#'   Darwin Core standards, but followed by the suffix '.new'.
#'
#' @param tax a data.frame containing the taxonomic information to be edited.
#' @param use.suggestion logical. Should the edited species names be used
#'   instead of the input species names? Defaults to TRUE.
#'
#' @inheritParams fixSpecies
#' @inheritParams prepSpecies
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
#'  \link[plantR]{fixSpecies}, \link[plantR]{prepSpecies} and
#'  \link[plantR]{prepFamily}.
#'
#' @export formatTax
#'
formatTax <- function(tax,
                      use.suggestion = TRUE,
                      tax.name = "scientificName",
                      rm.rank = FALSE,
                      tax.names = c("scientificName.new","scientificNameAuthorship"),
                      db = c("bfo","tpl"),
                      sug.dist = 0.9,
                      use.authors = FALSE,
                      drop.cols = c("tmp.ordem","family","verbatimSpecies","author","full_sp","authorship","id"),
                      fam.name = "family",
                      gen.name = "genus",
                      spp.name = "scientificName") {

  # check input:
  if (!class(tax) == "data.frame")
    stop("input object needs to be a data frame!")

  # prepSpecies
  tax1 <- fixSpecies(x = tax, tax.name = tax.name, rm.rank = rm.rank)

  # formatSpecies
  tax1 <- prepSpecies(x = tax1, tax.names = tax.names, db = db,
                      sug.dist = sug.dist, use.authors = use.authors,
                      drop.cols = drop.cols)
  if (use.suggestion)
    tax1$scientificName.new[!is.na(tax1$suggestedName)] <-
      tax1$suggestedName[!is.na(tax1$suggestedName)]

  # prepFamily
  tax1 <- prepFamily(x = tax1, fam.name, spp.name = "scientificName.new")

  return(tax1)
}
