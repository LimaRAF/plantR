#' @title Format Taxonomic Information
#'
#' @description This function edits and standardizes taxon names and
#'   families, following a given taxonomic backbones
#'
#' @return The input data frame \code{tax}, plus the new columns with
#'   the formatted information. The new columns have the same name of
#'   the Darwin Core standards, but followed by the suffix '.new'.
#'
#' @param tax a data.frame containing the input taxonomic information
#'   to be edited.
#' @param use.suggestion logical. Should the edited taxon names and
#'   authorships be used instead of the input species names? Defaults
#'   to TRUE.
#'
#' @inheritParams fixSpecies
#' @inheritParams prepSpecies
#' @inheritParams prepFamily
#'
#' @details The function works as a wrapper, where the individuals
#'   steps of the proposed __plantR__ workflow for editing taxonomic
#'   information are performed altogether (see the __plantR__ tutorial
#'   and the help of each function for details).
#'
#'   The input data frame usually contains the following taxonomic
#'   fields: "family", "genus", "scientificName" and
#'   "scientificNameAuthorship". But users can define the names for
#'   their own data.
#'
#'   By default, the taxonomic backbone used for name validation comes
#'   from the [Flora e Funga do Brasil](https://floradobrasil.jbrj.gov.br/consulta).
#'   However, any taxonomic backbone can be used, as long as it has a
#'   specific content and format. Check the companion R package
#'   __plantRdata__ that provides other backbones already in this specific
#'   format from the [World Flora Online](https://www.worldfloraonline.org/),
#'   the [World Checklist of Vascular Plants](https://powo.science.kew.org/)
#'   and [GBIF](https://www.gbif.org/).
#'
#' @author Renato A. Ferreira de Lima
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
                      author.name = "scientificNameAuthorship",
                      rm.rank = FALSE,
                      tax.names = c("scientificName.new",
                                    "scientificNameAuthorship.new"),
                      db = "bfo",
                      sug.dist = 0.9,
                      clean.indet = TRUE,
                      use.authors = TRUE,
                      mult.matches = "all",
                      replace.names = TRUE,
                      clean.names = FALSE,
                      split.letters = FALSE,
                      parallel = FALSE,
                      cores = 2,
                      drop.cols = c("match_type", "multiple_match",
                                    "fuzzy_dist_name",
                                    "fuzzy_dist_author",
                                    "name.status", "taxon.status",
                                    "accepted.id", "accepted.name",
                                    "accepted.authorship",
                                    "accepted.taxon.rank",
                                    "accepted.taxon.status",
                                    "accepted.name.status"),
                      fam.name = "suggestedFamily",
                      gen.name = "genus",
                      spp.name = "scientificName.new",
                      kingdom = "plantae") {

  # check input:
  if (!inherits(tax, "data.frame"))
    stop("Input object needs to be a data frame!")

  if (dim(tax)[1] == 0)
    stop("Input data frame is empty!")

  # Checking the presence of reserved columns in the input dataset
  tax <- checkColNames(tax, group = "format.tax")

  # prepSpecies
  tax1 <- fixSpecies(x = tax,
                     tax.name = tax.name,
                     author.name = author.name,
                     rm.rank = rm.rank)

  # formatSpecies
  tax1 <- prepSpecies(x = tax1,
                      tax.names = tax.names,
                      db = db,
                      sug.dist = sug.dist,
                      clean.indet = clean.indet,
                      use.authors = use.authors,
                      replace.names = replace.names,
                      clean.names = clean.names,
                      split.letters = split.letters,
                      parallel = parallel,
                      cores = cores,
                      mult.matches = mult.matches,
                      drop.cols = drop.cols)

  if (use.suggestion) {
    tax1$scientificName.new[!is.na(tax1$suggestedName)] <-
      tax1$suggestedName[!is.na(tax1$suggestedName)]
    tax1$scientificNameAuthorship.new[!is.na(tax1$suggestedAuthorship)] <-
      tax1$suggestedAuthorship[!is.na(tax1$suggestedAuthorship)]
  }

  # prepFamily
  tax1 <- prepFamily(x = tax1,
                     fam.name = fam.name,
                     gen.name = gen.name,
                     spp.name = spp.name,
                     kingdom = kingdom)

  if (use.suggestion) {
    tax1$family.new[!is.na(tax1$suggestedFamily)] <-
      tax1$suggestedFamily[!is.na(tax1$suggestedFamily)]
  }

  return(tax1)
}
