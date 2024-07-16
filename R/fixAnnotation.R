#'
#' @title Fix Taxon Name Notation
#'
#' @description Standardizes the notation of open nomenclature
#'   and modificators of taxon names (e.g. aff., cf., etc.).
#'
#' @param x a vector of characters with taxon names
#'
#' @return the vector \code{x} with the standardize taxon name
#'   notation
#'
#' @author Renato A. F. de Lima
#'
#' @details The function solve most but not all possible notation of
#'   name modificators, specially when there are spaces that separate
#'   them from the naes themselves are missing (see examples).
#'
#' @examples
#' \dontrun{
#' taxa <- c("Lindsaea lancea var angulata", "Lindsaea Aff.lancea",
#' "Blechnum cf.spannagelii", "Blechnumcf.spannagelii")
#' fixAnnotation(taxa)
#' }
#'
#' @keywords internal
#'
fixAnnotation <- function(x) {

  x <- squish(x)

  x <- gsub("var\\.", "var. ", x, perl = TRUE)
  x <- gsub("subsp\\.", "subsp. ", x, perl = TRUE)
  x <- gsub("ssp\\.", "subsp. ", x, perl = TRUE)
  x <- gsub("aff\\.", "aff. ", x, perl = TRUE, ignore.case = TRUE)
  x <- gsub("cf\\.", "cf. ", x, perl = TRUE, ignore.case = TRUE)
  x <- gsub(" f\\.", " f. ", x, perl = TRUE)
  x <- gsub(" f\\. \\)", " f.)", x, perl = TRUE)
  x <- gsub(" var ", " var. ", x, fixed = TRUE)
  x <- gsub(" subsp ", " subsp. ", x, fixed = TRUE)
  x <- gsub(" ssp ", " subsp. ", x, fixed = TRUE)
  x <- gsub(" aff ", " aff. ", x, fixed = TRUE)
  x <- gsub(" Aff. ", " aff. ", x, fixed = TRUE)
  x <- gsub(" cf ", " cf. ", x, fixed = TRUE)
  x <- gsub(" Cf. ", " cf. ", x, fixed = TRUE)
  x <- gsub(" form ", " form. ", x, fixed = TRUE)
  x <- gsub(" f ", " f. ", x, fixed = TRUE)
  x <- gsub("( \u00d7)(?=[[:alpha:]])","\\1 \\2", x, perl = TRUE)

  # Codes from SynTreeSys (check overlap)
  # x1 <- gsub("^cf ", "cf. ", x, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub("^cf, ", "cf., ", x1, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub(" cf ", " cf. ", x1, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub(" cf$", " cf.", x1, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub("(^cf\\.)([a-z])", "\\1 \\2", x1, perl = TRUE)
  # x1 <- gsub("( cf\\.)([a-z])", "\\1 \\2", x1, perl = TRUE)
  # x1 <- gsub("(cf\\. )([A-Z])", "\\1\\L\\2", x1, perl = TRUE)
  # x1 <- gsub("(cf\\.)([A-Z])", "\\1 \\L\\2", x1, perl = TRUE)
  #
  # x1 <- gsub("^cf$", "cf.", x1, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub("^Cf\\.$", "cf.", x1, perl = TRUE)
  # x1 <- gsub(" Cf\\. ", " cf. ", x1, perl = TRUE)
  # x1 <- gsub("^cf\\. ", "cf. ", x1, perl = TRUE, ignore.case = TRUE)
  #
  # x1 <- gsub("^aff ", "aff. ", x, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub("^aff, ", "aff., ", x1, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub(" aff ", " aff. ", x1, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub(" af\\. ", " aff. ", x1, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub(" aff$", " aff.", x1, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub("(^aff\\.)([a-z])", "\\1 \\2", x1, perl = TRUE)
  # x1 <- gsub("( aff\\.)([a-z])", "\\1 \\2", x1, perl = TRUE)
  # x1 <- gsub("(aff\\. )([A-Z])", "\\1\\L\\2", x1, perl = TRUE)
  # x1 <- gsub("(aff\\.)([A-Z])", "\\1 \\L\\2", x1, perl = TRUE)
  #
  # x1 <- gsub("^aff$", "aff.", x1, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub("^Aff\\.$", "aff.", x1, perl = TRUE)
  # x1 <- gsub(" Aff\\. ", " aff. ", x1, perl = TRUE)
  # x1 <- gsub("^aff\\. ", "aff. ", x1, perl = TRUE, ignore.case = TRUE)
  #
  # x1 <- gsub("^var ", "var. ", x, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub(" var ", " var. ", x1, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub(" var$", " var.", x1, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub("(^var\\.)([a-z])", "\\1 \\2", x1, perl = TRUE)
  # x1 <- gsub("( var\\.)([a-z])", "\\1 \\2", x1, perl = TRUE)
  # x1 <- gsub("([a-z])(var\\.)([a-z])", "\\1 \\2 \\3", x1, perl = TRUE)
  # x1 <- gsub("(var\\. )([A-Z])", "\\1\\L\\2", x1, perl = TRUE)
  # x1 <- gsub("^var$", "var.", x1, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub("^Var\\.$", "var.", x1, perl = TRUE)
  # x1 <- gsub(" Var\\. ", " var. ", x1, perl = TRUE)
  # x1 <- gsub("([a-z])(_var\\.)([a-z])", "\\1 var. \\3", x1, perl = TRUE)
  #
  # x1 <- gsub("^subsp |^spp |^ssp ", "subsp. ", x, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub("^spp\\. |^ssp\\. ", "subsp. ", x, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub(" subsp | spp | ssp ", " subsp. ", x1, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub(" subsp$| spp$| ssp$", " subsp.", x1, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub(" spp\\. ", " subsp. ", x1, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub(" ssp\\. ", " subsp. ", x1, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub(" sub\\. ", " subsp. ", x1, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub("^spp\\. ", "subsp. ", x1, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub("^ssp\\.", "subsp.", x1, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub("(^subsp\\.)([a-z])", "\\1 \\2", x1, perl = TRUE)
  # x1 <- gsub("( subsp\\.)([a-z])", "\\1 \\2", x1, perl = TRUE)
  # x1 <- gsub("([a-z])(subsp\\.)([a-z])", "\\1 \\2 \\3", x1, perl = TRUE)
  # x1 <- gsub("^subsp$|^spp$", "subsp.", x1, perl = TRUE, ignore.case = TRUE)
  # x1 <- gsub("^Subsp\\.$", "subsp.", x1, perl = TRUE)
  # x1 <- gsub(" Subsp\\. ", " subsp. ", x1, perl = TRUE)
  # x1 <- gsub("(subsp\\. )([A-Z])", "\\1\\L\\2", x1, perl = TRUE)
  # x1 <- gsub("(subsp\\.)([A-Z])", "\\1\\ \\L\\2", x1, perl = TRUE)
  # x1 <- gsub("^subspd", "subsp. d", x1, perl = TRUE)
  # x1 <- gsub("^subspp", "subsp. p", x1, perl = TRUE)
  # x1 <- gsub("^subsps", "subsp. s", x1, perl = TRUE)
  # x1 <- gsub("^sspr", "subsp. r", x1, perl = TRUE)
  # x1 <- gsub("([a-z])(_subsp\\.)([a-z])", "\\1 subsp. \\3", x1, perl = TRUE)

  return(squish(x))
}
