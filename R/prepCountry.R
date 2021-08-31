#' @title Format Country Name
#'
#' @description Simple function to standardize the notation of country name
#'   (administrative level 0) by converting country codes to their long names
#'   and by removing special characters and some country name prepositions and
#'   separators.
#'
#' @param x a vector of country names to be standardized
#' @param to.lower logical. Should the output names be return in lower cases?
#'   Default to TRUE.
#' @param special.char logical. Should special characters be maintained? Default
#'   to FALSE.
#' @param rm.abbrev logical. Should common name abbreviation be replaced?
#'   Default to TRUE.
#'
#' @return The input vector \code{x} in the standard name notation (see Details)
#'
#' @details Country information is formatted into a standard notation, i.e. long
#'   name format (in English). By default, all letters are lower-cased (argument
#'   `to.lower`) and special characters (argument `special.char`) and common
#'   abbreviations (e.g. 'st.') are removed (argument `rm.abbrev`). These edits
#'   aim at reducing possible variation in country name notation and facilitate
#'   further data processing and comparison within the __plantR__ workflow.
#'
#'   All country information with less than four letters are treated as country
#'   codes and they are converted to the long format. Currently, only the ISO
#'   3166-1 aplha-2 and alpha-3 codes are considered for convertion to the long
#'   country name format.
#'
#' @author Renato A. F. de Lima
#'
#' @importFrom countrycode countrycode
#'
#' @keywords internal
#'
#' @examples
#' # Creating a data frame with locality information
#' paises <- c("VC", "VCT", "St. Vincent and the Grenadines",
#' "St. Vincent & Grenadines", "Saint-Martin", "Falkland Is.", NA)
#'
#' # Formating the locality information
#' prepCountry(paises)
#' prepCountry(paises, to.lower = FALSE)
#' prepCountry(paises, rm.abbrev = FALSE)
#'
#' @export
prepCountry <- function(x,
                       to.lower = TRUE,
                       special.char = FALSE,
                       rm.abbrev = TRUE)
  {

  # Identifying NAs
  x1 <- x[!x %in% c("", " ", NA)]

  # Converting any country codes into country long names
  n.letters <- nchar(x1)
  if (any(n.letters < 4)) {
    x1[n.letters == 2 & !is.na(x1)] <-
      countrycode::countrycode(as.character(x1[n.letters == 2 & !is.na(x1)]),
                               'iso2c', 'country.name')
    x1[n.letters == 3 & !is.na(x1)] <-
      countrycode::countrycode(as.character(x1[n.letters == 3 & !is.na(x1)]),
                               'iso3c', 'country.name')
  }

  # Replacing '&' by 'and' in compound country names
  x1 <- gsub(" & ", " and ", x1, fixed = TRUE)

  # Replacing abbreviated names
  if (rm.abbrev) {
    x1 <- gsub("^St\\. ", "Saint ", x1, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub(" I\\.", " Island", x1, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub(" Is\\.", " Islands", x1, perl = TRUE, ignore.case = TRUE)
  }

  # Replacing abbreviated country descriptions
  x1 <- gsub("Dem\\. Rep\\.", "Democratic Republic", x1, perl = TRUE,
            ignore.case = TRUE)

  # Removing some separators and prepositions from country names
  x1 <- gsub("-", " ", x1, fixed = TRUE)
  x1 <- gsub("\\s+", " ", x1, perl = TRUE)
  x1 <- gsub(" of the ", " ", x1, fixed = TRUE)
  x1 <- gsub(" and the ", " ", x1, fixed = TRUE)
  x1 <- gsub(" of ", " ", x1, fixed = TRUE)
  x1 <- gsub(" and ", " ", x1, fixed = TRUE)

  # Removing unwanted characters
  if (!special.char)
    x1 <- rmLatin(x1)

  # Lower-casing
  if (to.lower)
    x1 <- tolower(x1)

  # Writing the results
  x[!x %in% c("", " ", NA)] <- x1
  return(x)
}
