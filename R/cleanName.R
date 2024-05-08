#'
#' @title Remove ponctuation, special characters, numbers and spaces
#'
#' @description Simple function to prepare vectors of taxon names for
#'   increasing the chances of exact and fuzzy name matching
#'
#' @param x a vector of characters (i.e. names)
#'
#' @return the vector \code{x} without ponctuation, special
#'   characters, numbers and spaces
#'
#' @author Renato A. F. de Lima
#'
#' @examples
#'
#' \dontrun{
#' taxa <- c("Genus \u00d7 epiteth (Jos\u00e9 AB.) C.")
#' print(taxa)
#' cleanName(taxa)
#' }
#'
#' @keywords internal
#'
#' @importFrom tm removePunctuation
#'
cleanName <- function(x) {

  x1 <- tolower(gsub(" ", "", x, fixed =  TRUE))
  x1 <- tm::removePunctuation(x1)
  x1 <- rmLatin(x1)
  x1 <- gsub("\u00d7", "", x1, fixed = TRUE)
  x1 <- gsub("[0-9]", "", x1, perl = TRUE)

  return(x1)
}
