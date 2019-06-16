#' @title Capitalize Name
#'
#' @description Capitalize the name of the person that collected or identified a biological specimen.
#'
#' @param x a character string to be capitalized.
#'
#' @return a character string equal to \code{x} with the first letter capitalized.
#'
#' @details The function works for simple names and coumpound names which are separated by
#' a space or by thecharacter '-'.
#'
#' @export
#'
#' @examples
#' # Simple name
#'   capName("gentry")
#' # Names with generational suffixes
#'   capName("leitao filho")
#' # Compound name
#'   capName("saint-hilaire")
capName <- function(x) {
  # check input:
  if (length(x)>1) {
    stop("'x' cannot be a vector of strings!")
  }

  # identifying compound names:
  if(grepl('-',x)) {
      split <- strsplit(x, "-")[[1]]
      nome = paste(toupper(substring(split, 1, 1)), tolower(substring(split, 2)), sep = "", collapse = "-")
    } else {
      split <- strsplit(x, " ")[[1]]
      nome = paste(toupper(substring(split, 1, 1)), tolower(substring(split, 2)), sep = "", collapse = " ")
    }
  return(nome)
}
