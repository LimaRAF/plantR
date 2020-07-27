#' @title Capitalize Name
#'
#' @description Capitalize the name of the collector or determiner of a biological specimen.
#'
#' @param x the character string to be capitalized.
#'
#' @return the character string equal to \code{x} with the first letter capitalized.
#'
#' @details The function works for simple names and compound names that are separated by
#' a space or by the character '-'. It also works for names that are entirely capitalized.
#' This function was adapted from the function `simpleCap` from package `hpoPlot` by Daniel Greene.
#'
#' @author Renato A. F. de Lima (after Daniel Greene)
#'
#' @export capName
#'
#' @examples
#' # Simple name
#'   capName("gentry")
#'   capName("HATSCHBACH")
#'
#' # Names with generational suffixes
#'   capName("leitao filho")
#'
#' # Compound names
#'   capName("saint-hilaire")
#'
capName <- function(x) {
  # check input:
  if (length(x)>1)
    stop("currently, 'x' cannot be a vector of strings!")

  # identifying compound names:
  if(grepl('-', x)) {

    split <- strsplit(x, "-")[[1]]
    nome <- paste(toupper(substring(split, 1, 1)),
                  tolower(substring(split, 2)),
                  sep = "",
                  collapse = "-")

    } else {

      split <- strsplit(x, " ")[[1]]
      nome <- paste(toupper(substring(split, 1, 1)),
                    tolower(substring(split, 2)),
                    sep = "",
                    collapse = " ")

    }
  return(nome)
}
