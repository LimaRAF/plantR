#' @title Remove Unwanted Spaces
#'
#' @param x a character or vector
#'
#' @return the character `x` without trailing or double spaces
#'
#' @keywords internal
#'
#' @noRd
#'
#' @author Renato A. F. de Lima
#'
#' @examples
#' \dontrun{
#' squish(" Lindsaea   lancea ")
#' }
#'
squish <- function (x) {

  x <- gsub("\\s\\s+", " ", as.character(x), perl = TRUE)
  x <- gsub("^ | $", "", x, perl = TRUE)

  return(x)
}

