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
#'
#' @title Replace Whitespaces
#'
#' @description Replaces all types of whitespace characters (e.g.
#'   non-breaking, thin, hair) into a regular space character (i.e.
#'   "\U0020")
#'
#' @param x a vector of characters (e.g. taxon names)
#'
#' @return the input `x` with spaces standardized
#'
#' @keywords internal
#'
#' @author Renato A. F. de Lima
#'
#' @noRd
#'
fixSpaces <- function(x) {

  space.codes <- c("\U0020", "\U00A0", "\U2000", "\U2001", "\U2002",
                   "\U2003", "\U2004", "\U2005", "\U2006", "\U2007",
                   "\U2008", "\U2009", "\U200A", "\U205F", "\U3000")

  space.patt <- paste(space.codes, collapse = "|")

  x1 <- gsub(space.patt, " ", x, perl = TRUE)

  return(x1)
}
#' @title Display Progress Bar
#'
#' @description
#' A simple function to display the status of parallel processing.
#' Function adapted from ConR R package and written by Gilles Dauby.
#'
#' @param show.progress logical
#' @param max.pb integer
#'
#' @noRd
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @keywords internal
#'
progressBar <- function(show.progress = TRUE, max.pb) {
  if (show.progress) {
    pb <- utils::txtProgressBar(min = 0,
                     max = max.pb,
                     style = 3)

    progress <- function(n)
      utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
  } else {
    opts <- NULL
    pb <- NA
  }
  return(list(opts = opts, pb = pb))
}
#' @title Collapse Non-Empty Strings
#'
#' @description
#' A simple function, similar to the base function `paste0()` but
#' avoiding problems related to empty strings.
#'
#' @param x one or more objects to be collapsed
#' @param collapse character string to separate the result
#'
#' @noRd
#'
#'
#' @keywords internal
#'
paste1 <- function(x, sep = "", collapse = NULL) {

  x1 <- x[!x %in% c("", " ", NA)]
  x1 <- paste(x1, sep = sep, collapse = collapse)

  return(x1)
}
#'
#' @title Read File in Help
#'
#' @param file a path to the file with the script to be read
#' @param text a character or vector of additional text to be added to
#'   the tag
#' @param tag one of Roxygen's documentation tag. Defaults to 'note'.
#'
#' @keywords internal
#'
#' @noRd
#'
readScript <- function(file = NULL, text = "", tag = "note") {
  lns <- readLines(file)
  lns <- paste(sprintf("\\code{%s}", lns), collapse = "; ")
  return(paste("\\", tag, "{", text, "\n", lns, ".}", sep = ""))
}
