#' @title Fix Encoding Problems
#'
#' @description Simple function to replace typical encoding problems when
#'   importing text with accents or special characters which is not in UTF-8
#'   (i.e. latin1 encoding).
#'
#' @param x a vector with characters to be replaced
#'
#' @return the input vector with the encoding replaced
#'
#' @author Renato A. F. de Lima & Leila Meyer
#'
#' @keywords internal
#'
#' @details The function makes no encoding guessing. It just directly replaces
#' common enconding problems from text in latin1 to UTF-8 characters. So, make
#' sure that this replacement makes sure to your data.
#'
#' @examples
#'   nomes <- c("Ã€", "Ã‚", "Ã™", "Ãš")
#'
#' \dontrun{
#'   fixEncoding(nomes)
#' }
#'
fixEncoding <- function(x) {

  #Getting the encoding problems and replacements
  bad_enc <- badEncoding
  good_enc <- names(badEncoding)

  #Making the replacements (gsub is faster then using stringr::str_replace_all)
  for (i in seq_along(bad_enc))
    x <- gsub(bad_enc[i], good_enc[i], x, fixed = TRUE)

  return(x)
}
