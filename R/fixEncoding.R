#' @title Fix Encoding Problems
#'
#' @description Simple function to replace typical encoding problems when
#'   importing text with accents or special characters which is not in UTF-8
#'   (i.e. latin1 encoding).
#'
#' @param x a vector with characters to be replaced
#' @param encoding a character containing the encoding used to mark the output
#' vector
#'
#' @return the input vector with the encoding problems replaced
#'
#' @author Renato A. F. de Lima
#'
#' @keywords internal
#'
#' @details The function makes no encoding guessing. It just directly replaces
#' common enconding problems from text in latin1 to UTF-8 characters. So, make
#' sure that this replacement makes sure to your data.
#'
#' @examples
#'   nomes <- c("JosÃ©", "GonÃ§alves", "MÃ¼ller", "LondoÃ±o")
#'
#' \dontrun{
#'   fixEncoding(nomes)
#'   fixEncoding(nomes, encoding = "UTF-8")
#'   fixEncoding(nomes, encoding = "latin1")
#'
#' }
#'
fixEncoding <- function(x, encoding = NULL) {

  #Check input
  if (!is.null(encoding))
    if (!encoding[1] %in% c("latin1", "UTF-8", "bytes"))
      warning("Available encoding are: 'latin1', 'UTF-8' and 'bytes'",
              call. = FALSE)

  #Getting the common latin1 to UTF-8 encoding problems and replacements
  bad_enc <- badEncoding
  good_enc <- names(badEncoding)

  #Making the replacements (gsub is faster then using stringr::str_replace_all)
  for (i in seq_along(bad_enc))
    x <- gsub(bad_enc[i], good_enc[i], x, fixed = TRUE)

  #Making sure the output has the desired encoding
  if (!is.null(encoding))
    Encoding(x) <- encoding

  return(x)
}
