#' @title Remove Special Characters
#'
#' @description Simple function to replace accents by the same or corresponding
#'   letters without accents. Currently, the function basically replaces special
#'   characters corresponding to the Latin-1 Supplement and very few Latin
#'   Extended-A. For a more complete replacement of all non-ASCII characters,
#'   please use the function `replace_non_ascii()` from package __textclean__.
#'
#' @param x a vector with characters to be replaced
#'
#' @return the input vector with the special characters replaced
#'
#' @author Renato A. F. de Lima
#'
#' @keywords internal
#'
#' @examples
#'   nomes <- c("Thom\u00e9", "Mu\u00f1oz", "\u0153uf")
#'   nomes
#'
#' \dontrun{
#'   rmLatin(nomes)
#' }
#'
rmLatin <- function(x) {

  #Getting the special characters to be replaced
  unwanted_latin <- names(unwantedLatin)
  replace_latin <- unwantedLatin

  #Single letter replacements
  replace_latin1 <- replace_latin[nchar(replace_latin) == 1]
  unwanted_latin1 <- unwanted_latin[nchar(replace_latin) == 1]
  x <- chartr(
    paste(unwanted_latin1, collapse = ''),
    paste(replace_latin1, collapse = ''),
    x)

  #Double letter replacements
  replace_latin2 <- replace_latin[nchar(replace_latin) == 2]
  names(replace_latin2) <- unwanted_latin[nchar(replace_latin) == 2]
  for (i in 1:length(replace_latin2))
    x <- gsub(names(replace_latin2)[i],
                  replace_latin2[i],
                  x, fixed = TRUE)

  return(x)
}
