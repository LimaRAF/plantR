#' @title Format Locality String
#'
#' @description Simplify the notation of the `plantR` string of locality search
#'
#' @param x character. The concatenated locality string to be simplified.
#'
#' @return The edited locality strings.
#'
#' @details The function removes unwanted hifens and prepositions of locality names
#'
#' @author Renato A. F. de Lima
#'
#' @examples
#'   prepLoc("Brazil_Rio de Janeiro")
#'   prepLoc("Brazil_Rio Grande do Sul_Coqueiros do Sul")
#'   prepLoc("Brazil_Bahia_Arraial d'Ajuda")
#'   prepLoc("Brazil_São Paulo_Sete Barras_Parque Estadual de Carlos Botelho")
#'
#' @export prepLoc
#'
#' @importFrom stringr str_trim
#'
prepLoc <- function(x) {

  # Removing unwanted characters
  unwanted_array <- plantR:::unwanted_array
  x <-
    tolower(chartr(
      paste(names(unwanted_array), collapse = ''),
      paste(unwanted_array, collapse = ''),
      x
    ))

  # Correcting NAs converted to lower caracters
  x <- gsub("_na_", "_NA_", x)
  x <- gsub("_na_", "_NA_", x)

  # Removing prepositions and other unwanted
  x <- gsub("-", " ", x)
  x <- gsub(" - ", " / ", x)
  x <- gsub(" do | da | de ", " ", x)
  x <- gsub(" dos | das | des ", " ", x)
  x <- gsub(' d\' | d\'| d´| d\"| d’', " ", x)
  x <- gsub('\\.$', "", x)
  x <- gsub(" dx ", " ", x)

  # Removing possible problems
  x <- gsub("   ", " ", x)
  x <- gsub("  ", " ", x)
  x <- stringr::str_trim(x)

  return(x)
}
