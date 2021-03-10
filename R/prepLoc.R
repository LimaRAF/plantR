#' @title Format Locality String
#'
#' @description Simplify the notation of the standard __plantR__ locality
#'   string, which is used for the validate localities and geographical
#'   coordinates.
#'
#' @param x character. The concatenated locality string to be simplified.
#'
#' @return The edited locality string.
#'
#' @details The goal of this function is to simplify the locality strings in
#'   order to reduce variations in locality notation and thus to increase the
#'   chances of match with the __plantR__ gazetteer and maps.
#'
#'   It tranforms the string to lowercase and removes special characters and
#'   prepositions of locality names.
#'
#' @author Renato A. F. de Lima
#'
#' @examples
#'   prepLoc("Brazil_Rio de Janeiro")
#'   prepLoc("Brazil_Rio Grande do Sul_Coqueiros do Sul")
#'   prepLoc("Brazil_Bahia_Arraial d'Ajuda")
#'   prepLoc("Brazil_São Paulo_Sete Barras_Parque Estadual de Carlos Botelho")
#'   prepLoc("Cuba_Isla de la Juventud")
#'
#' @export prepLoc
#'
#' @seealso \link[plantR]{strLoc}
#'
#' @importFrom stringr str_trim
#'
prepLoc <- function(x) {

  # Removing unwanted characters
  x <- tolower(rmLatin(x))

  # Correcting NAs converted to lower caracters
  x <- gsub("_na_", "_NA_", x, fixed = TRUE)
  x <- gsub("_na_", "_NA_", x, fixed = TRUE)
  x <- gsub("_na$", "_NA", x, perl = TRUE)

  # Removing prepositions and other unwanted characters
  x <- gsub("-", " ", x, fixed = TRUE)
  x <- gsub(" - ", " / ", x, fixed = TRUE)
  x <- gsub(" de la | del | du ", " ", x, perl = TRUE)
  x <- gsub(" dos | das | des ", " ", x, perl = TRUE)
  x <- gsub(" do | da | de ", " ", x, perl = TRUE)
  # x <- gsub(' d\' | d\'| d`| d´| d\"| d’', " ", x, perl = TRUE)
  x <- gsub(' d\' | d\'| d\u0060| d\u00b4| d\"| d\u2019', " ", x, perl = TRUE)
  x <- gsub('\\.$', "", x, perl = TRUE)
  x <- gsub(" dx ", " ", x, fixed = TRUE)

  # Removing possible problems
  x <- gsub("s\\+", " ", x, perl = TRUE)
  x <- stringr::str_trim(x)

  return(x)
}
