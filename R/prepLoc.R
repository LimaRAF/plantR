#' @title Format Locality String
#'
#' @description Simplify the notation plantR locality search string
#'
#' @param x the concatenated string of localities to be edited.
#'
#' @return the edited locality strings.
#'
#' @details The function removes unwanted hifens and prepositions of locality names
#'
#' @author Lima, R.A.F.
#'
#' @examples
#'   prepLoc("Brazil_Rio de Janeiro")
#'   prepLoc("Brazil_São Paulo_São José do Rio Preto")
#'   prepLoc("Brazil_Bahia_Arraial d'Ajuda")
#'   prepLoc("Brazil_São Paulo_Sete Barras_Parque Estadual de Carlos Botelho")
#'
prepLoc <- function(x) {
  # Removing unwanted characters
  load("./R/sysdata.rda")
  unwanted_array <- unwanted_array

  # Removing unwanted characters
  x = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), x))

  # Correcting NAs converted to lower caracters
  x = gsub("_na_", "_NA_", x)
  x = gsub("_na_", "_NA_", x)

  # Removing prepositions and other unwanted
  x = gsub("-"," ", x)
  x = gsub(" - "," / ", x)
  x = gsub(" do | da | de ", " ", x)
  x = gsub(" dos | das | des ", " ", x)
  x = gsub(' d\' | d\'| d´| d\"| d’', " ", x)
  x = gsub('\\.$', "", x)
  x = gsub(" dx ", " ", x)

  # Removing possible problems
  x = gsub("   "," ", x)
  x = gsub("  "," ", x)
  x = str_trim(x)

  return(x)
}
