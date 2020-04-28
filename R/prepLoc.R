#' @title Format Locality String
#'
#' @description Edit and simplify the notation plantR locality search string
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
  ## Put this object in the sysData??
  unwanted_array = list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                        'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                        'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='S', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                        'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                        'ö'='o', 'ø'='o', 'ü'='u', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )
  # Removing unwanted characters
  x = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), x))

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
