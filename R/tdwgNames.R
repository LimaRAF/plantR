#' @title Format Multiple People's Names To TDWG Standard
#'
#' @description Put the the multiple collector or determiner names in the TDWG format.
#'
#' @param x the character string.
#' @param sep.in the character string.
#' @param sep.out the character string.
#'
#' @return the character string \code{x} in the TDWG format. If the names of
#' only one nperson is given, the function returns the same as function \code{tdwgName}.
#'
#' @details The function puts the name of multiple persons into the format suggested by
#' the TDWG <International Working Group on Taxonomic Databases for Plant Sciences>.
#' The standard notation is: last name, followed by a comma and then the initials,
#' separated by points (e.g. Hatschbach, G.G.). Currenlty, the function remove name
#' prefixs or prepositions (e.g. de, dos, van, ter, ...). The function is relatively
#' stable regarding the input format and spacing, but it may not work in all cases,
#' particularly if the string provided already contain commas.
#'
#' The user must chose the input and output character separating the multiple names.
#' By default, the function uses ';', ' e ', ' et ' '&' and '|' as input and '|' as output
#' separators between names. If special separators are used (e.g. '.,'), make sure that their
#' notation contains the scape symbols required when using regular expressions in R (e.g. '\\.,').
#'
#' @author Lima, R.A.F.
#'
#' @references Willemse, L.P., van Welzen, P.C. & Mols, J.B. (2008). Standardisation in data-entry across databases: Avoiding Babylonian confusion. Taxon 57(2): 343-345.
#'
#' @export tdwgNames
#'
#' @seealso \code{tdwgName}
#'
#' @examples
#' # Simple names
#'   tdwgNames("Karl Emrich & Balduino Rambo")
#'   tdwgNames("R. Reitz; R.M. Klein")
#'   tdwgNames("R. Reitz; R.M. Klein", sep.out= " & ")
#'   tdwgNames("Alwyn H. Gentry|Elsa M. Zardini")
#'   tdwgNames("Gert G. Hatschbach")
#' # Names with generational suffixes
#'   tdwgNames("Leitão Filho, H.F.; Shepherd, G.J.")
#'   tdwgNames("Leitao Filho, HF")
#' # Compound last names
#'   tdwgNames("A. Ducke; Dárdano de Andrade-Lima")
#' # Multiple names separated by commas
#'   tdwgNames("A. Alvarez, A. Zamora & V. Huaraca") # output incorrect (combine names of authors)
#'   tdwgNames("A. Alvarez, A. Zamora & V. Huaraca", sep.in=c(",","&")) # output correct
#'   tdwgNames("Alvarez, A., Zamora, A. & Huaraca, V.", sep.in=c(",","&")) # output incorrect (make initials separate names)
#'   tdwgNames("Alvarez, A., Zamora, A. & Huaraca, V.", sep.in=c("\\.,","&")) # output correct
#' # Unusual formatting (function won't always work)
#'   tdwgNames("Cesar Sandro, Esteves, F") # one name, two commas: fails to get the right last name
#'
tdwgNames = function(x, sep.in = c(";","&"," e "," et ","\\|"), sep.out = " | ") {

  # check input:
  if (length(x)>1) { stop("input 'name' cannot be a vector of strings!") }

  # name inside brackets? removing here and adding after editions
  bracks = grepl('^\\[',x) & grepl('\\]$',x)
  x = gsub("^\\[|\\]$", "", x)

  # first edits:
  x = gsub("  "," ",x)
  x = gsub("  "," ",x)
  x = gsub(paste(sep.in, collapse="|"), ";", x)
  x = gsub(" ; ",";",x)
  x = gsub("&|\\|",";",x)
  x = gsub("; ;",";",x)
  x = gsub(" ;",";",x)
  x = gsub('^\\;','',x)
  x = gsub('\\( ','\\(',x)
  x = gsub(' \\)','\\)',x)
  x = gsub(' <U+','; ',x)
  x = gsub(" s.n. ",";",x)
  x = gsub("Collector\\(s\\):","",x)
  x = gsub("\\(Coll.",";",x)
  x = gsub(" \\(\\?\\) ","; ",x)

  # Spliting the list of names into a list
  autores = as.character(unlist(sapply(strsplit(x,";|; "), FUN = stringr::str_trim)))

  # Converting names to TDWG format
  autores.tdwg = sapply(autores, FUN = tdwgName)

  # Creating the output name list in the TDWG format
  name.correct = paste(autores.tdwg, collapse= sep.out)

  # Final edits (removing duplicated commas and bad name endings)
  name.correct = gsub(",,", ",", name.correct)
  name.correct = gsub("\\.\\.", ".", name.correct)
  name.correct = gsub(", \\.$", "", name.correct)

  # Adding brackets (if needed)
  if(bracks==TRUE) name.correct = paste("[",name.correct,"]",sep="")

  return(name.correct)
}
