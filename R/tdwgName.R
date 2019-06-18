#' @title Format Name To TDWG Standard
#'
#' @description Put the the collector or determiner name in the TDWG format.
#'
#' @param name the character string.
#'
#' @return the character string \code{name} in the TDWG format. If only one name
#' is given, the function returns \code{name} with the first letter capitalized.
#'
#' @details The function puts the name of a person into the format suggested by
#' the TDWG <International Working Group on Taxonomic Databases for Plant Sciences>.
#' The standard notation is: last name, followed by a comma and then the initials,
#' separated by points (e.g. Hatschbach, G.G.). Currenlty, the function remove name
#' prefixs or prepositions (e.g. de, dos, van, ter, ...). The function is relatively
#' stable regarding the input format and spacing, but it may not work in all cases,
#' particularly if the string provided already contain commas.
#'
#' @author Lima, R.A.F. & ter Steege, H.
#'
#' @references Willemse, L.P., van Welzen, P.C. & Mols, J.B. (2008). Standardisation in data-entry across databases: Avoiding Babylonian confusion. Taxon 57(2): 343-345.
#'
#' @export tdwgName
#'
#' @examples
#' # Simple name
#'   tdwgName("Al Gentry")
#'   tdwgName("Gert Guenther Hatschbach")
#'   tdwgName("Gert G. Hatschbach")
#'   tdwgName("HATSCHBACH, G.G.")
#'   tdwgName("HATSCHBACH, G. G.")
#'   tdwgName("G. G. HATSCHBACH")
#' # Name with generational suffixes
#'   tdwgName("Hermogenes Leitao Filho")
#'   tdwgName("Leitao Filho, H.")
#'   tdwgName("Leitao Filho, H.F.")
#'   tdwgName("S.J. Filho Neto")
#' # Compound last name
#'   tdwgName("Augustin Saint-hilaire")
#'   tdwgName("Saint-Hilaire A.")
#' # Unusual formatting (function won't always work)
#'   tdwgName("Cesar Sandro, Esteves, F") # one author, two commas: fails to get the right last name
#'   tdwgName("Mendonca Filho, C.V. Neto, F.C.C.") # two authors not properly separated: combine names of both authors
#'   tdwgName("Cyl Farney Catarino de Sa") # small last name, no comma: output correct
#'   tdwgName("Sa, Cyl") # small last name, no comma: output incorrect (inverted)
tdwgName = function(name) {
  # check input:
  if (length(name)>1) { stop("input 'name' cannot be a vector of strings!") }

  # first edits:
  name = gsub("[.]", ". ", name)                         # adding a space between points
  name = gsub("  ", " ", name)                           # removing double spaces

  # spliting the name
  names = unlist(strsplit(name," "))                     # split o names and initials
  names = as.character(unlist(sapply(names,capName)))    # capitalizing first letter of each name

  if (length(names) < 2) return(names)				    # stop if there is only one name

  lastname = names[[length(names)]]

  # identifying names with generational suffixes
  # Add suffixs: II, fils, Sr.
  # Re-check encoding problems related to cp equals to 'Junior' and 'Junior,' com acento agudos no u
  cp = c("Filho","Filho,","Neto","Neto,","Jr.","Jr.,","Junior","Junior,","Sobrinho","Sobrinho,") #compound names
  if (any(names %in% cp)) {
    lastname = tail(names[!names %in% cp],1)
    cp.nome = paste(names[names %in% cp],collapse = " ") #collapse if there are two gen. suffixes
    other.names = names[!names %in% cp & !names %in% lastname]
  } else {
    lastname = lastname
    other.names = names[1:(length(names)-1)]
  }

  # putting last name in the good order
  i = 1
  while((nchar(lastname) < 3) & (i <= length(names))){
    lastname = names[i]
    other.names = names[!names %in% c(lastname)]
    i = i + 1
  }

  lastname = gsub("\\.$","",lastname)
  # making sure gen. suffixes did not got mixed up
  if(any(names %in% cp)) {
    cp.nome = gsub("\\.$","",cp.nome)
    lastname = paste(lastname,cp.nome,sep=" ")
    lastname = paste(unique(strsplit(lastname," ")[[1]]),collapse =" ")
    other.names = other.names[!other.names %in% names[names %in% cp]]
  } else {
    lastname = lastname
    other.names = other.names
  }
  initials = sapply(other.names, function(x) toupper(strsplit(x,"")[[1]][1]))
  initials = initials[!grepl("^De$|^Dos$|^Do$|^Da$|^Das$|^Von$|^Van$|^Van Der$|^Van Den$|^Ter$",names(initials))]
  initials = paste(initials,collapse=".")
  initials = paste(initials,".",sep="")

  # Creating the name in the TDWG format
  name.correct = paste(lastname,initials,sep=", ")
  # Removing possible duplicated commas
  name.correct = gsub(",,",",",name.correct)
  return(name.correct)
}
