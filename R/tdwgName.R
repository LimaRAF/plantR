#' @title Format People's Name To TDWG Standard
#'
#' @description Put the the collector or determiner name in the TDWG format.
#'
#' @param x the character string.
#'
#' @return the character string \code{x} in the TDWG format. If only one name
#' is given, the function returns \code{x} with the first letter capitalized.
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
#'   tdwgName("Alwyn Howard Gentry")
#'   tdwgName("Gert G. Hatschbach")
#'   tdwgName("HATSCHBACH, G.G.")
#'   tdwgName("HATSCHBACH, G. G.")
#'   tdwgName("G. G. HATSCHBACH")
#' # Name with generational suffixes
#'   tdwgName("Hermogenes Leitao Filho")
#'   tdwgName("Leitao Filho, H.")
#'   tdwgName("Leitao Filho, H.F.")
#'   tdwgName("Leitao Filho, HF")
#'   tdwgName("S.J. Filho Neto")
#' # Compound last name
#'   tdwgName("Augustin Saint-hilaire")
#'   tdwgName("Saint-Hilaire A.")
#' # Unusual formatting (function won't always work)
#'   tdwgName("[D. Hugh-Jones]") #names inside bracket: output correct
#'   tdwgName("Cyl Farney Catarino de Sa") # small last name, no comma: output correct
#'   tdwgName("Cyl Sa") # small last name, without comma: output incorrect (inverted)
#'   tdwgName("Sa, Cyl") # small last name, with comma: output incorrect (inverted)
#'   tdwgName("L. McDade") #cannot recognize names starting with Mc or Mac
#'   tdwgName("Cesar Sandro, Esteves, F") # one name, two commas: fails to get the right last name
#'   tdwgName("Mendonca Filho, C.V.; Neto, F.C.C.") # two or more names: output incorrect (combine names of authors)
#'   tdwgName("A. Alvarez, A. Zamora & V. Huaraca") # two or more names, separeted by comma: output incorrect (combine names of authors)
#'   tdwgName("Karl Emrich & Balduino Rambo") # two names, not separeted by comma: output incorrect (combine names of authors)
tdwgName = function(x) {
  # check input:
  if (length(x)>1) { stop("input 'name' cannot be a vector of strings!") }

  # name inside brackets? removing here and adding after editions
  bracks = grepl('^\\[',x) & grepl('\\]$',x)
  x = gsub("^\\[|\\]$", "", x)

  # first edits:
  if(grepl(", [A-Z]",x)) x = fixName(x)            # fixing names already in the TDWG format
  x = gsub("[.]", ". ", x)                         # adding a space between points
  x = gsub("  ", " ", x)                           # removing double spaces

  # removing unwanted characters
  x = gsub(", --$| --, --|^-\\. ||^--\\. |^-- |^\\* ", "", x)

  # removing treatment prepositions (e.g. Dr., Prof., Pe., ...)
  x = gsub("^Dr\\. |Pe\\. |Prof\\. ", "", x)

  # spliting the name
  names = unlist(strsplit(x, " "))                     # split o names and initials
  names = as.character(unlist(sapply(names, FUN= capName)))    # capitalizing first letter of each name

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

  #Creating and editing the name initials
  initials = sapply(other.names, function(x) toupper(strsplit(x,"")[[1]][1]))

  #Editing the name initials
  if(any(initials=="-")) initials[initials=="-"] = substr(names(initials[initials=="-"]),1,2)
  initials = initials[!grepl("^De$|^Dos$|^Do$|^Da$|^Das$|^Von$|^Van$|^Van Der$|^Van Den$|^Ter$",names(initials))]
  initials = paste(initials,collapse=".")
  initials = paste(initials,".",sep="")

  # Creating the name in the TDWG format
  name.correct = paste(lastname,initials,sep=", ")
  # Final edits (removing duplicated commas and bad name endings)
  name.correct = gsub(",,", ",",name.correct)
  name.correct = gsub(", \\.$", "",name.correct)

  # Adding brackets (if needed)
  if(bracks==TRUE) name.correct = paste("[",name.correct,"]",sep="")

  return(name.correct)
}
