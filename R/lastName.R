#' @title Get Last Name
#'
#' @description Extract the last name of the collector or determiner of a biological specimen.
#'
#' @param name the character string.
#'
#' @return the character string in lower case with the last name of \code{name}.
#'
#' @details The function works for simple last names or compund last names, independently
#' if names are provided  in lower or capital letters. It is relatively stable to the format
#' and spacing of the string provided, but it may not work in all cases.
#'
#' If only one name is given, the function returns the same name in lower case.
#'
#' @author Lima, R.A.F. & ter Steege, H.
#'
#' @export lastName
#'
#' @examples
#' # Simple last name
#'   lastName("Al Gentry")
#'   lastName("Gert Guenther Hatschbach")
#'   lastName("Gert G. Hatschbach")
#'   lastName("GERT GUENTHER HATSCHBACH")
#'   lastName("HATSCHBACH, G.G.")
#'   lastName("HATSCHBACH, G. G.")
#'   lastName("G. G. HATSCHBACH")
#' # Last name with generational suffixes
#'   lastName("Hermogenes Leitao Filho")
#'   lastName("Leitao Filho, H.")
#'   lastName("Leitao Filho, H.F.")
#'   lastName("Filho Neto, S.J.")
#' # Compound last name
#'   lastName("Augustin Saint-hilaire")
#'   lastName("Saint-Hilaire A.")
#' # Unusual formatting
#'   lastName("Cesar Sandro, Esteves, F")
#'   lastName("Mendonca Filho, C.V. Neto, F.C.C.")
#'   lastName("Cyl Farney Catarino de Sa") # small last name, no comma: output incorrect
#'   lastName("Sa, Cyl") # small last name, with comma: output correct
#'   lastName("A. Alvarez, A. Zamora & V. Huaraca") # two or more names, separated by comma: output correct
#'   lastName("Karl Emrich & Balduino Rambo") # two or more names, separated by comma: output incorrect (give names of authors)
lastName = function(name) {
  # check input:
  if (length(name)>1) { stop("input 'name' cannot be a vector of strings!") }

  # first edits:
  name = gsub("[.]", ". ", name)                         # adding a space between points
  name = gsub("  ", " ", name)                           # removing double spaces

  # spliting the name
  names = unlist(strsplit(name," "))                     # split of names and its initials
  names = as.character(unlist(sapply(names,capName)))    # capitalizing first letter of each name
  if (length(names) < 2) return(tolower(names))		       # stop if there is only one name

  # identifying names with last name coming first and separated by a comma
  if(any(grepl(",",names))) {
    lastname = head(names[grep(",",names)],1)
  } else {
    lastname = names[[length(names)]]
  }

  # identifying names with generational suffixes
  # Add suffixs: II, fils, Sr.
  # Re-check encoding problems related to cp equals to 'Junior' and 'Junior,' com acento agudos no u
  cp = c("Filho","Filho,","Neto","Neto,","Jr.","Jr.,","Junior","Junior,","Sobrinho","Sobrinho,") #compound names
  if (sum(names %in% cp)>0) {
    lastname = tail(names[!names %in% cp],1)
    cp.nome = names[names %in% cp]
    other.names = names[!names %in% cp & !names %in% lastname]
  } else {
    lastname = lastname
    other.names = names[1:(length(names)-1)]
  }

  # putting last names in the good order
  i = 1
  while((nchar(lastname) < 3) & (i <= length(names))){
    lastname = names[i]
    other.names = names[!names %in% c(lastname)]
    i = i + 1
  }

  lastname = gsub("\\.$","",lastname) # Removing the point at the end of the string
  if(sum(names %in% cp)==0) {
    lastname = lastname
    other.names = other.names
  } else {
    cp.nome = gsub("\\.$","",cp.nome)
    lastname = paste(lastname,cp.nome,sep=" ")
    other.names = other.names[!other.names %in% cp.nome]
  }

  if (length(lastname)>1) {
    lastname = lastname[grepl(",",lastname)]
    lastname = head(lastname,1)
  } else { lastname = lastname }
  lastname = tolower(gsub('\\.|,',"",lastname))

  return(lastname)
}


