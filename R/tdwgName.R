#' @title Format People's Name To TDWG Standard
#'
#' @description Convert a single collector or determiner name into the
#'   \href{https://www.tdwg.org/}{Biodiversity Information Standards} (TDWG)
#'   format.
#'
#' @param x the character string.
#'
#' @return The character string \code{x} in the TDWG format. If only one name
#'   is given, the function returns \code{x} with the first letter capitalized.
#'
#' @details The function puts the name of a person into the format suggested by
#'   the \href{https://www.tdwg.org/}{Biodiversity Information Standards}
#'   (TDWG). The standard name format is: last name, followed by a comma and
#'   then the initials, separated by points (e.g. Hatschbach, G.G.).
#'
#'   Currently, the function removes name prefixes or prepositions (e.g. de,
#'   dos, van, ter, ...). Also, it removes some titles (i.e. Dr., Prof., Pe.)
#'   but not all of them (e.g. Doctor, Priest, etc.). The function also does not
#'   handle hyphenated first names.
#'
#'   The function is relatively stable regarding the input
#'   format and spacing, but it may not work in all cases, particularly if the
#'   string provided already contains commas.
#'
#' @author Renato A. F. de Lima & Hans ter Steege
#'
#' @references
#'
#' Conn, Barry J. (ed.) (1996). HISPID 3 - Herbarium Information Standards and
#'   Protocols for Interchange of Data. Herbarium Information Systems Committee'
#'   (HISCOM). https://www.tdwg.org/standards/hispid3/
#'
#' Willemse, L.P., van Welzen, P.C. & Mols, J.B. (2008).
#'   Standardisation in data-entry across databases: Avoiding Babylonian
#'   confusion. Taxon 57(2): 343-345.
#'
#' @export
#'
#' @examples
#'   # Simple name
#'   tdwgName("Al Gentry")
#'   tdwgName("Alwyn Howard Gentry")
#'   tdwgName("Gert G. Hatschbach")
#'   tdwgName("HATSCHBACH, G.G.")
#'   tdwgName("HATSCHBACH, G. G.")
#'   tdwgName("G. G. HATSCHBACH")
#'
#'   # Name with prepositions
#'   tdwgName("Carl Friedrich Philipp von Martius")
#'   tdwgName("Alphonse Louis Pierre Pyrame de Candolle")
#'   tdwgName("Simon Jan van Ooststroom")
#'   tdwgName("Maria da Silva")
#'   tdwgName("Silva, M. da")
#'   tdwgName("da Silva, M.")
#'
#'   # Name with generational suffixes
#'   tdwgName("Hermogenes Leitao Filho")
#'   tdwgName("Leitao Filho, H.")
#'   tdwgName("Leitao Filho, H.F.")
#'   tdwgName("Leitao Filho, HF")
#'   tdwgName("S.J. Filho Neto")
#'
#'   # Compound last name (needs to be marked with a '-')
#'   tdwgName("Augustin Saint-hilaire")
#'   tdwgName("Saint-Hilaire A.")
#'   tdwgName("Augustin Saint Hilaire") # compound name missing '-'
#'
#'   # Names with titles
#'   tdwgName("Pe. Raulino Reitz")
#'   tdwgName("Prof. Hermogenes de Freitas Leitao Filho")
#'
#'   # Unusual formatting (function won't always work)
#'   tdwgName("[D. Hugh-Jones]") #names inside bracket: output correct
#'   tdwgName("Cyl Farney Catarino de Sa") # small last name, no comma: output correct
#'   tdwgName("Cyl Sa") # small last name, without comma: output incorrect (inverted)
#'   tdwgName("Sa, Cyl") # small last name, with comma: output incorrect (inverted)
#'   tdwgName("L. McDade") #cannot recognize names starting with Mc or Mac
#'   tdwgName("Jan van der Hoeven") #double name preprositions
#'   tdwgName("Cesar Sandro, Esteves, F") # one name, two commas: fails to get the right last name
#'   # two or more names: output incorrect (combine names of authors)
#'   tdwgName("Mendonca Filho, C.V.; Neto, F.C.C.")
#'   # two or more names, separeted by comma: output incorrect (combine names of authors)
#'   tdwgName("A. Alvarez, A. Zamora & V. Huaraca")
#'   # two names, not separated by comma: output incorrect (combine names of authors)
#'   tdwgName("Karl Emrich & Balduino Rambo")
#'
tdwgName <- function(x) {

  # check input:
  if (length(x) > 1)
    stop("input 'name' cannot be a vector of strings!")

  # name inside brackets? removing here and adding after editions
  bracks <- grepl('^\\[', x, perl = TRUE) & grepl('\\]$', x, perl = TRUE)
  x <- gsub("^\\[|\\]$", "", x, perl = TRUE)

  # first edits:
  if (grepl(", [A-Z]", x, perl = TRUE))
    x <- fixName(x)                                 # fixing names already in the TDWG format
  x <- gsub("[.]", ". ", x, perl = TRUE)            # adding a space between points
  x <- gsub("  ", " ", x, fixed = TRUE)             # removing double spaces

  # removing unwanted characters
  x <- gsub(", --$| --, --|^-\\. ||^--\\. |^-- |^\\* ", "", x, perl = TRUE)

  # removing treatment prepositions (e.g. Dr., Prof., Pe., ...)
  x <- gsub("^Dr\\. |Pe\\. |Prof\\. ", "", x, perl = TRUE)

  # spliting the name
  names <- unlist(strsplit(x, " ", perl = TRUE))          # split o names and initials
  names <- as.character(unlist(sapply(names,
                                      FUN = capName)))    # capitalizing first letter of each name

  if (length(names) < 2)
    return(names)				    # stop if there is only one name

  lastname <- names[[length(names)]]

  # identifying names with generational suffixes
  # Add suffixs: II, fils, Sr.
  # Re-check encoding problems related to cp equals to 'Junior' and 'Junior,' com acento agudos no u
  cp <- c("Filho", "Filho,", "Neto", "Neto,", "Jr.", "Jr.,", "Junior", "Junior,", "Sobrinho", "Sobrinho,") #compound names
  if (any(names %in% cp)) {

    lastname <- tail(names[!names %in% cp], 1)
    cp.nome <-  paste(names[names %in% cp], collapse = " ") # collapse if there are two gen. suffixes
    other.names <- names[!names %in% cp & !names %in% lastname]

  } else {

    lastname <- lastname
    other.names <- names[1:(length(names) - 1)]

  }

  # putting last name in the good order
  i <- 1
  while ((nchar(lastname) < 3) & (i <= length(names))) {
    lastname <- names[i]
    other.names <- names[!names %in% c(lastname)]
    i = i + 1

  }

  lastname <- gsub("\\.$", "", lastname, perl = TRUE)
  # making sure gen. suffixes did not got mixed up
  if(any(names %in% cp)) {

    cp.nome <- gsub("\\.$", "", cp.nome, perl = TRUE)
    lastname <- paste(lastname, cp.nome, sep = " ")
    lastname <- paste(unique(strsplit(lastname, " ")[[1]]), collapse = " ")
    other.names <- other.names[!other.names %in% names[names %in% cp]]

  } else {

    lastname <- lastname
    other.names <- other.names

  }

  #Creating and editing the name initials
  initials <- sapply(other.names, function(x)
    toupper(strsplit(x, "", perl = TRUE)[[1]][1]))

  #Editing the name initials
  if (any(initials == "-"))
    initials[initials == "-"] <- substr(names(initials[initials == "-"]), 1, 2)
  initials <- initials[!grepl("^De$|^Dos$|^Do$|^Da$|^Das$|^Von$|^Van$|^Van Der$|^Van Den$|^Ter$", names(initials))]
  initials <- paste(initials, collapse = ".")
  initials <- paste(initials, ".", sep = "")

  # Creating the name in the TDWG format
  name.correct <- paste(lastname, initials, sep = ", ")
  # Final edits (removing duplicated commas and bad name endings)
  name.correct <- gsub(",,", ",", name.correct, fixed = TRUE)
  name.correct <- gsub(", \\.$", "", name.correct, perl = TRUE)
  #name.correct <- gsub("NANA", NA, name.correct)

  # Adding brackets (if needed)
  if (bracks == TRUE)
    name.correct <- paste("[", name.correct, "]", sep = "")

  return(name.correct)
}
