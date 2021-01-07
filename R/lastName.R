#' @title Get Last Name
#'
#' @description Extract the last name of the collector or determiner of a
#'   biological specimen.
#'
#' @param name the character string or vector containing the names.
#' @param noName character. The standard notation for missing names in
#'   \code{name}. Default to "s.n.".
#'
#' @return the last name provided in \code{name} with the first letter
#'   capitalized.
#'
#' @details The function works for simple last names or compound last names,
#'   independently if names are provided in lower or capital letters. It is
#'   relatively stable to the format and spacing of the string provided, but it
#'   may not work in all cases.
#'
#'   It implicitly assumes that last names are the ones at the end of the name or
#'   the first name preceeding the comma.
#'
#'   If only one name is given, the function returns the same name with the first
#'   letter capitalized.
#'
#'   For missing names (i.e. "", " " or NAs) the function returns the character
#'   defined by the argument `noName`
#'
#' @author
#'   Renato A. F. de Lima & Hans ter Steege
#'
#' @importFrom stringr str_trim
#'
#' @export lastName
#'
#' @examples
#'
#' # Simple last name
#'  lastName("Al Gentry")
#'  lastName("Gert Guenther Hatschbach")
#'  lastName("Gert G. Hatschbach")
#'  lastName("GERT GUENTHER HATSCHBACH")
#'  lastName("HATSCHBACH, G.G.")
#'  lastName("HATSCHBACH, G. G.")
#'  lastName("G. G. HATSCHBACH")
#'
#'  # Last name with generational suffixes
#'  lastName("Hermogenes Leitao Filho")
#'  lastName("Leitao Filho, H.")
#'  lastName("Leitao Filho, H.F.")
#'  lastName("Filho Neto, S.J.")
#'
#'  # Compound last name
#'  lastName("Augustin Saint-hilaire")
#'  lastName("Saint-Hilaire A.")
#'
#'  # Multiple names
#'  names <- c("Gentry, AH", "Gentry A.H.",
#'  "Leitao filho, H. F.","Saint-Hilaire, augustin")
#'  lastName(names)
#'
#'  # Unusual formatting
#'  lastName("Cesar Sandro, Esteves, F")
#'  lastName("Mendonca Filho, C.V. Neto, F.C.C.")
#'  # two or more names, separated by comma: output correct
#'  lastName("A. Alvarez, A. Zamora & V. Huaraca")
#'  # two or more names, not separated by comma: output incorrect (give names of authors)
#'  lastName("Karl Emrich & Balduino Rambo")
#'
lastName <- function(name, noName = "s.n.") {

  # detecting missing names
  miss.name <- name %in% c("NA", NA, "", " ", noName)

  # first edits:
  name <- gsub("[.]", ". ", name, perl = TRUE) # adding a space between points
  name <- gsub("  ", " ", name, fixed = TRUE) # removing double spaces

  # spliting the names
  names <- strsplit(name[!miss.name], " ", fixed = TRUE)

  no.first <- lengths(names) < 2
  comma <- grepl("[a-z;A-Z],", name[!miss.name], perl = TRUE)
  last.name <- names

  # identifying names with last name coming first and separated by a comma
  if (any(comma))
    last.name[comma] <- lapply(last.name[comma],
                               function(x) x[grepl(",", x, fixed = TRUE)][1])

  outros <- !no.first & !comma
  if (any(outros))
    last.name[outros] <- lapply(last.name[outros],
                                function(x) x[length(x)])

  # Detecting possible problems with abbreviated initials at the end of the name
  probs <- grepl("^[a-z;A-Z]\\.$", last.name)
  if (any(probs))
    last.name[probs] <- sapply(names[probs],
                               function(x) x[which.max(nchar(x))])

  # identifying names with generational suffixes
  cp.nome <- rep("", length(last.name))

  # CHECK: Add suffixs: II, fils, Sr.
  # Re-check encoding problems related to cp equals to 'Junior' and 'Junior,' com acento agudos no u
  cp <- c("Filho", "Filho,", "Neto", "Neto,", "Jr.", "Jr.,", "Junior", "Junior,", "Sobrinho", "Sobrinho,") #compound names
  cp <- c(cp, tolower(cp))
  gen.suf <- last.name %in% cp
  if (any(gen.suf)) {

    cp.nome[gen.suf] <- unlist(last.name[gen.suf])
    #slowest part of the code:
    last.name[gen.suf] <- lapply(names[gen.suf],
                                 function(x) x[!x %in% cp & !grepl("^[A-Z].$", x, perl = TRUE)])
    last.name[gen.suf] <-
      mapply(`[`, last.name[gen.suf], lengths(last.name[gen.suf]))

    double.cp <- sapply(last.name[gen.suf], identical, character(0))
    last.name[gen.suf][double.cp] <- ""
    cp.nome[gen.suf][double.cp] <- unlist(lapply(names[gen.suf][double.cp],
                                                 function(x) paste(x[!grepl("^[A-Z].$", x, perl = TRUE)], collapse = " ")))
    last.name[gen.suf] <-
      paste(last.name[gen.suf], cp.nome[gen.suf], sep = " ")
  }

  # Removing the point at the end of the string
  last.name <-
    stringr::str_trim(gsub("\\.$|,$", "", last.name, perl = TRUE))

  # Capitalizing the first letter and saving
  last.name <- capName(last.name)
  #last.name <- stringr::str_to_title(last.name)
  name[!miss.name] <- last.name
  name[miss.name] <- noName

  return(name)
}
