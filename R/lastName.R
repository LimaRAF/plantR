#' @title Get Last Name
#'
#' @description Extract the last name of the collector or determiner of a
#'   biological specimen.
#'
#' @param name the character string or vector containing the names.
#' @param noName character. The standard notation for missing names in
#'   \code{name}. Default to "s.n.".
#' @param invert logical. Should first name(s) be returned instead of the
#' last name? Default to FALSE.
#' @param initials logical. If first name(s) are chosen, should they be returned
#' in full or only their initials? Default to FALSE.
#'
#' @return the last name provided in \code{name} with the first letter
#'   capitalized.
#'
#' @details The function works for simple last names or compound last names,
#'   independently if names are provided in lower or capital letters. It is
#'   relatively stable to the format and spacing of the string provided, but it
#'   may not work in all cases.
#'
#'   It implicitly assumes that last names are (i) the ones provided at the end of
#'   the name string if there is no comma, or (ii)the first name preceeding the comma,
#'   if there is a comma in the name string.
#'
#'   If only one name is given, the function returns the same name with the first
#'   letter capitalized.
#'
#'   For missing names (i.e. "", " " or NAs) the function returns the character
#'   defined by the argument `noName`.
#'
#'   The function can also return all names but the last name detected by
#'   setting the argument `invert` to TRUE. In this case, user can chose between
#'   full first names and only their initials by setting the argument `initials`
#'   to TRUE.
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
#'  lastName(names, invert = TRUE)
#'  lastName(names, invert = TRUE, initials = TRUE)
#'
#'  # Unusual formatting
#'  lastName("Cesar Sandro, Esteves, F")
#'  lastName("Mendonca Filho, C.V. Neto, F.C.C.")
#'  # two or more names, separated by comma: output correct
#'  lastName("A. Alvarez, A. Zamora & V. Huaraca")
#'  # two or more names, not separated by comma: output incorrect (give names of authors)
#'  lastName("Karl Emrich & Balduino Rambo")
#'
#'  # Some problematic (unresolved) examples
#'  lastName("Neto, F.C.C.")
#'  lastName("Gentry AH")
#'
lastName <- function(name, noName = "s.n.", invert = FALSE, initials = FALSE) {

  # detecting missing names
  miss.name <- name %in% c("NA", NA, "", " ", noName)

  # first edits:
  name <- gsub("[.]", ". ", name, perl = TRUE) # adding a space between points
  name <- gsub("  ", " ", name, fixed = TRUE) # removing double spaces

  # spliting the names
  names <- strsplit(name[!miss.name], " ", fixed = TRUE)

  # Defining the general name formats (single, mult. with comma, mult. without comma)
  no.first <- lengths(names) < 2
  comma <- grepl("[a-z;A-Z;à-ý;À-Ý],", name[!miss.name], perl = TRUE)
  outros <- !no.first & !comma

  # identifying names with last name at the start and separated by a comma
  last.name <- names
  if (any(comma))
    last.name[comma] <- lapply(last.name[comma],
                               function(x) paste(
                                 x[1:grep(",", x, fixed = TRUE)[1]], collapse = " "))

  if (any(outros))
    last.name[outros] <- lapply(last.name[outros],
                                function(x) x[length(x)])

  # Detecting possible problems with abbreviated initials at the end of the name
  probs <- grepl("^[a-z;A-Z;à-ý;À-Ý]\\.$", last.name)
  if (any(probs))
    last.name[probs] <- sapply(names[probs],
                               function(x) x[which.max(nchar(x))])

  # identifying names with generational suffixes
  cp.nome <- rep("", length(last.name))
  cp <- c("Filho", "Filho,", "Neto", "Neto,", "Jr.", "Jr.,", "Junior", "Junior,", "Sobrinho", "Sobrinho,") #compound names
  cp <- c(cp, tolower(cp))
  gen.suf <- last.name %in% cp
  if (any(gen.suf)) {

    cp.nome[gen.suf] <- unlist(last.name[gen.suf])
    #slowest part of the code:
    last.name[gen.suf] <- lapply(names[gen.suf],
                                 function(x) x[!x %in% cp & !grepl("^[A-Z;à-ý;À-Ý].$", x, perl = TRUE)])
    last.name[gen.suf] <-
      mapply(`[`, last.name[gen.suf], lengths(last.name[gen.suf]))

    double.cp <- sapply(last.name[gen.suf], identical, character(0))
    last.name[gen.suf][double.cp] <- ""
    cp.nome[gen.suf][double.cp] <- unlist(lapply(names[gen.suf][double.cp],
                                                 function(x) paste(x[!grepl("^[A-Z;À-Ý].$", x, perl = TRUE)], collapse = " ")))
    last.name[gen.suf] <-
      paste(last.name[gen.suf], cp.nome[gen.suf], sep = " ")
  }

  if (invert) {
    # Keeping all but the last name
    other.names <-
      sapply(last.name, gsub, replacement = "", x = name[!miss.name],
             perl = TRUE, ignore.case=TRUE)
    if (class(other.names) == "character")
      other.names <- as.matrix(other.names)
    other.names <- stringr::str_trim(diag(other.names))

    #Detecting possible multiple names and removing them
    seps = c(";|&|\\|| e | y | and | und | et ")
    mult.names <- grepl(seps, other.names, perl = TRUE)

    if (any(mult.names))
      other.names[mult.names] <- sapply(strsplit(other.names[mult.names], seps),
                                        function(x) x[1])

    if (initials) {

      #Detecting the possible types of first name: full or abbreviated
      initls <- strsplit(other.names, "")

      inits <- grepl('[A-ZÀ-Ý]', initls, perl = TRUE)
      abrev.inits <- grepl('([a-z;A-Z;à-ý;À-Ý]\\.)([a-z;A-Z;à-ý;À-Ý]\\.)+|([a-z;A-Z;à-ý;À-Ý]\\.)\\s([a-z;A-Z;à-ý;À-Ý]\\.)+',
                           other.names, perl = TRUE)
      combo <- inits | abrev.inits
      inits[abrev.inits] <- FALSE

      #Extracting the initials for each type of first name
      if (any(inits))
        initls[inits] <- lapply(initls[inits],
                                function(x) x[grepl('[A-ZÀ-Ý]', x, perl = TRUE)])

      if (any(abrev.inits))
        initls[abrev.inits] <- lapply(initls[abrev.inits],
                                      function(x) toupper(x[!grepl('\\.|\\s', x, perl = TRUE)]))

      if (any(!combo))
        initls[!combo] <- lapply(strsplit(other.names[!combo], "\\s"),
                                 function(x) toupper(substr(x, 1, 1)))

      #Preparing the initials
      numb.inits <- lengths(gregexpr("[A-Z]", initls, perl = TRUE))
      initls[numb.inits > 1] <-
        sapply(initls[numb.inits > 1], function(x)
          paste0(paste0(x, collapse = "."), "."))
      initls[numb.inits == 1] <-
        paste(initls[numb.inits == 1], ".", sep = "")
      other.names <- unlist(initls)

    }

    # Saving
    name[!miss.name] <- other.names

  } else {

    # Removing the point at the end of the string
    last.name <-
      stringr::str_trim(gsub("\\.$|,$", "", last.name, perl = TRUE))

    # Capitalizing the first letter
    last.name <- capName(last.name)
    #last.name <- stringr::str_to_title(last.name)

    # Saving
    name[!miss.name] <- last.name
  }

  if (any(miss.name))
    name[miss.name] <- noName

  return(name)
}
