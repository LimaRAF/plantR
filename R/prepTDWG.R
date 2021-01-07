#' @title Format People's Name In TDWG Format
#'
#' @description Solve minor formatting issues of names that are already provided
#'   in the \href{https://www.tdwg.org/}{Biodiversity Information Standards}
#'   (TDWG) format.
#'
#' @param x the character string or vector.
#' @param sep character. Separator of the name format. Default to ", ".
#' @param format character. Output name format. The default ("last_init") is the
#'   similar to the TDWG standard, but the inverse format can also be used
#'   ("init_last").
#' @param get.prep logical. Should last name prepositions be included? Default to
#' FALSE.
#'
#' @return The character string \code{x} in the fixed TDWG format.
#'
#' @details The function fixes simple problems (e.g. missing points between name
#'   initials) for names provided already in a the following format: last name(s)
#'   + comma + name or initials.
#'
#'   If the name provided is not separated by commas (the default separator),
#'   the function returns the same input character (i.e. no formatting is
#'   performed). In case of vector, only the names separated by commas will
#'   be edited.
#'
#'   If the name provided has unusual formatting or if multiple names are
#'   provided within the same string, the function may not work properly. So,
#'   the output may depend on the input format and some level of
#'   double-checking may be necessary. See examples below.
#'
#' @author Renato A. F. de Lima
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
#' @export prepTDWG
#'
#' @examples
#'   # Simple name
#'   prepTDWG("Gentry, A.H.") # supposed not to change
#'   prepTDWG("Gentry, AH")
#'   prepTDWG("GENTRY, A H")
#'   prepTDWG("Gentry, Alwyn")
#'   prepTDWG("Gentry, Alwyn Howard")
#'   prepTDWG("gentry, alwyn howard")
#'   prepTDWG("Gentry, A.h.")
#'   prepTDWG("gentry, a.")
#'   prepTDWG("gentry, a.h.")
#'   prepTDWG("gentry, a. h.")
#'   prepTDWG("gentry, a")
#'
#'   # Name not in TDWG format (no comma; does nothing)
#'   prepTDWG("Gentry AH")
#'   prepTDWG("Dardano de Andrade-Lima")
#'
#'   # Names with generational suffixes
#'   prepTDWG("Leitao Filho, HF")
#'   prepTDWG("Filho Neto, S J")
#'   prepTDWG("Leitao filho, H. F.")
#'
#'   # Compound last name
#'   prepTDWG("Saint-Hilaire, Augustin")
#'   prepTDWG("Ziffer Berger, Richter")
#'   prepTDWG("Saint-hilaire, a.")
#'
#'   # Names with prepositions
#'   prepTDWG("Silva, Maria A. Pereira da")
#'   prepTDWG("Silva, Maria A. Pereira Da")
#'   prepTDWG("da Silva, Maria A. Pereira")
#'   prepTDWG("Silva, Maria A. Pereira da", get.prep = TRUE)
#'   prepTDWG("Silva, Maria A. Pereira Da", get.prep = TRUE)
#'   prepTDWG("da Silva, Maria A. Pereira", get.prep = TRUE)
#'
#'   # Multiple names
#'   names <- c("Gentry, AH", "Gentry A.H.",
#'   "Leitao filho, H. F.", "Saint-Hilaire, a",
#'   "gentry, alwyn howard", "da Silva, Maria A.")
#'   prepTDWG(names)
#'   prepTDWG(names, format = "init_last")
#'   prepTDWG(names, format = "init_last", get.prep = TRUE)
#'
#'   # Unusual formatting (function won't work always...)
#'   # one name, two commas: fails to get all names
#'   prepTDWG("Cesar Sandro, Esteves, F")
#'   # two or more names: output incorrect (combine names of authors)
#'   prepTDWG("Mendonca Filho, C.V.; Neto, F.C.C.")
#'   # two or more names, separated by comma: output incorrect (combine names of authors)
#'   prepTDWG("A. Alvarez, A. Zamora & V. Huaraca")
#'
prepTDWG <- function(x, sep = ", ", format = "last_init", get.prep = FALSE) {

  patt <- paste0("[a-z;A-Z]", sep)
  comma <- grepl(patt, x, perl = TRUE)

  if (any(comma)) {

    split <- strsplit(x[comma], sep, perl = TRUE)
    split <- t(sapply(split, `length<-`, 2))
    split <- getPrep(split, rm.prep = !get.prep)

    lastname <- capName(split[,1])
    initials <- strsplit(split[,2], "")

    inits <- grepl('[A-Z]', initials, perl = TRUE)
    abrev.inits <- grepl('([a-z;A-Z]\\.)([a-z;A-Z]\\.)+|([a-z;A-Z]\\.)\\s([a-z;A-Z]\\.)+', split[,2], perl = TRUE)
    combo <- inits | abrev.inits
    inits[abrev.inits] <- FALSE

    if (any(inits))
      initials[inits] <- lapply(initials[inits],
                                function(x) x[grepl('[A-Z]', x, perl = TRUE)])

    if (any(abrev.inits))
      initials[abrev.inits] <- lapply(initials[abrev.inits],
                                      function(x) toupper(x[!grepl('\\.', x, perl = TRUE)]))

    if (any(!combo))
      initials[!combo] <- lapply(strsplit(split[,2, drop = FALSE][!combo], "\\s"),
                                 function(x) toupper(substr(x, 1, 1)))

    numb.inits <- lengths(gregexpr("[A-Z]", initials, perl = TRUE))
    initials[numb.inits > 1] <-
      sapply(initials[numb.inits > 1], function(x)
        paste0(paste0(x, collapse = "."), "."))
    initials[numb.inits == 1] <-
      paste(initials[numb.inits == 1], ".", sep = "")
    initials <- unlist(initials)

    if(format == "last_init") {
      names <- paste(lastname, initials, sep = sep)
      if (get.prep) {
        names <- paste(names, split[,3], sep = " ")
        names <- gsub(" $", "", names, perl = TRUE)
      }

    }

    if(format == "init_last") {
      if (get.prep) {
        names <- paste(initials, split[,3], lastname, sep = " ")
        names <- gsub("\\s\\s+", " ", names, perl = TRUE)
      } else {
        names <- paste(initials, lastname, sep = " ")
      }

    }

    names <- gsub("\\.\\s\\.", ".", names, perl = TRUE)
    nomes <- x
    nomes[comma] <- names

  } else {

    nomes <- x

  }

  return(nomes)
}
