#' @title Format People's Name In TDWG Format
#'
#' @description Solve minor formatting issues of names that are already provided
#'   in the \href{https://www.tdwg.org/}{Biodiversity Information Standards}
#'   (TDWG) format.
#'
#' @param x the character string or vector.
#' @param sep character. Separator of the name format. Default to ", ".
#' @param format character. Output name format. The default ("last_init")
#' is the TDWG standard, but the inverse format can also be used ("init_last").
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
#' @export prepName
#'
#' @examples
#'   # Simple name
#'   prepName("Gentry, AH")
#'   prepName("GENTRY, A H")
#'   prepName("Gentry, Alwyn Howard")
#'
#'   # does nothing (not in TDWG format)
#'   prepName("Gentry AH")
#'   prepName("Gentry A.H.")
#'
#'   # Names with generational suffixes
#'   prepName("Leitao Filho, HF")
#'   prepName("Filho Neto, S J")
#'   prepName("Leitao filho, H. F.")
#'
#'   # Compound last name
#'   prepName("Saint-Hilaire, Augustin")
#'   prepName("Ziffer Berger, Richter")
#'   prepName("Saint-hilaire, a.")
#'
#'   # Multiple names
#'   names <- c("Gentry, AH", "Gentry A.H.",
#'   "Leitao filho, H. F.", "Saint-Hilaire, a")
#'   prepName(names)
#'   prepName(names, format = "init_last")
#'
#'   # Unusual formatting (function won't work always...)
#'   prepName("Sa, Cyl")
#'   # one name, two commas: fails to get all names
#'   prepName("Cesar Sandro, Esteves, F")
#'   # two or more names: output incorrect (combine names of authors)
#'   prepName("Mendonca Filho, C.V.; Neto, F.C.C.")
#'   # two or more names, separated by comma: output incorrect (combine names of authors)
#'   prepName("A. Alvarez, A. Zamora & V. Huaraca")
#'
prepName <- function(x, sep = ", ", format = "last_init") {

  patt <- paste0("[a-z;A-Z]", sep)
  comma <- grepl(patt, x, perl = TRUE)

  if (any(comma)) {

    split <- strsplit(x[comma], sep, perl = TRUE)
    split <- t(sapply(split, `length<-`, 2))

    lastname <- capName(split[,1])
    initials <- strsplit(split[,2], "")

    inits <- grepl('[A-Z]', initials, perl = TRUE)

    if (any(inits))
      initials[inits] <- lapply(initials[inits], function(x) x[grepl('[A-Z]', x, perl = TRUE)])
    if (any(!inits))
      initials[!inits] <- lapply(initials[!inits], function(x) toupper(head(x, 1)))

    numb.inits <- lengths(gregexpr("[A-Z]", initials, perl = TRUE))
    initials[numb.inits > 1] <-
      sapply(initials[numb.inits > 1], function(x)
        paste0(paste0(x, collapse = "."), "."))
    initials[numb.inits == 1] <-
      paste(initials[numb.inits == 1], ".", sep = "")
    split[,2] <- unlist(initials)

    if(format == "last_init")
      names <- paste(lastname, initials, sep = sep)
    if(format == "init_last")
      names <- paste(initials, lastname, sep = " ")

    nomes <- x
    nomes[comma] <- names

    #nomes <- gsub("\\.\\.\\.", ".", nomes, perl = TRUE)
    #nomes <- gsub("\\.\\.", ".", nomes, perl = TRUE)

  } else {

    nomes <- x

  }

  return(nomes)
}
