#' @title Format People's Name In TDWG Format
#'
#' @description Solve minor formatting issues of names that are already provided
#'   in the \href{https://www.tdwg.org/}{Biodiversity Information Standards}
#'   (TDWG) format.
#'
#' @param x the character string.
#'
#' @return The character string \code{x} in the fixed TDWG format.
#'
#' @details The function fixes simple problems (e.g. missing points between name
#'   initials) for names provided already in the TDWG format (i.e. last name(s)
#'   + comma + initials).
#'
#'   If the name provided is not in the TDWG format, the function returns the
#'   same input character (i.e. no formatting is performed). In addition, if the
#'   name provided has unusual formatting or if multiple names are provided,
#'   the function may not work properly. So, the output may depend on the input
#'   format and so some level of double-checking may be needed. See examples
#'   below.
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
#' @export formatName
#'
#' @examples
#'   # Simple name
#'   formatName("Gentry, AH")
#'   formatName("GENTRY, AH")
#'   formatName("Gentry, A H")
#'   formatName("Gentry, Alwyn Howard")
#'
#'   # does nothing (not in TDWG format)
#'   formatName("Gentry AH")
#'   formatName("Gentry A.H.")
#'
#'   # Names with generational suffixes
#'   formatName("Leitao Filho, HF")
#'   formatName("Filho Neto, S J")
#'   formatName("Leitao filho, H. F.")
#'
#'   # Compound last name
#'   formatName("Saint-Hilaire, Augustin")
#'   formatName("Ziffer Berger, Richter")
#'   formatName("Saint-hilaire, A.")
#'   formatName("Saint-Hilaire A.")
#'
#'   # Unusual formatting (function won't work always...)
#'   formatName("Sa, Cyl")
#'   # one name, two commas: fails to get all names
#'   formatName("Cesar Sandro, Esteves, F")
#'   # two or more names: output incorrect (combine names of authors)
#'   formatName("Mendonca Filho, C.V.; Neto, F.C.C.")
#'   # two or more names, separeted by comma: output incorrect (combine names of authors)
#'   formatName("A. Alvarez, A. Zamora & V. Huaraca")
#'
formatName <- function(x) {

  if (!grepl("[a-z;A-Z], ", x)) {

    nome <- x

  } else {

    names <- strsplit(x, ", |; ")[[1]]
    lastname <- capName(names[1])
    initials <- strsplit(names[2], "")[[1]]

    if (any(grepl('[A-Z]', initials))) {

      initials <- initials[grepl('[A-Z]', initials)]

    } else {

      if (nchar(initials) == 1) {

        initials <- toupper(initials)

      } else {

        initials <- initials

      }
    }

    initials <- paste(paste(initials, collapse = "."), ".", sep = "")
    nome <- paste(lastname, initials, sep = ", ")
    nome <- gsub("\\.\\.\\.", ".", nome)
    nome <- gsub("\\.\\.", ".", nome)
  }

  return(nome)
}
