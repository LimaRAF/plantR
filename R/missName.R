#' @title Homogenize Notation For Missing Name
#'
#' @description Standardize the different notation of missing collector or
#'   identifier names associated with biological records.
#'
#' @param x the character string or vector with names.
#' @param type type of name: 'collector' or 'determiner'.
#' @param noName standard notation for missing names. Default to "Anonymous".
#'
#' @return the character string \code{x} in the standard notation for missing
#'   names.
#'
#' @author Renato A. F. de Lima
#'
#' @references
#' Conn, Barry J. (ed.) (1996). HISPID 3 - Herbarium Information Standards and
#'   Protocols for Interchange of Data. Herbarium Information Systems Committee'
#'   (HISCOM). https://www.tdwg.org/standards/hispid3/
#'
#' @export missName
#'
#' @examples
#' # Needs to chose between 'collector' or 'identificator'
#'  missName(c("Gentry, AH", "s/col.", NA, "?", "s/c", "s/coletor"),
#'            type = "collector",
#'            noName = "s./c.")
#'
#'  missName(c("Gentry, AH", "s/col.", NA, "?", "s/c", "s/coletor"),
#'          type = "collector")
#'
#'  missName(c("Gentry, AH", "s/det.", "s/det", "s/d", "Determiner unknown"),
#'           type = "determiner")
#'
missName <- function(x, type = NULL, noName = "Anonymous") {

  # Checking if all arguments are provided
  if (is.null(noName))
    stop("Please provide a character to replace missing names")

  if (is.null(type) | all(!type %in% c("collector","coletor","colector","determiner",
                                       "identificator","identificador","determinador")))
    stop("Please chose between 'collector' or 'determiner' to replace missing names")

  nomes <- x

  if (any(type %in% c("collector", "coletor", "colector"))) {   #No collector's name

    nomes[nomes %in% c("", " ", NA)] <- noName
    busca <- missColls
    nomes[tolower(nomes) %in% busca] <- noName
    nomes <- gsub('anonymous', noName, nomes, perl = TRUE, ignore.case = TRUE)
    nomes <- gsub("NANA", noName, nomes, fixed = TRUE)

  }

  if (any(type %in% c("determiner", "identificator", "identificador", "determinador"))) {   #No identificator's name

    nomes[is.na(nomes) | nomes %in% ""] <- noName
    busca <- missDets
    nomes[tolower(nomes) %in% busca] <- noName
    nomes <- gsub('anonymous', noName, nomes, perl = TRUE, ignore.case = TRUE)
    nomes <- gsub("NANA", noName, nomes, fixed = TRUE)

  }

  return(nomes)
}
