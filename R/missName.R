#' @title Homogenize Notation For Missing Name
#'
#' @description Standardize the different notation of missing collector or identifier names
#'
#' @param x the character string
#' @param type type of ...
#' @param noName standard for missing data in Name
#'
#' @return the character string \code{x} in the fixed TDWG format.
#'
#' @author Lima, R.A.F.
#'
#' @references
#'
#' @export missName
#'
#' @examples
#' # Error: need to chose between 'collector' or 'identificator'
#   missName(c("Gentry, AH", "s/col.", NA))
#'  missName(c("Gentry, AH", "s/col.", NA, "?", "s/c", "s/coletor"),
#'            type = "collector",
#'            noName = "s./c.")
#'
#'  missName(c("Gentry, AH", "s/col.", NA, "?", "s/c", "s/coletor"),
#'          type = "collector",
#'          noName = "Anonymous")
#'  missName(c("Gentry, AH", "s/det.","s/det","s/d","Determiner unknown"),
#'           type = "identificator",
#'           noName = "Anonymous")
missName = function(x,
                    type = NULL,
                    noName = "Anonymous") {
  # Checking if all arguments are provided
  if(is.null(noName)) { stop("Please provide a character to replace missing names") }
  if(is.null(type) | all(!type %in% c("collector","coletor","colector","identificator","identificador","determinador"))) { stop("Please chose between 'collector' or 'identificator' to replace missing names") }

  nomes = x

  if(any(type %in% c("collector","coletor","colector"))) {   #No collector's name
    nomes[is.na(nomes) | nomes %in% ""] = noName
    nomes[nomes %in% c("s/col.", "s/col", "s/c", "s/coletor", " sem col.", "s.col.")] = noName
    nomes[nomes %in% c("Collector unspecified", "Collector unknown")] = noName
    nomes[nomes %in% c("?")] = noName
    nomes = gsub('^Disponible, N\\.$|^Disponivel, N\\.$|^Available, N\\.$', noName, nomes)
    nomes = gsub('^Sin$', noName, nomes)
    nomes = gsub('Anonymous', noName, nomes)
    nomes = gsub("NANA", noName, nomes)
  }

  if(any(type %in% c("identificator","identificador","determinador"))) {   #No identificator's name
    nomes[is.na(nomes) | nomes %in% ""] = noName
    nomes[nomes %in% c("s/det.","s/det","s/d","s/determinador"," sem det.","s.det.")] = noName
    nomes[nomes %in% c("Determiner unspecified", "Determiner unknown")] = noName
    nomes[nomes %in% c("?")] = noName
    nomes = gsub('^Disponible, N\\.$|^Disponivel, N\\.$|^Available, N\\.$', noName, nomes)
    nomes = gsub('^Sin$', noName, nomes)
    nomes = gsub('Anonymous', noName, nomes)
    nomes = gsub("NANA", noName, nomes)
  }

  return(nomes)
}
