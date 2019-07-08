#' @title Fix People's Name In TDWG Format
#'
#' @description Fix problems on names already provided in the TDWG format.
#'
#' @param x the character string.
#'
#' @return the character string \code{x} in the fixed TDWG format. If
#' \code{x} is not in the TDWG format, the function returns \code{x}.
#'
#' @details The function fixes problems (e.g. missing points between name initials)
#' for names provided already in the TDWG format (i.e. last name + comma + initials).
#'
#' @author Lima, R.A.F.
#'
#' @references Willemse, L.P., van Welzen, P.C. & Mols, J.B. (2008). Standardisation in data-entry across databases: Avoiding Babylonian confusion. Taxon 57(2): 343-345.
#'
#' @export fixName
#'
#' @examples
#' # Simple name
#'   fixName("Gentry, AH")
#'   fixName("GENTRY, AH")
#'   fixName("Gentry, A H")
#'   fixName("Gentry, Alwyn Howard")
#'   fixName("Gentry AH") # does nothing (not in TDWG format)
#'   fixName("Gentry A.H.") # does nothing (not in TDWG format)
#' # Name with generational suffixes
#'   fixName("Leitao Filho, HF")
#'   fixName("Filho Neto, S J")
#'   fixName("Leitao filho, H. F.")
#' # Compound last name
#'   fixName("Saint-Hilaire, Augustin")
#'   fixName("Ziffer Berger, Richter")
#'   fixName("Saint-hilaire, A.")
#'   fixName("Saint-Hilaire A.")
#' # Unusual formatting (function won't always work)
#'   fixName("Sa, Cyl") # small last name, with comma: output incorrect (inverted)
#'   fixName("Cesar Sandro, Esteves, F") # one name, two commas: fails to get all names
#'   fixName("Mendonca Filho, C.V.; Neto, F.C.C.") # two or more names: output incorrect (combine names of authors)
#'   fixName("A. Alvarez, A. Zamora & V. Huaraca") # two or more names, separeted by comma: output incorrect (combine names of authors)
fixName = function(x) {
  if(!grepl("[a-z;A-Z], ", x)) {
    name.correct = x
  } else {
    names = strsplit(x,", ")[[1]]
    lastname = capName(names[1])
    initials = strsplit(names[2],"")[[1]]
    if(any(grepl('[A-Z]',initials))) {
      initials = initials[grepl('[A-Z]',initials)]
    } else {
      if(nchar(initials)==1) {
        initials = toupper(initials)
      } else {
        initials = initials
      }
    }
    initials = paste(paste(initials,collapse="."),".",sep="")
    name.correct = paste(lastname,initials,sep=", ")
    name.correct = gsub("\\.\\.\\.",".",name.correct)
    name.correct = gsub("\\.\\.",".",name.correct)
  }
  return(name.correct)
}
