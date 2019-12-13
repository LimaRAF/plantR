#' @title Format People's Name In TDWG Format
#'
#' @description Fix minor problems on names already provided in the TDWG format.
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
formatName = function(x) {
  if(!grepl("[a-z;A-Z], ", x)) {
    name.correct = x
  } else {
    names = strsplit(x,", |; ")[[1]]
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
    initials = paste(paste(initials, collapse="."), ".", sep="")
    name.correct = paste(lastname, initials,sep=", ")
    name.correct = gsub("\\.\\.\\.",".", name.correct)
    name.correct = gsub("\\.\\.",".", name.correct)
  }
  return(name.correct)
}
