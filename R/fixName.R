#' @title Preparing People's Names
#'
#' @description Standardize name notation
#'
#' @param nomes a character string or a vector with names.
#' @param special.char logical. Should special characters be maintained? Default
#'   to FALSE.
#' @param sep.out a character string with the symbol separating multiple names
#'   in the output string. Defaults to "; ".
#'
#' @return The character string \code{x} in the standard notation to facilitate
#'   further data processing.
#'
#' @details The function fixes small problems in name notation (e.g. orphan
#'   spaces), standardize the separation between multiple authors and between
#'   initials and/or prepositions within the same name. It also standardize the
#'   notation of some compound names (i.e. Faria Jr. to Faria Junior). In
#'   addition, the function removes numbers, some unwanted expressions (e.g. 'et
#'   al.') and symbols (e.g. ? or !).
#'
#'   Due to common encoding problems related to Latin names, names are returned
#'   without accents by default. But users can choose between outputs with and
#'   without accents and species characters, by setting the argument
#'   `special.char` to TRUE.
#'
#' @author Renato A. F. de Lima
#'
#' @importFrom textclean replace_non_ascii
#' @importFrom stringr str_trim
#'
#' @examples
#'   fixName("J.E.Q. Faria Jr.")
#'   fixName("J.E.Q. Faria Jr.", special.char = TRUE)
#'   fixName("Leitão F°, H.F.")
#'   fixName("Leitão F°, H.F.", special.char = TRUE)
#'   fixName("Gert G. Hatschbach, et al.")
#'   fixName("Karl Emrich & Balduino Rambo")
#'   fixName("Karl Emrich & Balduino Rambo", sep.out = " | ")
#'   fixName('( Karl) Emrich ;(Balduino ) Rambo')
#'   fixName('F. da S.N. Thomé')
#'   fixName('F.daS.N.Thomé')
#'   fixName('Pedro L.R.de Morães')
#'   fixName('Pedro L.R.de Morães (30/4/1998)')
#'
#' @export fixName
#'
fixName <- function(nomes, special.char = FALSE, sep.out = "; ") {

  #Defining the temporary separator
  sep <- "__"

  #Separation between multiple authors
  nomes <- gsub("(?<=[A-Z])\\.(?=[a-z])", "\\1. ", nomes, perl = TRUE)
  nomes <- gsub("(?<=[A-Z])\\.([A-Z])([a-z])", ". \\1\\2", nomes, perl = TRUE)
  nomes <- gsub("(?<=[a-z])([A-Z])\\.", " \\1.", nomes, perl = TRUE)
  nomes <- gsub("\\s+", " ", nomes, perl = TRUE)
  nomes <- gsub(' & | e | \\| | and | und | y ', sep, nomes, perl = TRUE)
  nomes <- gsub("&|\\|", sep, nomes, perl = TRUE)
  nomes <- gsub("; ;", sep, nomes, fixed = TRUE)
  nomes <- gsub(" ;", sep, nomes, fixed = TRUE)
  nomes <- gsub('\\( ', '\\(', nomes, perl = TRUE)
  nomes <- gsub(' \\)', '\\)', nomes, perl = TRUE)
  nomes <- gsub(" s.n. ", sep, nomes, perl = TRUE)
  nomes <- gsub("Collector\\(s\\):", "", nomes, perl = TRUE)
  nomes <- gsub("\\(Coll.", sep, nomes, perl = TRUE)
  nomes <- gsub(" \\(\\?\\) ", sep, nomes, perl = TRUE)

  #Removing unwanted character
  nomes <- gsub("[0-9]", "", nomes, perl = TRUE)
  nomes <- gsub('!', '', nomes, fixed = TRUE)
  nomes <- gsub('\\(\\)|\\( \\)', "", nomes, perl = TRUE)
  nomes <- gsub('\\[|\\]', "", nomes, perl = TRUE)
  nomes <- gsub(' - | -|- ', "-", nomes, perl = TRUE)
  # nomes[grepl('^<', nomes, perl = TRUE) &
  #         grepl('>$', nomes, perl = TRUE)] <- "EncodingError"
  nomes <- gsub('^\\* ', '', nomes, perl = TRUE)

  #Removing "et al." expression
  nomes <- gsub(" et alii| et alli| et all$", "", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub("et alii$", "", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(" et\\. al\\.$| et\\.al\\.$| et al\\.$", "", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(" at al\\.$| etal\\.$| et,al\\.$| et, al\\.$|et\\. al\\.$",
                "", nomes, perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(" et\\. al$| et\\.al$| et\\. al\\.\\.$", "", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(" et\\. al,\\.$| et\\.a l\\.$| et\\. a\\.$",
                "", nomes, perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(" et\\.al,\\.$| et\\.al\\.,$", "", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(" et al $| et al\\. $", "", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(" el al\\.$", "", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(" et al$|et al$", "", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(" et\\. al\\.;$| et\\. al;$| et\\. alli;$", "", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(' et ', sep, nomes, fixed = TRUE)

  #Compound names
  nomes <- gsub(" jr\\.| jr$", " Júnior", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(" - júnior| - junior", " Júnior", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(", júnior,|, junior,", " Júnior,", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub("-júnior|-junior", " Júnior", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub("jAºnior", "Júnior", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(" F°| Fº", " Filho", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(' f\\.,', " Filho", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(" - filho", " Filho", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub("-filho", " Filho", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub("- filho", " Filho", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(" Filho\\.,", " Filho,", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(" Sobr°| Sobrº", " Sobrinho", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(" Sobrinho\\.,| Sobr\\.,", " Sobrinho,", nomes,
                perl = TRUE, ignore.case = TRUE)

  #Final formatting
  nomes <- gsub("\\s+", " ", nomes, perl = TRUE)
  nomes <- stringr::str_trim(nomes)
  nomes <- gsub("^-+|-+$", "", nomes, perl = TRUE)
  nomes <- gsub("^\\.+|\\.+$", "", nomes, perl = TRUE)
  nomes <- gsub("^,+|,+$", "", nomes, perl = TRUE)
  nomes <- gsub("^;+|;+$", "", nomes, perl = TRUE)
  nomes <- gsub("--", "", nomes, fixed = TRUE)
  nomes <- gsub("//", "", nomes, fixed = TRUE)
  nomes <- gsub("\\.\\.", "", nomes, perl = TRUE)
  nomes <- gsub("\\(\\)|\\(-\\)|\\(/\\)", "", nomes, perl = TRUE)
  nomes <- stringr::str_trim(nomes)
  nomes[nomes %in% c("")] <- NA_character_
  nomes <- gsub(sep, sep.out, nomes, fixed = TRUE)

  #Remove special characters?
  if (special.char == FALSE) {
    nomes <- textclean::replace_non_ascii(nomes)
  }

  return(nomes)
}
