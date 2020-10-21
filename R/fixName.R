#' @title Preparing People's Names
#'
#' @description Fix problems and standardize name notation
#'
#' @param x a character string with a name.
#' @param special.char logical. Should special characters be maintained? Default
#'   to FALSE.
#' @param from a character string with the original/current encoding of the
#'   charcater. Argument currenlty not implemented.
#' @param to a character string with the desired/target encoding of the
#'   charcater. Argument currenlty not implemented.
#'
#' @return The character string \code{x} in the standard notation necessary for
#'   further processing.
#'
#' @details The function fixes problems in name notation (e.g. orphan spaces) and
#'   standardized the notation (i.e. Faria Jr. to Faria Junior). Due to common
#'   encoding problems related to Latin names, names are returned without accents
#'   by default. But users can choose between outputs with and without accents
#'   and species characters, using the argument `special.char`.
#'
#' @author Renato A. F. de Lima
#'
#' @importFrom stringr str_trim
#'
#' @export fixName
#'
#' @examples
#'   fixName("J.E.Q. Faria Jr.")
#'   fixName("J.E.Q. Faria Jr.", special.char = TRUE)
#'   fixName("Leitão F°, H.F.")
#'   fixName("Leitão F°, H.F.", special.char = TRUE)
#'   fixName("Gert G. Hatschbach, et al.")
#'   fixName("Karl Emrich & Balduino Rambo")
#'   fixName('F. da S.N. Thomé')
#'   fixName('Pedro L.R.de Morães')
#'
fixName <- function(x,
                    special.char = FALSE,
                    from = "UTF-8",
                    to = "windows-1252//TRANSLIT") {

  nomes <- x

  #Separation between multiple authors
  nomes <- gsub("  ", " ", nomes)
  nomes <- gsub(' & | e | \\| ', "; ", nomes) #### INCLUDE HERE: "et", "and", "und", "y"
  nomes <- gsub("&|\\|", ";", nomes)
  nomes <- gsub("; ;", ";", nomes)
  nomes <- gsub(" ;", ";", nomes)
  nomes <- gsub('^\\;', '', nomes)
  nomes <- gsub('\\( ', '\\(', nomes)
  nomes <- gsub(' \\)', '\\)', nomes)
  nomes <- gsub(' <U+', '; ', nomes)
  nomes <- gsub(" s.n. ", ";", nomes)
  nomes <- gsub("Collector\\(s\\):", "", nomes)
  nomes <- gsub("\\(Coll.", ";", nomes)
  nomes <- gsub(" \\(\\?\\) ", "; ", nomes)

  #Removing unwanted character
  nomes <- gsub("[0-9]", "", nomes)
  nomes <- gsub('\\(\\)|\\( \\)', "", nomes)
  nomes <- gsub('\\[|\\]', "", nomes)
  nomes <- gsub(' - | -|- ', "-", nomes)
  nomes <- gsub('^-- ', "", nomes)
  nomes <- gsub(' --$', "", nomes)
  nomes <- gsub('^-\\.$', 'SemColetor', nomes)
  nomes <- gsub('^-\\. ', '', nomes)
  nomes[grepl('^<', nomes) & grepl('>$', nomes)] <- "EncodingError"
  nomes <- gsub('!', '', nomes)
  nomes <- gsub('^\\* ', '', nomes)

  #Removing "et al." expression
  nomes <- gsub(" et alii| Et alii| Et Alii| et alli| Et alli| et all$|et alii$",
                "", nomes)
  nomes <- gsub(" et\\. al\\.$| et\\.al\\.$| et al\\.$", "", nomes)
  nomes <- gsub(" at al\\.$| etal\\.$| et,al\\.$| et, al\\.$|et. al\\.$",
                "", nomes)
  nomes <- gsub(" et\\. al$| et\\.al$| et\\. al\\.\\.$", "", nomes)
  nomes <- gsub(" et\\. al,\\.$| et\\.a l\\.$| et\\. al,\\.$| et\\. a\\.$",
                "", nomes)
  nomes <- gsub(" et\\.al,\\.$| et\\.al\\.,$", "", nomes)
  nomes <- gsub(" et al $| et al\\. $", "", nomes)
  nomes <- gsub(" el al\\.$| Et al\\.$| Et Al\\.$| Et\\. Al\\.$| at al\\.$| et Al\\.$",
                "", nomes)
  nomes <- gsub(" et al$|et al$", "", nomes)
  nomes <- gsub(" et. al.;$| et. al;$| et. alli;$", "", nomes)

  #Compound names
  nomes <- gsub("Jr\\.|jr\\.| jr$| Jr$", " Júnior", nomes)
  nomes <- gsub(" - Júnior| - Junior", " Júnior", nomes)
  nomes <- gsub(", Júnior,|, Junior,", " Júnior,", nomes)
  nomes <- gsub("-Júnior|-Junior", " Júnior", nomes)
  nomes <- gsub("JAºnior|jAºnior", "Júnior", nomes)
  nomes <- gsub(" F°| Fº", " Filho", nomes)
  nomes <- gsub(' f\\.,', " Filho", nomes)
  nomes <- gsub(" - Filho", " Filho", nomes)
  nomes <- gsub("-Filho", " Filho", nomes)
  nomes <- gsub("- Filho", " Filho", nomes)
  nomes <- gsub(" Filho\\.,", " Filho,", nomes)
  nomes <- gsub(" Sobr°| Sobrº", " Sobrinho", nomes)
  nomes <- gsub(" Sobrinho\\.,| Sobr\\.,", " Sobrinho,", nomes)

  #Formatting names without abbreviation
  nomes <- gsub("     ", " ", nomes)
  nomes <- gsub("    ", " ", nomes)
  nomes <- gsub("   ", " ", nomes)
  nomes <- gsub("  ", " ", nomes)
  nomes <- stringr::str_trim(nomes)
  nomes <- gsub('^\\.|^,', '', nomes)
  nomes <- gsub(',$', '', nomes)
  nomes <- gsub('-$', '', nomes)
  nomes <- stringr::str_trim(nomes)

  #Try to solve encoding problems?
  #nomes <- as.character(iconv(nomes, from= from, to= to))

  #Remove special characters?
  if(special.char == FALSE) {
    unwanted_array <- list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                           'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                           'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='S', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                           'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                           'ö'='o', 'ø'='o', 'ü'='u', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )
    nomes <- chartr(paste(names(unwanted_array), collapse = ''),
                    paste(unwanted_array, collapse = ''),
                    nomes)
  }

  return(nomes)
}
