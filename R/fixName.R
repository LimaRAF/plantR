#' @title Preparing People's Names
#'
#' @description Standardize name notation
#'
#' @param nomes a character string or a vector with names.
#'   to FALSE.
#' @param sep.in a vector of the symbols separating multiple names. Default to:
#'   ";", "&", "|", " e ", " y ", " and ", " und ", and " et ".
#' @param sep.out a character string with the symbol separating multiple names
#'   in the output string. Defaults to "|".
#' @param special.char logical. Should special characters be maintained? Default
#'   to FALSE.
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
#'   The function was created to deal with people's names, so input separators
#'   for multiple names composed only by letters should be surrounded by spaces.
#'   For non-alphabetic characters (e.g. semi-colons, ampersand) the function are
#'   taken independently of the presence of spaces nearby.
#'
#'   Due to common encoding problems related to Latin characters, names are
#'   returned without accents by default. But users can choose between outputs
#'   with and without accents and species characters, by setting the argument
#'   `special.char` to TRUE.
#'
#' @author Renato A. F. de Lima & Hans ter Steege
#'
#' @importFrom stringr str_trim
#'
#' @encoding UTF-8
#'
#' @examples
#'   names <- c("J.E.Q. Faria Jr.",
#'   "Leitão F°, H.F.", "Gert G. Hatschbach, et al.",
#'   "Karl Emrich & Balduino Rambo",
#'   '( Karl) Emrich ;(Balduino ) Rambo', "F.daS.N.Thomé",
#'   'F. da S.N. Thomé', 'Pedro L.R.de Moraes (30/4/1998)')
#'   Encoding(names) <- "latin1"
#'   names
#'
#'   fixName(names)
#'   fixName(names, special.char = TRUE)
#'   fixName(names, sep.out = " | ")
#'
#'
#' @export fixName
#'
fixName <- function(nomes,
                    sep.in = c(";","&","|"," e "," y "," and "," und "," et "),
                    sep.out = "|",
                    special.char = FALSE) {
  #Defining the input and temporary separators
  sep.in2 <- sep.in[grepl('[[:alpha:]]', sep.in) & !grepl(' et ', sep.in)]
  sep.in1 <- sep.in[!grepl('[[:alpha:]]', sep.in)]
  sep.in1 <- gsub("(\\|)", "\\\\\\1", sep.in1, perl = TRUE)
  sep.in1 <- gsub("(\\+)", "\\\\\\1", sep.in1, perl = TRUE)
  sep.in1 <- gsub("(\\$)", "\\\\\\1", sep.in1, perl = TRUE)
  sep.in1 <- gsub("(\\.)", "\\\\\\1", sep.in1, perl = TRUE)
  sep.in1 <- gsub("(\\?)", "\\\\\\1", sep.in1, perl = TRUE)
  sep.in1 <- gsub("(\\*)", "\\\\\\1", sep.in1, perl = TRUE)
  sep.in3 <- unique(c(sep.in1, sep.in2))

  if (length(sep.in3) == 0)
    stop("Please provide valid separators between multiple authors")
  sep.other <- paste(sep.in3, collapse = "|")
  sep.et <- ifelse(" et " %in% sep.in, TRUE, FALSE)
  sep0 <- "__"

  #Separation between names/initials
  nomes <- gsub("(?<=\\p{Lu})\\.(?=\\p{Ll})", "\\1. ",
                nomes, perl = TRUE)
  nomes <- gsub("(?<=\\p{Lu})\\.(\\p{Lu})(\\p{Ll})", ". \\1\\2",
                nomes, perl = TRUE)
  nomes <- gsub("(?<=\\p{Ll})(\\p{Lu})\\.", " \\1.",
                nomes, perl = TRUE)
  nomes <- gsub("(?<=\\p{Ll}),(?=\\p{L})", "\\1, \\2",
                nomes, perl = TRUE)
  # nomes <- gsub("(?<=[A-ZÀ-Ý])\\.(?=[a-zà-ý])", "\\1. ",
  #               nomes, perl = TRUE)
  # nomes <- gsub("(?<=[A-ZÀ-Ý])\\.([A-ZÀ-Ý])([a-zà-ý])", ". \\1\\2",
  #               nomes, perl = TRUE)
  # nomes <- gsub("(?<=[a-zà-ý])([A-ZÀ-Ý])\\.", " \\1.",
  #               nomes, perl = TRUE)
  # nomes <- gsub("(?<=[a-zà-ý]),(?=[a-zà-ýA-ZÀ-Ý])", "\\1, \\2",
  #               nomes, perl = TRUE)
  nomes <- gsub("\\s+", " ", nomes, perl = TRUE)

  #Separation between multiple authors
  nomes <- gsub(sep.other, sep0, nomes, perl = TRUE)
  nomes <- gsub("____", sep0, nomes, fixed = TRUE)
  nomes <- gsub("^__|__$", "", nomes, perl = TRUE)
  nomes <- gsub('\\( ', '(', nomes, perl = TRUE)
  nomes <- gsub(' \\)', ')', nomes, perl = TRUE)
  nomes <- gsub('\\[ ', '[', nomes, perl = TRUE)
  nomes <- gsub(' \\]', ']', nomes, perl = TRUE)
  nomes <- gsub(" s.n. ", sep0, nomes, perl = TRUE)
  nomes <- gsub("Collector\\(s\\):", "", nomes, perl = TRUE)
  nomes <- gsub("\\(Coll.", "", nomes, perl = TRUE)
  nomes <- gsub('Data:|Date:', "", nomes, perl = TRUE, ignore.case = TRUE)
  nomes <- gsub('^det\\. by', "", nomes, perl = TRUE, ignore.case = TRUE)
  nomes <- gsub('^det\\.:|^det:', "", nomes, perl = TRUE, ignore.case = TRUE)
  nomes <- gsub('^det\\. ', "", nomes, perl = TRUE, ignore.case = TRUE)
  nomes <- gsub('^det\\.', "", nomes, perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(" \\(\\?\\) ", "", nomes, perl = TRUE)
  nomes <- gsub("([[:alpha:]])(\\()", "\\1 \\2", nomes, perl = TRUE)

  #Removing unwanted character
  nomes <- gsub("[0-9]", "", nomes, perl = TRUE)
  nomes <- gsub('!', '', nomes, fixed = TRUE)
  nomes <- gsub(' - | -|- ', "-", nomes, perl = TRUE)
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
  if (sep.et)
    nomes <- gsub(' et ', sep0, nomes, fixed = TRUE)

  #Compound names
  nomes <- gsub(" jr\\.|Jr\\.", " Junior", nomes,
                perl = TRUE)
  nomes <- gsub(" Jr$", " Junior", nomes,
                perl = TRUE)
  nomes <- gsub(" - j\u00fanior", " J\u00fanior", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(", junior,", " Junior,", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(", j\u00fanior,", " J\u00fanior,", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(" - junior", " Junior", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(" - j\u00fanior", " J\u00fanior", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub("-junior", " Junior", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub("-j\u00fanior", " J\u00fanior", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub("j\u00fanior", "J\u00fanior", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(" F\u00ba| F\u00b0", " Filho", nomes,
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
  nomes <- gsub(" Sobr\u00ba| Sobr\u00b0", " Sobrinho", nomes,
                perl = TRUE, ignore.case = TRUE)
  nomes <- gsub(" Sobrinho\\.,| Sobr\\.,", " Sobrinho,", nomes,
                perl = TRUE, ignore.case = TRUE)

  #Final formatting
  nomes <- gsub("\\s+", " ", nomes, perl = TRUE)
  nomes <- stringr::str_trim(nomes)
  nomes <- gsub("^-+|-+$", "", nomes, perl = TRUE)
  nomes <- gsub("^\\.+|\\.\\.+$", "", nomes, perl = TRUE)
  nomes <- gsub("^,+|,+$", "", nomes, perl = TRUE)
  nomes <- gsub("^;+|;+$", "", nomes, perl = TRUE)
  nomes <- gsub("--", "", nomes, fixed = TRUE)
  nomes <- gsub("//", "", nomes, fixed = TRUE)
  nomes <- gsub("\\.\\.", "", nomes, perl = TRUE)
  nomes <- gsub("\\(\\)", "", nomes, perl = TRUE)
  nomes <- gsub('\\[\\]|\\[\\s+\\]|\\[-\\]|\\[/\\]', "", nomes, perl = TRUE)
  nomes <- gsub("^/+|/+$", "", nomes, perl = TRUE)
  nomes <- stringr::str_trim(nomes)
  nomes[nomes %in% c("")] <- NA_character_

  #Replacing the temporary separator by sep.out
  nomes <- gsub(" __ ", sep0, nomes, fixed = TRUE)
  nomes <- gsub("__ ", sep0, nomes, fixed = TRUE)
  nomes <- gsub(" __", sep0, nomes, fixed = TRUE)
  nomes <- gsub(sep0, sep.out, nomes, fixed = TRUE)

  #Remove special (latin-1) characters?
  if (special.char == FALSE)
    nomes <- rmLatin(nomes)

  return(nomes)
}
