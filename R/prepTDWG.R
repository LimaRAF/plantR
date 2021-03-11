#' @title Format People's Name
#'
#' @description Convert people's names to different formats (i.e. Last Name,
#'   First Names(s) or First Name(s), Last Name) with or without last name
#'   prepositions. The default name format is the one suggested in the
#'   \href{https://www.tdwg.org/}{Biodiversity Information Standards} (TDWG)
#'   format.
#'
#' @param x the character string or vector containing the names.
#' @param sep character. Input and output name separator. Default to ", ".
#' @param format character. Output name format. The default is "last_init".
#' @param get.prep logical. Should last name prepositions be included? Default to
#' FALSE.
#' @param get.initials logical. Should last name prepositions be included? Default to
#' TRUE.
#'
#' @return The character string \code{x} in the standardized format.
#'
#' @details The default name format follows the one suggested by the
#'   \href{https://www.tdwg.org/}{TDWG}, which is: Last name, followed by a
#'   comma and then the initials, separated by points (e.g. Hatschbach, G.G.).
#'
#'   The functions uses internally another __plantR__ function: `lastName()`.
#'   So, it assumes that people last names are the ones provided at the end of
#'   the name string or preceeding the name separator (i.e. comma), if present.
#'
#'   The function deals with simples last names, as well as with compound
#'   last names and last names with common name prefixes or prepositions (e.g. de,
#'   dos, van, ter, ...). By default, these prefixes and prepositions are
#'   removed, but they can be returned if the argument `get.prep` is set to
#'   TRUE.
#'
#'   The function asumes that all names containing separators (default to a comma) are in
#'   the format suggested by TDWG. But even for those cases, the function fixes
#'   simple problems (e.g. missing points between name initials).
#'
#'   If only one name is given, the fucntion return the same name with the first
#'   letter capitalized.
#'
#'   The function output it is relatively stable regarding the input format,
#'   lower/uppercasing and spacing. But if the name provided has unusual
#'   formatting or if names for mutltiple people are provided within the same
#'   string, the function may not work properly. So, the output may depend on
#'   the input format and some level of double-checking may be necessary. See
#'   examples below.
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
#' @seealso
#'  \link[plantR]{lastName}, \link[plantR]{getPrep} and \link[plantR]{getInit}.
#'
#' @export prepTDWG
#'
#' @examples
#'   # Single names
#'   prepTDWG("gentry")
#'   prepTDWG("GENTRY")
#'
#'   # Simple names
#'   prepTDWG("Alwyn Howard Gentry")
#'   prepTDWG("Alwyn H. Gentry")
#'   prepTDWG("A.H. Gentry")
#'   prepTDWG("A H Gentry")
#'   prepTDWG("Gentry, Alwyn Howard")
#'   prepTDWG("Gentry, AH")
#'   prepTDWG("Gentry AH")
#'   prepTDWG("GENTRY, A H")
#'   prepTDWG("gentry, alwyn howard")
#'   prepTDWG("gentry, a.h.")
#'   prepTDWG("gentry, a. h.")
#'
#'   # Name with prepositions
#'   prepTDWG("Carl F. P. von Martius")
#'   prepTDWG("Carl F. P. von Martius", get.prep = TRUE)
#'
#'   # Names with generational suffixes
#'   prepTDWG("Hermogenes de Freitas Leitao Filho")
#'   prepTDWG("H.F. Leitao Filho")
#'   prepTDWG("Leitao Filho, HF")
#'   prepTDWG("Leitao filho, H. F.")
#'
#'   # Compound last name
#'   prepTDWG("Augustin Saint-Hilaire")
#'   prepTDWG("A. Saint-Hilaire")
#'   prepTDWG("Saint-Hilaire, Augustin")
#'
#'   # Other formats
#'   prepTDWG("John MacDonald")
#'   prepTDWG("John McDonald")
#'   prepTDWG("John O'Brien")
#'
#'   # Multiple names, different settings
#'   names <- c("Gentry, AH", "Gentry A.H.",
#'   "Carl F. P. von Martius","Leitao filho, H. de F.",
#'   "Auguste de Saint-Hilaire", "John O'Reilly")
#'   prepTDWG(names)
#'   prepTDWG(names, format = "init_last")
#'   prepTDWG(names, format = "init_last", get.prep = TRUE)
#'   prepTDWG(names, get.prep = TRUE, format = "prep_last_init")
#'   prepTDWG(names, get.prep = TRUE, format = "prep_last_init", get.initials = FALSE)
#'
#'   ## Unusual formatting (function won't work always...)
#'   # two or more people names: output incorrect (combine names of authors)
#'   prepTDWG("C. Mendonca Filho; F. da Silva")
#'   # two or more names, separated by comma: output incorrect (combine names of authors)
#'   prepTDWG("A. Alvarez, A. Zamora & V. Huaraca")
#'   # one name, two commas: fails to get all names
#'   prepTDWG("Cesar Sandro, Esteves, F")
#'   #' one name, abbreviations in the start and end: fails to get all names
#'   prepTDWG("C.S. Esteves F.")
#'
prepTDWG <- function(x, sep = ", ", format = "last_init", get.prep = FALSE, get.initials = TRUE) {

  # Detecting different name formats
  patt <- paste0("\\p{Ll}", sep, "\\p{Lu}")
  # patt <- paste0("[[:alpha:]]", sep, "[A-ZÀ-Ý]")
  commas <- grepl(patt, x, perl = TRUE)
  NAs <- x %in% c("", " ", NA)
  words <- grepl(" ", x, fixed = TRUE)

  # Detecting other name formats
  oa <- grepl("O'[A-Z]", x, perl = TRUE)
  mac <- grepl('Mac[A-Z]', x, perl = TRUE)
  mc <- grepl('Mc[A-Z]', x, perl = TRUE)

  nomes <- x

  if (any(commas & words & !NAs)) { # Names already with commas and >1 word

    # Getting last and first names, as well as the prefixes/prepositions and initials
    split <- matrix(c(lastName(x[commas & words & !NAs]),
                      lastName(x[commas & words & !NAs], invert = TRUE, initials = FALSE)),
                    ncol = 2)
    split <- getPrep(split, rm.prep = FALSE)

    if (get.initials) {

      # Initials containing (collection) codes in parentheses
      remove_these <-
        grepl(" \\([A-Z]", split[, 2]) & grepl("[A-Z]\\)", split[, 2])
      if (any(remove_these))
        split[, 2][remove_these] <-
          gsub(" \\([A-Z].*", "", split[, 2][remove_these], perl = TRUE)

      split[,2] <- getInit(split[, 2], max.initials = 10)

    } else {

      split[,2] <- gsub("(\\.)(\\s)(\\p{L})(\\.)", "\\1\\3\\4", split[,2], perl = TRUE)
      split[,2] <- gsub("(\\p{L})(\\s)(\\p{L})+", "\\1.\\3.", split[,2], perl = TRUE)
      split[,2] <- gsub("(\\p{L})(\\p{L})+", "\\1.\\2.", split[,2], perl = TRUE)
      # split[,2] <- gsub("(\\.)(\\s)([A-Zà-ý])(\\.)", "\\1\\3\\4", split[,2], perl = TRUE)
      # split[,2] <- gsub("([A-Zà-ý])(\\s)([A-Zà-ý])+", "\\1.\\3.", split[,2], perl = TRUE)
      # split[,2] <- gsub("([A-Zà-ý])([A-Zà-ý])+", "\\1.\\2.", split[,2], perl = TRUE)
      split[,2] <- gsub("\\s\\s+", " ", split[,2], perl = TRUE)
      split[,2] <- gsub("^ | $", "", split[,2], perl = TRUE)

    }

    # Generating the name string
    if (get.prep) {

      if (format %in% c("last_init", "last_init_prep"))
        names <- paste(paste(split[,1], split[,2], sep = sep), split[,3])
      if (format %in% c("prep_last_init"))
        names <- paste(split[,3], paste(split[,1], split[,2], sep = sep))
      if (format %in% c("init_last"))
        names <- paste(split[,2], split[,3], split[,1])

    } else {

      if (format %in% c("last_init", "last_init_prep", "prep_last_init"))
        names <- paste(split[,1], split[,2], sep = sep)
      if (format %in% c("init_last"))
        names <- paste(split[,2], split[,1])

    }

    # Final edits on the name string
    names <- gsub("\\s\\s+", " ", names, perl = TRUE)
    names <- gsub("^ | $", "", names, perl = TRUE)
    names <- gsub("\\.\\s\\.", ".", names, perl = TRUE)
    nomes[commas & words & !NAs] <- names

  }

  if (any(!commas & words & !NAs)) { # Names without commas and >1 word

    # Names ending with (collection) codes in parentheses
    remove_these <-
      grepl(" \\([A-Z]", x[!commas & words & !NAs]) &
        grepl("[A-Z]\\)$", x[!commas & words & !NAs])
    if (any(remove_these))
      x[!commas & words & !NAs][remove_these] <-
        gsub(" \\([A-Z].*", "",
             x[!commas & words & !NAs][remove_these], perl = TRUE)

    split <- matrix(c(lastName(x[!commas & words & !NAs]),
                      lastName(x[!commas & words & !NAs], invert = TRUE, initials = FALSE)),
                    ncol = 2)
    split <- getPrep(split, rm.prep = FALSE)

    if (get.initials) {

      split[,2] <- getInit(split[,2], max.initials = 10)
      split[,2][split[,2] %in% "character(0)."] <- ""

    } else {

      split[,2] <- gsub("(\\.)(\\s)(\\p{L})(\\.)", "\\1\\3\\4", split[,2], perl = TRUE)
      split[,2] <- gsub("(\\p{L})(\\s)(\\p{L})+", "\\1.\\3.", split[,2], perl = TRUE)
      split[,2] <- gsub("(\\p{L})(\\p{L})+", "\\1.\\2.", split[,2], perl = TRUE)
      # split[,2] <- gsub("(\\.)(\\s)([A-Zà-ý])(\\.)", "\\1\\3\\4", split[,2], perl = TRUE)
      # split[,2] <- gsub("([A-Zà-ý])(\\s)([A-Zà-ý])+", "\\1.\\3.", split[,2], perl = TRUE)
      # split[,2] <- gsub("([A-Zà-ý])([A-Zà-ý])+", "\\1.\\2.", split[,2], perl = TRUE)
      split[,2] <- gsub("\\s\\s+", " ", split[,2], perl = TRUE)
      split[,2] <- gsub("^ | $", "", split[,2], perl = TRUE)

    }

    # Generating the name string
    if (get.prep) {

      if(format %in% c("last_init", "last_init_prep"))
        names <- paste(paste(split[,1], split[,2], sep = sep), split[,3])
      if(format %in% c("prep_last_init"))
        names <- paste(split[,3], paste(split[,1], split[,2], sep = sep))
      if(format %in% c("init_last"))
        names <- paste(split[,2], split[,3], split[,1])

    } else {

      if(format %in% c("last_init", "last_init_prep", "prep_last_init"))
        names <- paste(split[,1], split[,2], sep = sep)
      if(format %in% c("init_last"))
        names <- paste(split[,2], split[,1])

    }

    # Final edits on the name string
    names <- gsub("\\s\\s+", " ", names, perl = TRUE)
    names <- gsub("^ | $", "", names, perl = TRUE)
    names <- gsub(",$", "", names, perl = TRUE)
    names <- gsub("\\.\\s\\.", ".", names, perl = TRUE)
    nomes[!commas & words & !NAs] <- names

  }

  if (any(!words & !NAs)) # Names with a single word
    nomes[!words & !NAs] <- capName(nomes[!words & !NAs])

  if (any(NAs)) # NAs, "" or " "
    nomes[NAs] <- NA_character_

  #Fixing names in other formats, if present
  if (any(oa))
    nomes[oa] <- gsub("(O')([a-z])", "\\1\\U\\2",  nomes[oa], perl = TRUE)

  if (any(mac))
    nomes[mac] <- gsub("(Mac)([a-z])", "\\1\\U\\2",  nomes[mac], perl = TRUE)

  if (any(mc))
    nomes[mc] <- gsub("(Mc)([a-z])", "\\1\\U\\2",  nomes[mc], perl = TRUE)

  return(as.character(nomes))
}
