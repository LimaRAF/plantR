#' @title Format People's Name To The TDWG Format
#'
#' @description Convert people's names from the long format (i.e. 'First Name(s)
#'   + Last Name(s)') to the \href{https://www.tdwg.org/}{Biodiversity Information Standards}
#'   (TDWG) format (i.e. Last Name, Initials of First Names(s)).
#'
#' @param x the character string with a name.
#' @param keep.prep logical. Should name prepositions be returned? Default to
#'   FALSE.
#'
#' @return The character string \code{x} in the TDWG format.
#'
#' @details The standard name format suggested by the
#'   \href{https://www.tdwg.org/}{TDWG} is: Last name, followed by a comma and
#'   then the initials, separated by points (e.g. Hatschbach, G.G.).
#'
#'   The function implicitly assumes that the last name is the one at the end of
#'   the name string.
#'
#'   The function identifies common name prefixes or prepositions (e.g. de,
#'   dos, van, ter, ...). By default, these prefixes and prepositions are
#'   removed, but they can be returned if the argument `keep.prep` is set to
#'   TRUE.
#'
#'   The functions uses internally another __plantR__ function: `lastName()`.
#'   So, the function deals with simples last names, as well as with compound
#'   last names and last names with generational suffixes.
#'
#'   The function asumes that all names already containig commas are in the TDWG
#'   format. So, this cases are not edited. If only one name is given, the fucntion return
#'   the same name with the first letter capitalized. The function currently does not handle
#'   multiple names within the same name string. The function output it is
#'   relatively stable regarding the input format, lower/uppercasing and spacing, but it may not
#'   work in all cases (see examples below).
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
#' @export getTDWG
#'
#' @importFrom stringr str_count
#'
#' @examples
#'   # Simple names
#'   getTDWG("gentry")
#'   getTDWG("Al Gentry")
#'   getTDWG("Alwyn Howard Gentry")
#'   getTDWG("Gert G. Hatschbach")
#'   getTDWG("G. G. HATSCHBACH")
#'
#'   # Names already in the TDWG format (does nothing)
#'   getTDWG("Gentry, AH")
#'   getTDWG("Gentry, A.")
#'
#'   # Name with prepositions
#'   getTDWG("Carl Friedrich Philipp von Martius")
#'   getTDWG("Alphonse Louis Pierre Pyrame de Candolle")
#'   getTDWG("Simon Jan van Ooststroom")
#'   getTDWG("Jan van der Hoeven")
#'   getTDWG("Maria da Silva")
#'   getTDWG("Maria da Silva", keep.prep = TRUE)
#'
#'   # Names with generational suffixes
#'   getTDWG("Hermogenes Leitao Filho")
#'   getTDWG("J.E.Q. Faria Junior")
#'   getTDWG("S.J. Filho Neto")
#'
#'   # Compound last name (needs to be marked with a '-')
#'   getTDWG("Augustin Saint-Hilaire")
#'   getTDWG("a. saint-hilaire")
#'   getTDWG("Saint-Hilaire A.")
#'   getTDWG("Augustin Saint Hilaire") # compound name missing '-'
#'
#'   # Unusual formatting (function won't work always...)
#'   getTDWG("L. McDade") #cannot recognize names starting with Mc or Mac
#'   # two or more names: output incorrect (combine names of authors)
#'   getTDWG("C. Mendonca Filho; F. da Silva")
#'
#'
getTDWG <- function(x, keep.prep = FALSE) {

  words <- stringr::str_count(x, "\\w+") >= 2
  no.commas <- !grepl("[[:alpha:]], [A-ZÀ-Ý]", x, perl = TRUE)
  ids <- words & no.commas

  if (any(ids)) {

    #Editing the separation between initials
    x1 <- gsub("[.]", ". ", x[ids], perl = TRUE)
    x1 <- gsub("\\s+", " ", x1, perl = TRUE)

    #Editing compound prepositions
    x1 <- gsub(" (De) (La) ", " \\1_\\2 ", x1,
               perl = TRUE, ignore.case = TRUE)
    x1 <- gsub(" (De) (Las) ", " \\1_\\2 ", x1,
               perl = TRUE, ignore.case = TRUE)
    x1 <- gsub(" (De) (Lo) ", " \\1_\\2 ", x1,
               perl = TRUE, ignore.case = TRUE)
    x1 <- gsub(" (De) (Los) ", " \\1_\\2 ", x1,
               perl = TRUE, ignore.case = TRUE)
    x1 <- gsub(" (Van) (Den) ", " \\1_\\2 ", x1,
               perl = TRUE, ignore.case = TRUE)
    x1 <- gsub(" (Van) (Der) ", " \\1_\\2 ", x1,
               perl = TRUE, ignore.case = TRUE)

    #Getting last name, the other names and the initials
    last.name <- lastName(x1)
    split <- sapply(x1, strsplit, " ", fixed = TRUE)
    patt <- sapply(last.name, function (x)
      paste0(unique(
        c(x, unlist(strsplit(x, " |-", perl = TRUE)
                    ))), collapse = "|"))

    preps <-
      "^De$|^Dos$|^Do$|^Da$|^Das$|^Del$|^Du$|^Des$|^Di$|^Dalla$|^Della$|^Ter$|^Von$|^Van$|^De_La$|^De_Las$|^De_Lo$|^De_Los$|^Van_Der$|^Van_Den$"

    other.names <- initials <- prep.ids <- split
    for(i in 1:length(split)) {
      other.names[[i]] <-
        split[[i]][!grepl(patt[i], split[[i]],
                          perl = TRUE, ignore.case = TRUE)]
      initials[[i]] <-
        toupper(sapply(other.names[[i]], substring, 1, 1))
      prep.ids[[i]] <-
        !grepl(preps, names(initials[[i]]),
               perl = TRUE, ignore.case = TRUE)
    }

    #Storing the name prefix and prepositions, if required
    name.prep <- NULL
    if (keep.prep) {
      name.prep <- vector("list", length(prep.ids))
      check.preps <- !sapply(prep.ids, tail, 1)
      if (any(check.preps)) {
        name.prep[check.preps] <-
          sapply(initials[check.preps], function(x) tail(names(x), 1))
        name.prep[check.preps] <-
          lapply(name.prep[check.preps], function(x) gsub("_", " ", x, fixed = TRUE))
      }
    }

    # Removing the name prefix and prepositions from the initials
    for(i in 1:length(initials)) {
      initials[[i]] <- initials[[i]][prep.ids[[i]]]
    }
    initials <- sapply(initials, paste0, collapse = ".")
    initials <- gsub("$", "\\1.", initials, perl=)

    #Getting the name in the final format
    x2 <- paste(last.name, initials, sep = ", ")
    if (keep.prep)
      x2[check.preps] <-
        paste(x2[check.preps], unlist(name.prep[check.preps]), sep = " ")



    # split <- sapply(x1, strsplit, " ", fixed = TRUE)[[1]]
    # patt <- paste0(unique(c(last.name, unlist(
    #   strsplit(last.name, " |-", perl = TRUE)
    #   ))), collapse = "|")
    # other.names <- split[!grepl(patt, split,
    #                             perl = TRUE, ignore.case = TRUE)]

    #Getting the name initials and prepositions
    # initials <- toupper(sapply(other.names, substring, 1, 1))
    # preps <-
    #   "^De$|^Dos$|^Do$|^Da$|^Das$|^Del$|^Du$|^Des$|^Di$|^Dalla$|^Della$|^Ter$|^Von$|^Van$|^De_La$|^De_Las$|^De_Lo$|^De_Los$|^Van_Der$|^Van_Den$"
    # prep.ids <-
    #   !grepl(preps,
    #          names(initials),
    #          perl = TRUE,
    #          ignore.case = TRUE)
    # name.prep = NULL
    #
    # if (keep.prep) {
    #   if (!tail(prep.ids, 1)) {
    #     name.prep <- tail(names(initials), 1)
    #     name.prep <- gsub("_", " ", name.prep, fixed = TRUE)
    #   }
    # }
    #
    # initials <- initials[prep.ids]
    # initials <- paste0(paste0(initials, collapse = "."), ".")
    #
    # #Getting the name in the final format
    # x2 <- paste(last.name, initials, sep = ", ")
    # if (keep.prep & !is.null(name.prep))
    #   x2 <- paste(x2, name.prep, sep = " ")

    #Final edits (removing duplicated commas and bad name endings)
    x2 <- gsub(",,", ",", x2, fixed = TRUE)
    x2 <- gsub("\\.\\.", "", x2, perl = TRUE)

    #Saving only the pertinent changes
    x[ids] <- x2
  }

  if (any(!words))
    x[!words] <- capName(x[!words])

 return(x)
}
