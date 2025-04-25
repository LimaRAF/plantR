#' @title Get Last Name
#'
#' @description Extract the last name of the collector or determiner
#'   of a biological specimen.
#'
#' @param name the character string or vector containing the names.
#' @param noName character. The standard notation for missing names in
#'   \code{name}. Default to "s.n.".
#' @param invert logical. Should first name(s) be returned instead of
#'   the last name? Default to FALSE.
#' @param initials logical. If first name(s) are chosen, should they
#'   be returned in full or only their initials? Default to FALSE.
#' @param first.capital logical. Should the name returned have only
#'   the first letter(s) capitalized or returned as provided? Default
#'   to TRUE.
#'
#' @return the last name provided in \code{name} with the first letter
#'   capitalized.
#'
#' @details The function works for simple last names or compound last
#'   names, independently if names are provided in lower or capital
#'   letters. It is relatively stable to the format and spacing of the
#'   string provided, but it may not work in all cases.
#'
#'   It implicitly assumes that last names are (i) the ones provided
#'   at the end of the name string if there is no comma, or (ii) the
#'   first name preceding the comma, if there is a comma in the name
#'   string. Few exceptions related to names not in the 'first + last
#'   name' or 'last name + comma + first name' formats (e.g.
#'   'Hatschbach G.G.') are also considered.
#'
#'   If only one name is given, the function returns the same name
#'   with the first letter capitalized.
#'
#'   For missing names (i.e. "", " " or NAs) the function returns the
#'   character defined by the argument `noName`.
#'
#'   The function can also return all names but the last name detected
#'   by setting the argument `invert` to TRUE. In this case, user can
#'   choose between full first names and only their initials by
#'   setting the argument `initials` to TRUE.
#'
#' @author
#'   Renato A. F. de Lima & Hans ter Steege
#'
#' @importFrom stringr str_c
#'
#' @export lastName
#'
#' @examples
#'
#' # Simple last name
#'  lastName("Gert Hatschbach")
#'  lastName("Gert Guenther Hatschbach")
#'  lastName("Gert G. Hatschbach")
#'  lastName("GERT GUENTHER HATSCHBACH")
#'  lastName("HATSCHBACH, G.G.")
#'  lastName("HATSCHBACH, G. G.")
#'  lastName("G. G. HATSCHBACH")
#'  lastName("Hatschbach GG")
#'
#'  # Last name with generational suffixes
#'  lastName("Hermogenes Leitao Filho")
#'  lastName("Leitao Filho, H.")
#'  lastName("Leitao Filho, H.F.")
#'  lastName("Filho Neto, S.J.")
#'
#'  # Compound last name
#'  lastName("Augustin Saint-hilaire")
#'  lastName("Saint-Hilaire A.")
#'
#'  # Multiple names
#'  names <- c("Hatschbach, GG", "Hatschbach G.G.",
#'  "Leitao filho, H. F.","Saint-Hilaire, augustin")
#'  lastName(names)
#'  lastName(names, invert = TRUE)
#'  lastName(names, invert = TRUE, initials = TRUE)
#'
#'  # Unusual formatting
#'  lastName("Cesar Sandro, Esteves, F")
#'  lastName("C.S. Esteves F.")
#'  lastName("Mendonca Filho, C.V. Neto, F.C.C.")
#'  # two or more names, separated by comma: output correct
#'  lastName("A. Alvarez, A. Zamora & V. Huaraca")
#'  # two or more names, not separated by comma: output incorrect
#'  lastName("Karl Emrich & Balduino Rambo")
#'
#'  # Some problematic (unresolved) examples
#'  lastName("Maria Da Silva", invert = TRUE, initials = TRUE)
#'
lastName <- function(name,
                     noName = "s.n.",
                     invert = FALSE,
                     initials = FALSE,
                     first.capital = TRUE) {

  # detecting missing names
  miss.name <- name %in% c("NA", NA, "", " ", noName)

  # preliminary edits:
  name <- gsub("[.]", ". ", name, perl = TRUE) # adding a space between points
  name <- squish(name)

  # Small, preliminary edits
  name <- gsub("^, | ,$", "", name, perl = TRUE)

  # Defining the general name formats (single, mult. w/ comma, mult. w/out comma)
  comma <- grepl("\\p{L},", name[!miss.name], perl = TRUE)
  no.first <- !grepl(" ", name[!miss.name], perl = TRUE)
  outros <- !no.first & !comma

  # identifying names with last name at the start and separated by a comma
  last.name <- name[!miss.name]
  if (any(comma))
    last.name[comma] <- gsub(",.*", "", last.name[comma], perl = TRUE)

  if (any(outros))
    last.name[outros] <- gsub(".*\\s", "", last.name[outros],
                              perl = TRUE)

  # Detecting possible problems with abbreviated initials at the end of the name
  probs <- grepl("^\\p{L}\\.$", last.name, perl = TRUE)
  if (any(probs))
    last.name[probs] <-
      gsub(" \\p{L}\\..*", "", name[!miss.name][probs], perl = TRUE)

  # Detecting possible problems with non-abbreviated initials at the end of the name
  probs1 <- last.name == toupper(last.name) &
              !grepl("\\.|,", name[!miss.name], perl = TRUE) &
                grepl(" ", name[!miss.name], fixed = TRUE)
  if (any(probs1)) {
    nome1 <- gsub(" .*", "", name[!miss.name][probs1], perl = TRUE)
    nome2 <- gsub(".* ", "", name[!miss.name][probs1], perl = TRUE)
    nome2[nchar(nome1) > nchar(nome2)] <-
      nome1[nchar(nome1) > nchar(nome2)]
    last.name[probs1] <- nome2
  }

  # identifying names with generational suffixes
  cp <- c("Filho", "Neto", "Jr.", "Junior", "Sobrinho")
  cp <- c(cp, tolower(cp))
  gen.suf <- last.name %in% cp

  if (any(gen.suf)) {
    cp.nome <- rep("", length(last.name))

    cp.nome[gen.suf] <- last.name[gen.suf]
    last.name[gen.suf] <- name[!miss.name][gen.suf]

    #Removing initials (muted, was leading to: Filho, E L P -> P Filho, F.L.P.)
    # last.name[gen.suf] <-
    #   gsub("\\s\\p{L}\\.", "", last.name[gen.suf], perl = TRUE)
    # last.name[gen.suf] <-
    #   gsub("\\p{L}\\.\\s", "", last.name[gen.suf], perl = TRUE)
    # last.name[gen.suf] <- gsub(",$", "", last.name[gen.suf], perl = TRUE)

    #Removing generational suffixes
    last.name[gen.suf] <- mapply(function(x, y) { gsub(x, "", y, fixed = TRUE) },
                                 cp.nome[gen.suf], last.name[gen.suf])
    last.name[gen.suf] <- gsub(" $", "", last.name[gen.suf], perl = TRUE)
    last.name[gen.suf] <- gsub(".*\\s", "", last.name[gen.suf], perl = TRUE)

    # Creating the compound last name, if not empty
    double.cp <- last.name[gen.suf] %in% ""
    last.name[gen.suf][!double.cp] <-
      paste(last.name[gen.suf][!double.cp],
            cp.nome[gen.suf][!double.cp], sep = " ")
    last.name[gen.suf][double.cp] <- cp.nome[gen.suf][double.cp]
  }

  # identifying compound last names
  cp <- c("Filho", "Neto", "Jr\\.", "Junior", "Sobrinho")
  compound <- grepl(" ", last.name, fixed = TRUE) &
                !grepl(paste(cp, collapse = "|"), last.name,
                       perl = TRUE, ignore.case = TRUE)
  if (any(compound)) {
    extra.name <- comp.nome <- rep("", length(last.name))
    comp.nome[compound] <- last.name[compound]

    extra.name[compound] <- gsub("\\s.*", "", comp.nome[compound], perl = TRUE)
    last.name[compound] <- mapply(function(x, y) { gsub(x, "", y, fixed = TRUE) },
                                 extra.name[compound], last.name[compound])
    last.name[compound] <- squish(last.name[compound])

    name[!miss.name][compound] <- mapply(function(x, y) { gsub(x, "", y, fixed = TRUE) },
                          extra.name[compound], name[!miss.name][compound])
    name[!miss.name][compound] <-
      gsub("^, | ,$", "", name[!miss.name][compound], perl = TRUE)
    name[!miss.name][compound] <-
      squish(name[!miss.name][compound])
    name[!miss.name][compound] <-
      paste(name[!miss.name][compound], extra.name[compound])
  }

  if (invert) {
    # Keeping all but the last name(s)
    other.names <- mapply(function(x, y) {
                          gsub(x, "", y, fixed = TRUE) },
                          last.name, name[!miss.name])

    # Inspecting the exceptions (compound names with abbreviations)
    check_these <- other.names == name[!miss.name] &
                      grepl(" ", last.name, fixed = TRUE)
    if (any(check_these)) {
      last.name1 <- sapply(strsplit(last.name[check_these], " ",
                                    fixed = TRUE),
                           stringr::str_c, collapse = "|")
      last.name1 <- gsub("(^\\p{Lu})(\\|)", " \\1$\\2", last.name1,
                         perl = TRUE)
      last.name1 <- gsub("(^\\p{Lu})\\.(\\|)", " \\1\\\\.$\\2", last.name1,
                         perl = TRUE)
      other.names1 <- other.names[check_these]
      other.names2 <- mapply(function(x, y) { gsub(x, "", y, perl = TRUE) },
                            last.name1, other.names1)
      other.names[check_these] <- other.names2
    }

    #Fixing other names
    # other.names <- stringr::str_squish(other.names)
    other.names <- gsub("\\s+", " ", other.names, perl = TRUE)
    other.names <- gsub("^ | $", "", other.names, perl = TRUE)
    other.names <- gsub(", ", "", other.names, fixed = TRUE)

    #Detecting possible multiple names and removing them
    seps <- c(";|&|\\|| e | y | and | und | et ")
    mult.names <- grepl(seps, other.names, perl = TRUE)
    if (any(mult.names))
      other.names[mult.names] <-
        gsub(";.*|&.*|\\|.*| e .*| y .*| and .*| und .*| et .*", "",
           other.names[mult.names], perl = TRUE)

    if (initials) {

      #Detecting the possible types of first name: full or abbreviated
      inits <- grepl('\\p{Lu}', other.names, perl = TRUE)
      abrev.inits <-
        grepl('(\\p{L}\\.)(\\p{L}\\.)+|(\\p{L}\\.)\\s(\\p{L}\\.)+',
                    other.names, perl = TRUE)
      combo <- inits | abrev.inits
      inits[abrev.inits] <- FALSE

      #Extracting the initials for each type of first name
      if (any(inits)) {
        w_spaces <- grepl(" ", other.names[inits], fixed = TRUE)
        other.names[inits][w_spaces] <-
          gsub('\\b(\\p{Lu})|.', '\\1', other.names[inits][w_spaces],
               perl = TRUE)
        other.names[inits][!w_spaces] <-
          gsub("[^\\p{Lu}]+", "", other.names[inits][!w_spaces],
                                   perl = TRUE)
      }

      if (any(abrev.inits)) {
        other.names[abrev.inits] <-
          gsub("[^\\p{L}]+", "", other.names[abrev.inits], perl = TRUE)
        other.names[abrev.inits] <-
          toupper(gsub("(?<=\\p{Lu})\\p{Ll}.*", "", other.names[abrev.inits],
                       perl = TRUE))
      }

      if (any(!combo))
        other.names[!combo] <- toupper(substr(other.names[!combo], 1, 1))

      #Preparing the initials
      other.names <- gsub("(\\p{Lu})", "\\1.", other.names, perl = TRUE)

    }

    # Saving
    name[!miss.name] <- other.names

  } else {

    # Final edits for possible initials remaining due to multiple names separated by commas
    last.name <- gsub("([A-Z]\\.) ([A-Z]\\.).", "\\1\\2",
                      last.name, perl = TRUE)

    # Capitalizing the first letter of the last name
    if (first.capital)
      last.name <- capName(last.name)

    # Adding a period to abbreviated compound last names (muted until checking)
    last.name <- gsub("(^\\p{Lu})(\\s\\p{Lu})", "\\1.\\2",
                      last.name, perl = TRUE)

    # Saving
    name[!miss.name] <- last.name
  }

  if (any(miss.name))
    name[miss.name] <- noName

  return(name)
}
