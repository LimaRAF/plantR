#' @title Format Multiple People's Names
#'
#' @description Format or convert multiple collector's or identificator's names
#'   into a standardized name format.
#'
#' @param x the character string or vector containing the names.
#' @param fix.names logical. Should the general notation of names be
#'   standardized? Default to TRUE.
#' @param output a character string with the type of output desired: all names,
#'   first name, or auxiliary names.
#' @param treat.prep character
#' @param ... Parameters from fixName
#'
#' @inherit fixName params
#' @inherit prepTDWG params
#'
#' @return The character string \code{x} in a standardized name format.
#'
#' @details The default name format is the one suggested by the
#'   \href{https://www.tdwg.org/}{TDWG} is: Last name, followed by a comma and
#'   then the initials, separated by points (e.g. Hatschbach, G.G.). By default,
#'   the names of multiple people associated to each record are separated by a
#'   pipe (i.e. '|'). But this default can be altered using the argument `sep.out`.
#'
#'   In the case of names from more then one person (separated by the characters
#'   defined in the argument `sep.in`, the argument `output` controls which names
#'   should be returned: names of all person ("all", the default), first person's
#'   names ("first") or all but the first person's names ("aux").
#'
#'   The function identifies (and removes) name prefixes or prepositions (e.g. de,
#'   dos, van, ter, ...). Also, it removes some titles (i.e. Dr., Prof., Pe., Sr., Mr.)
#'   but not all of them (e.g. Doctor, Priest, Mister, etc.). The function also does not
#'   handle hyphenated first names. If only one name is given, the function
#'   returns \code{x} with the first letter capitalized.
#'
#'   The function has the option of stardardizing the general notation of names
#'   and the general format of names. These standardizations are controled by
#'   the arguments `fix.names` and `prep.tdwg`, which call internally the
#'   __plantR__ functions `fixName()` and `prepTDWG()`.
#'
#' @seealso
#'  \link[plantR]{fixName}, \link[plantR]{lastName} and \link[plantR]{prepTDWG}.
#'
#' @author Renato A. F. de Lima & Hans ter Steege
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
#' @export prepName
#'
#' @importFrom stringr str_trim
#' @import data.table
#'
#' @examples
#'
#'   # Simple names
#'   prepName("Alwyn H. Gentry")
#'   prepName("Karl Emrich & Balduino Rambo")
#'   prepName("R. Reitz; R.M. Klein")
#'   prepName("Reitz, Raulino et R.M. Klein", sep.out = " & ")
#'
#'   # Name with prepositions and compound last names
#'   prepName("Carl F. P. von Martius; Augustin Saint-hilaire")
#'   prepName("Carl F. P. von Martius; Auguste de Saint-Hilaire", get.prep = TRUE)
#'   prepName("A. Ducke; Dárdano de Andrade-Lima")
#'   prepName("Ducke, A. ; Dárdano de Andrade-Lima")
#'
#'   # Names with generational suffixes
#'   prepName("HF Leitão Filho; GJ Shepherd")
#'
#'   # Names with titles
#'   prepName("Pe. Raulino Reitz")
#'   prepName("Prof. Hermogenes de Freitas Leitao Filho")
#'   prepName("Sir G.T. Prance")
#'   prepName("Sir G.T. Prance", treat.prep = "Sir")
#'
#'   # Other name formats
#'   prepName("[D. Hugh-Jones]")
#'   prepName("L. McDade & J. O'Brien")
#'
#'   # Multiple names separated by different characters
#'   prepName("A. Alvarez; A. Zamora & V. Huaraca")
#'   prepName("A. Alvarez; A. Zamora & V. Huaraca", out = "first")
#'   prepName("A. Alvarez; A. Zamora & V. Huaraca", out = "aux")
#'
#'   # Multiple names separated by commas
#'   prepName("A. Alvarez, A. Zamora & V. Huaraca") # output incorrect
#'   prepName("A. Alvarez, A. Zamora & V. Huaraca", sep.in=c(",","&")) # output correct
#'
#'   # Multiple (last + first) names separated by commas
#'   prepName("Alvarez, A., Zamora, A. & Huaraca, V.", sep.in=c(",","&"))  # output incorrect
#'   prepName("Alvarez, A., Zamora, A. & Huaraca, V.", sep.in=c(".,","&")) # output correct
#'
prepName <- function(x, fix.names = TRUE, sep.in = c(";", "&", "|", " e ", " y ", " and ", " und ", " et "),
                     sep.out = "|", special.char = FALSE, output = "all", treat.prep = c("Dr.","Pe.","Prof.","Sr.","Mr."),
                     format = "last_init", get.prep = FALSE, get.initials = TRUE, ...) {

  #Escaping R CMD check notes from using data.table syntax
  "V1" <- "tmp.ordem" <- NULL

  # check input:
  if (is.null(x)|!class(x) %in% "character")
    stop("Input need to be a string or vector of names")

  # name inside brackets or parentheses? removing here and adding after editions
  bracks <- grepl('^\\[', x, perl = TRUE) & grepl('\\]$', x, perl = TRUE)
  parent <- grepl('^\\(', x, perl = TRUE) & grepl('\\)$', x, perl = TRUE)
  x <- gsub("^\\[|\\]$|^\\(|\\)$", "", x, perl = TRUE) #

  # Editing the general name notation
  if (fix.names)
    x <- fixName(x, sep.in = sep.in, sep.out = sep.out, special.char = special.char)
    #x <- fixName(x, ...)

  # removing treatment prepositions (e.g. Dr., Prof., Pe., ...)
  patt.treat <-
    gsub("\\.", "\\\\.", paste(treat.prep, collapse = " |"))
  if (!grepl("\\|", patt.treat))
    patt.treat <- paste0(patt.treat," ")
  x <- gsub(patt.treat, "", x, perl = TRUE)

  # Spliting the list of multiple people's names into a list
  sep.in1 <- gsub("(\\|)", "\\\\\\1", sep.in, perl = TRUE)
  sep.in1 <- gsub("(\\+)", "\\\\\\1", sep.in1, perl = TRUE)
  sep.in1 <- gsub("(\\$)", "\\\\\\1", sep.in1, perl = TRUE)
  sep.in1 <- gsub("(\\.)", "\\\\\\1", sep.in1, perl = TRUE)
  sep.in1 <- gsub("(\\?)", "\\\\\\1", sep.in1, perl = TRUE)
  sep.in1 <- gsub("(\\*)", "\\\\\\1", sep.in1, perl = TRUE)

  sep.out1 <- gsub("(\\|)", "\\\\\\1", sep.out, perl = TRUE)
  sep.out1 <- gsub("(\\+)", "\\\\\\1", sep.out1, perl = TRUE)
  sep.out1 <- gsub("(\\$)", "\\\\\\1", sep.out1, perl = TRUE)
  sep.out1 <- gsub("(\\.)", "\\\\\\1", sep.out1, perl = TRUE)
  sep.out1 <- gsub("(\\?)", "\\\\\\1", sep.out1, perl = TRUE)
  sep.out1 <- gsub("(\\*)", "\\\\\\1", sep.out1, perl = TRUE)

  if (fix.names) {
    patt.split <- paste(sep.out1,
                        paste0(stringr::str_trim(sep.out1),"(?=[A-ZÀ-Ý])"),
                        sep = "|")
  } else {
    patt.split <- paste0(sep.in1, collapse = "|")
  }
  split <- strsplit(x, patt.split, perl = TRUE)

  # Transforming list to data.table and preparing names using prepTDWG()
  DT <- data.table::setDT(data.table::transpose(split, fill = NA))
  cols <- data.table::copy(names(DT))
  DT[ , tmp.ordem := .I, ]

  if (output %in% c("all", "first")) {
    data.table::setkeyv(DT, c("V1"))
    DT[, V1 := lapply(V1, prepTDWG,
                      format = format, get.prep = get.prep, get.initials = get.initials),
       by = c("V1")]
  }

  if (output %in% c("all", "aux") & length(cols) > 1) {
    cols1 <- cols[!cols %in% c("V1", "tmp.ordem")]
    data.table::setkeyv(DT, c(cols1))
    DT[, (cols1) := lapply(.SD, prepTDWG,
                           format = format, get.prep = get.prep, get.initials = get.initials),
       by = cols1, .SDcols = cols1]
  }

  # Preparing to return the result as a vector again
  if (output == "first") {
    data.table::setkeyv(DT, "tmp.ordem")
    names.out <- as.character(DT$V1)
  }

  if (output == "aux" & length(cols) == 1) {
    # data.table::setkeyv(DT, "tmp.ordem")
    # names.out <- as.character(DT$V1)
    names.out <- rep(NA_character_, dim(DT)[1])
  }

  if (output == "aux" & length(cols) > 1) {
    DT[, V1:= NULL]
    cols1 <- cols[!cols %in% c("V1", "tmp.ordem")]
    data.table::setkeyv(DT, c(cols1))
    DT[ , names := do.call(paste, c(.SD, sep = sep.out)),
        by = cols1, .SDcols = cols1]
    data.table::setkeyv(DT, "names")
    DT[ , names := gsub(paste0(sep.out1, "NA"), "", names, perl = TRUE), by = "names"]
    DT[ , names := gsub("NA", NA_character_, names, fixed = TRUE), by = "names"]
    data.table::setkeyv(DT, "tmp.ordem")
    names.out <- DT$names
  }

  if (output == "all" | !output %in% c("first", "aux")) {
    if (length(cols) > 1) {
      cols1 <- cols[!cols %in% c("tmp.ordem")]
      data.table::setkeyv(DT, c(cols1))
      DT[ , names := do.call(paste, c(.SD, sep = sep.out)),
          by = cols1, .SDcols = cols1]
      data.table::setkeyv(DT, "names")
      DT[ , names := gsub(paste0(sep.out1, "NA"), "", names, perl = TRUE), by = "names"]
      DT[ , names := gsub("NA", NA_character_, names, fixed = TRUE), by = "names"]
      data.table::setkeyv(DT, "tmp.ordem")
      names.out <- DT$names
    } else {
      names.out <- DT$V1
    }
  }

  # Adding brackets (if needed)
  if (any(bracks))
    names.out[bracks] <- paste("[", names.out[bracks], "]", sep = "")

  # Adding parentheses (if needed)
  if (any(parent))
    names.out[parent] <- paste("(", names.out[parent], ")", sep = "")

  return(as.character(names.out))
}
