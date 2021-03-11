#' @title Format Collector Number
#'
#' @description The function standardizes the 'collector number' which is
#'   typically associated with biological records.
#'
#' @param x a character string or vector
#' @param colCodes a character string with the collection codes to be removed
#'   from the collector number. Default to NULL.
#' @param noNumb character. The desired notation in the case of missing
#'   collector number. Defaults to "s.n."
#'
#' @return a character vector with the same length of \code{x} with the edited
#'   collector numbers.
#'
#' @details The function performs several edits such as removal of unnecessary
#'   spaces, letters, parentheses, and the replacement of missing information of
#'   collector numbers into a standardized notation, defined by the argument
#'   `noNumb`. Zeros and strings without numbers are treated as missing
#'   information.
#'
#' @author Renato A. F. de Lima
#'
#' @references Willemse, L.P., van Welzen, P.C. & Mols, J.B. (2008).
#'   Standardisation in data-entry across databases: Avoiding Babylonian
#'   confusion. Taxon 57(2): 343-345.
#'
#' @importFrom stringr str_trim
#'
#' @export colNumber
#'
#' @examples
#' # A vector with some typical examples of formats found in herbarium labels
#' numbers <- c("3467", "3467 ", " 3467", "ALCB3467", "Gentry 3467", "ALCB-3467",
#' "3467a", "3467A", "3467 A", "3467-A", "PL671", "57-685", "685 - 4724", "1-80",
#' "-4724", "(3467)", "(3467", "3467)", "32-3-77", "s/n.", "s.n.", "s.n", "", NA)
#'
#' # Using the function defaults
#' colNumber(numbers)
#'
#' # Using the function to remove the collection code from the collector number
#' colNumber(numbers, colCodes = c("ALCB","ESA"))
#'
#' # Defining user-specific abbreviations for the specimens without collector number
#' colNumber(numbers, colCodes = c("ALCB","ESA"), noNumb = "n.a.")
#'
colNumber <- function(x,
                      colCodes = NULL,
                      noNumb = "s.n.") {

  # first edits
  x <- gsub("\\s\\s+", " ", x, perl = TRUE)
  numbs <- x

  # Number missing
  numbs[numbs %in% c(0, "0", "", " ", NA)] <- "SemNumero"
  numbs[!grepl("\\d", numbs, perl = TRUE)] <- "SemNumero"
  numbs[!is.na(numbs) & grepl(" s.n. ", numbs)] <- "SemNumero"

  # Removing the collection code from the beggining of the collection number
  if (!is.null(colCodes))
    numbs[!is.na(numbs) &
            grepl(paste("^", colCodes, collapse = "|", sep = ""), numbs, ignore.case = TRUE, perl = TRUE)] <-
                gsub(paste("^", colCodes, collapse = "|", sep = ""), "", numbs[!is.na(numbs) &
                    grepl(paste("^", colCodes, collapse = "|", sep = ""), numbs, ignore.case = TRUE, perl = TRUE)])

  # Removing names of collectors and others codes from the beginning of the numbers
  numbs[!is.na(numbs) &
          grepl("[a-z][a-z][a-z] ", numbs, ignore.case = TRUE, perl = TRUE)] <-
            as.character(sapply(sapply(strsplit(numbs[!is.na(numbs) &
                grepl("[a-z][a-z][a-z] ", numbs, ignore.case = TRUE, perl = TRUE)], " "), function(x)
                  x[grepl('[0-9]|SemNumero', x, perl = TRUE)]), my.tail))
  numbs[!is.na(numbs) & grepl("SemNumero", numbs, perl = TRUE)] = "SemNumero"
  numbs[!is.na(numbs) & grepl("character\\(0\\)", numbs, perl = TRUE)] = "SemNumero"

  #Removing unwanted characters ans spacing
  numbs <- gsub(' - ', "-", numbs, fixed = TRUE)

  #Removing misplaced parenteses
  numbs <- gsub(' \\(', "\\(", numbs, perl = TRUE)
  numbs <- gsub('\\) ', "\\)", numbs, perl = TRUE)
  numbs[grepl('^\\(', numbs, perl = TRUE) & !grepl('\\)$', numbs, perl = TRUE)] <-
    gsub('^\\(', '', numbs[grepl('^\\(', numbs, perl = TRUE) & !grepl('\\)$', numbs, perl = TRUE)], perl = TRUE)
  numbs[!grepl('^\\(', numbs, perl = TRUE) &  grepl('\\)$', numbs, perl = TRUE)] <-
    gsub('\\)$', '', numbs[!grepl('^\\(', numbs, perl = TRUE) & grepl('\\)$', numbs, perl = TRUE)], perl = TRUE)

  #Replacing orphan spaces by separators
  numbs <- gsub(' ', "-", numbs, fixed = TRUE)

  #Including separators between number qualificators
  numbs[grepl('[0-9] [A-Z]', numbs, ignore.case = TRUE, perl = TRUE)] <-
    gsub(' ', "-", numbs[grepl('[0-9] [A-Z]', numbs, ignore.case = TRUE, perl = TRUE)], perl = TRUE)

  #PUT THIS FUNCTION IN PACKAGE DOCUMENTATION?
  #NEED TO BE FIXED: CONVERTING 116F4 TO 1164-F!
  f1 <- function(x) {
    x1 <- strsplit(x, "")[[1]]
    names(x1) <- 1:length(x1)
    x2 <- as.character(paste(x1[grepl('[0-9]', x1)], sep = "", collapse = ""))
    m.x2 <- min(as.double(names(x1[grepl('[0-9]', x1)])))
    x3 <- as.character(paste(x1[grepl('[a-z]', x1, ignore.case = TRUE)], sep =
                              "", collapse = ""))
    m.x3 <- min(as.double(names(x1[grepl('[a-z]', x1, ignore.case = TRUE)])))
    if (m.x2 < m.x3) {

      x4 = paste(x2, toupper(x3), sep = "-")

    } else {

      x4 = paste(toupper(x3), x2, sep = "-")
    }

    return(x4)
  }

  numbs[grepl('[0-9][A-Z]', numbs, ignore.case = TRUE)] <-
    sapply(numbs[grepl('[0-9][A-Z]', numbs, ignore.case = TRUE)], FUN = f1)

  numbs <- gsub(' e ', ", ", numbs, fixed = TRUE)
  numbs <- gsub('#|\\?|\\!|\\.', "", numbs, perl = TRUE)
  numbs <- gsub(", ", ",", numbs, fixed = TRUE)
  numbs <- gsub("Collector Number:", "", numbs, fixed = TRUE)
  numbs <- gsub("NANA", "SemNumero", numbs, fixed = TRUE)
  numbs <- gsub('^--$', "SemNumero", numbs, perl = TRUE)
  numbs <- gsub('^-', "", numbs, perl = TRUE)
  numbs <- gsub('-$', "", numbs, perl = TRUE)
  numbs[!grepl('[0-9]', numbs, perl = TRUE)] <- "SemNumero"

  # Replacing the missing number by a standard code, provided as an argument in the function
  numbs <- gsub("SemNumero", noNumb, numbs, fixed = TRUE)

  # Final edits
  numbs <- gsub("--", "-", numbs, fixed = TRUE)
  numbs <- gsub("&nf;", "", numbs, fixed = TRUE)
  numbs <- stringr::str_trim(numbs)
  #numb <- gsub('[a-z]-[0-9]','',numb, ignore.case=TRUE) ##CHECK

  return(numbs)
}
