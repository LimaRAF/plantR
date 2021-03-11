#' @title Isolate Family Name Prepositions
#'
#' @description This function isolates and removes (optional) last name prepositions
#'   (i.e. 'da' in 'da Silva') from multiple people's names, if present.
#'
#' @param x a name string, a vector of names or a two-column matrix or
#'   data.frame, containing the last name in the first column and other
#'   names in the second.
#' @param preps a vector with the name prepositions to be isolated. Defaults to
#' some common prepositions in portuguese, spanish, italian, french, and dutch
#' family names.
#' @param rm.prep logical. Should the preposition be removed? Default to FALSE.
#' @param output character. Should the names be returned as a vector of
#'   standardized names or organized as a matrix? Default to "matrix".
#' @param format character. Format of the output vector of names The default ("last_init")
#' is the TDWG standard, but the inverse format can also be chosen ("init_last").
#'
#' @return A vector or a matrix containing the names provided in \code{x} with
#'   the name prepositions isolated from the last name.
#'
#' @details By default, the function searches for common name prepositions but
#'   the user can provide a personal list.
#'
#'   The function assumes that prepositions can be at the start/end of the
#'   string for names in the 'Last name, First name(s)' format (e.g. "da Silva,
#'   Maria" or "Silva, Maria da"), separated by an space to the right/left from
#'   other names. Or it assumes that prepositions can be at the middle in the
#'   'First name(s) Last name' format ('Maria da Silva'), separated by spaces in
#'   both sides.
#'
#'   In the case of a vector of names, the 'Last name, First name(s)' name
#'   format is prioritized over the 'First name(s) Last name' format while
#'   separating last from first names. In addition, only the prepositions in the
#'   last name are isolated and can be returned. Prepositions in middle names
#'   are silently excluded.
#'
#'   Names can be provided as vectors of names or as a two-column matrix/data
#'   frame in which the last names are provided in the first column and other
#'   names in the second. Users can also chose the output format between
#'   "last_init_prep" or "last_init" (the default; e.g. "Silva, Maria A. Pereira"),
#'   "prep_last_init" (e.g. "da Silva, Maria A. Pereira") or "init_last"
#'   (e.g. "Maria A. Pereira (da) Silva"), using the argument `format`.
#'
#' @author Renato A. F. de Lima
#'
#' @keywords internal
#'
#' @importFrom stringr str_trim
#'
#' @examples
#' names <- c("Silva, Maria A. Pereira da", "Silva, Maria A. Pereira Da",
#' "da Silva, Maria A. Pereira", "ter Braak, Hans", "Braak, Hans ter",
#' "Silva, Maria A. Pereirada", "Braak, Hanster", "Maria A. Pereira da Silva",
#' "Hans ter Braak", "Maria A. Pereirada Silva", "Hanster Braak", "da Silva",
#' "Silva")
#'
#' \dontrun{
#' getPrep("Maria da Silva")
#' getPrep(names)
#' getPrep(names, output = "vector")
#' getPrep(names, output = "vector", format = "prep_last_init")
#' getPrep(names, output = "vector", format = "init_last")
#' }
#'
getPrep <- function(x, preps = c("De", "Dos", "Do", "Da", "Das", "Del", "Du",
                                 "Des", "Di", "Dalla", "Della", "Ter", "Von",
                                 "Van", "De La", "De Las", "De Lo", "De Los",
                                 "Van Der", "Van Den"),
                    rm.prep = FALSE, output = "matrix", format = "last_init_prep"){

  if (!class(x)[1] %in% c("character", "data.frame", "matrix")) {
    stop("Input must be a vector, matrix or a data frame")
  }

  if (!format %in% c("last_init", "last_init_prep", "prep_last_init", "init_last")) {
    stop("Please provide one of the following formats: last_init, last_init_prep, prep_last_init, init_last")
  }

  if (class(x)[1] == "character") {
    # Storing Last names and initials separately
    split <- strsplit(x, ", ", perl = TRUE)
    no.commas <- lengths(split) %in% 1
    split[no.commas] <- strsplit(x[no.commas], " (?=[^ ]+$)", perl = TRUE)
    split <- t(sapply(split, `length<-`, 2))
    split[no.commas & !is.na(split[,2]),] <-
      split[no.commas & !is.na(split[,2]), 2:1]
  }

  if (class(x)[1] %in% c("data.frame", "matrix")) {
    split <- as.matrix(x[,1:2, drop = FALSE])
  }

  # Creating the new column to receive the prepositions
  split <- cbind(split, "")

  # Building the patterns for the search and isolation of prepositions
  patt <- paste0(c(paste0(paste0("^", preps, " "), collapse = "|"),
                   paste0(paste0(" ", preps, "$"), collapse = "|"),
                   paste0(paste0("^", preps, "$"), collapse = "|")), collapse = "|")
  patt.kp <- paste0("(", patt, ")(*SKIP)(*FAIL)|.")

  # Finding and isolating PaP from last or middle names
  ids2 <- grepl(patt, split[,2],
                perl = TRUE, ignore.case = TRUE)
  if (any(ids2)) {
    split[,3][ids2] <- gsub(patt.kp, "", split[,2][ids2],
                            perl = TRUE, ignore.case = TRUE)
    split[,2][ids2] <- gsub(patt, "", split[,2][ids2],
                            perl = TRUE, ignore.case = TRUE)
  }

  ids1 <- grepl(patt, split[,1],
                perl = TRUE, ignore.case = TRUE)
  if (any(ids1)) {
    split[,3][ids1] <- gsub(patt.kp, "", split[,1][ids1],
                            perl = TRUE, ignore.case = TRUE)
    split[,1][ids1] <- gsub(patt, "", split[,1][ids1],
                            perl = TRUE, ignore.case = TRUE)
  }

  # Some minor edits
  split[,2][is.na(split[,2])] <- ""

  patt.rm <- paste0(paste0(" ", preps, " "), collapse = "|")
  split[,2] <- gsub(patt.rm, " ", split[,2],
                    perl = TRUE, ignore.case = TRUE)

  split[,3] <- tolower(split[,3])
  colnames(split) <- c("last.name", "first.names", "prep")

  # Organizing the output
  if (rm.prep) {

    if (output == "matrix") {
      result <- split[,1:2, drop = FALSE]
      return(result)
    }

    if (output == "vector") {
      if (format %in% c("last_init","last_init_prep","prep_last_init"))
        result <- paste(split[,1], split[,2], sep = ", ")

      if (format == "init_last")
        result <- paste(split[,2], split[,1], sep = " ")
    }

  } else {

    if (output == "matrix") {
      split[,3] <- stringr::str_trim(split[,3])
      return(split)
    }

    if (output == "vector") {
      if (format == "last_init_prep")
        result <- paste(paste(split[,1], split[,2], sep = ", "), split[,3],
                        sep = " ")

      if (format == "prep_last_init")
        result <- paste(split[,3], paste(split[,1], split[,2], sep = ", "),
                        sep = " ")

      if (format == "last_init")
        result <- paste(paste(split[,1], split[,2], sep = ", "), split[,3],
                        sep = " ")

      if (format == "init_last")
        result <- paste(split[,2], split[,3], split[,1], sep = " ")
    }
  }

  # Final edits
  result <- stringr::str_trim(result)
  result <- gsub(",$", "", result, perl = TRUE)
  result <- gsub("\\s\\s+", " ", result)

  return(result)
}
