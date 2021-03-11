#' @title Flag Occurrences From Cultivated Individuals
#'
#' @description This function detects records from cultivated individuals,
#' based on record locality, description and habitat.
#'
#' @param x a data frame with the species record data.
#' @param remarks character. The name of the column containing the record
#'   remarks. Default to the Darwin Core standard 'occurrenceRemarks'.
#' @param loc.name character. The name of the column containing the record
#'   locality information. Default to 'locality'.
#' @param habitat character. The name of the column containing the record
#'   habitat information. Default to 'habitat'.
#'
#' @return The input data frame with an additional column 'cult.check'
#' with the result of the search for records from cultivated individuals.
#'
#' @details The input data frame \code{x} should contain at least the columns
#'   containing the description of the record (e.g. 'occurrenceRemarks'), the
#'   record locality description (e.g. 'locality') or the record habitat
#'   description. The names of columns in which these information is stored can
#'   de declared using the arguments `loc.name`, `remarks` and `habitat`
#'   (defaults to the Darwin Core standard notation).
#'
#'   If present, other Darwin Core fields are used internally to obtain missing
#'   information on the three fields declared above, namely: 'verbatimLocality',
#'   'biologicalStatus' and 'fieldNotes'.
#'
#'   The search of records from cultivated individuals is performed on all the
#'   fileds available and it is based on a list of terms that denotes clear
#'   indication of cultivated individuals (e.g. 'Cultivated', 'Planted',
#'   'Exotic'). The function returns the column 'cult.check' with two
#'   categories:
#'   - "cultivated": exact matches of the list of terms of cultivated
#'   individuals with the text in at least one of the fields mentioned above.
#'   - "prob_cultivated": presence of one or more terms in the fields mentioned above.
#'
#'   For assigning the "prob_cultivated", a second list of terms is used to
#'   exclude possible spurious hits of cultivated individuals (e.g 'Cultivated
#'   area' or 'Presence of exotics'). But this list is not extensive, so this
#'   category may need some level of double-checking by the user.
#'
#' @import data.table
#'
#' @examples
#'
#' (df = data.frame(
#'   occurrenceRemarks = c("tree, 10 m", "Frutos Roxos. Cultivada", NA, "Tree"),
#'   locality = c("pastagem cultivada", NA, "Itatiaia, cultivada perto da sede", "Brazil"),
#'   habitat = c(NA, "Floresta", "Mata", "Cultivated"), stringsAsFactors = FALSE))
#'
#' getCult(df)
#'
#'
#' @export getCult
#'
getCult <- function(x, remarks = "occurrenceRemarks", loc.name = "locality", habitat = "habitat") {

  #Avoiding warnings in package check when using data.table
  cult.check <- tmp.ordem <- tmp.vrl <- NULL

  #Checking the input
  if (!class(x) == "data.frame")
    stop("Input object needs to be a data frame!")

  if (!(loc.name %in% names(x) |
        remarks %in% names(x) | habitat %in% names(x)))
    stop("Input data needs at least one of the following: record locality, remarks or habitat")

  # Missing locality that may be stored in the field 'verbatimLocality'
  if (loc.name %in% names(x) & "verbatimLocality" %in% names(x)) {
    ids <- !is.na(x$verbatimLocality) & is.na(x[, loc.name])
    x[, loc.name][ids] <- x$verbatimLocality[ids]
  }

  # Missing occurrence remarks that may be stored in other columns
  if (remarks %in% names(x) & "biologicalStatus" %in% names(x)) {
    ids <- !is.na(x$biologicalStatus) & is.na(x[, remarks])
    x[, remarks][ids] <- x$biologicalStatus[ids]
  }
  if (remarks %in% names(x) & "fieldNotes" %in% names(x)) {
    ids <- !is.na(x$fieldNotes) & is.na(x[, remarks])
    x[, remarks][ids] <- x$fieldNotes[ids]
  }

  #Objects and function needed for the search for cultivated specimens
  cult <- cultivated
  pat.cult <- paste(cult, collapse = "|")
  not.cult <- notCultivated
  pat.not.cult <- paste(not.cult, collapse = "|")
  `%like.ic%` <- function (x, pattern) {
    grepl(pattern, x, perl = TRUE, ignore.case = TRUE)
  }

  ## Obtaining the intermediary data frame for editing
  cols <- c(remarks, loc.name,  habitat)[c(remarks, loc.name,  habitat) %in% colnames(x)]
  x1 <- x[, match(cols, colnames(x)), drop = FALSE]

  ## Flaging records of true and probable cultivated specimens (based on the locality descriptions)
  dt <- data.table::data.table(x1)
  dt[, cult.check := NA_character_]
  dt[, tmp.ordem := .I]

  for (tmp in cols) {
    dt[, tmp.vrl := .SD, .SDcols = c(tmp)]
    data.table::setkey(dt, tmp.vrl)
    dt[, tmp.vrl := tolower(tmp.vrl), by = tmp.vrl]
    dt[tmp.vrl %in% cult, cult.check := "cultivated", by = tmp.vrl]
    dt[is.na(cult.check) &
         tmp.vrl %like.ic% pat.cult &
         !tmp.vrl %like.ic% pat.not.cult,
       cult.check := "prob_cultivated", by = tmp.vrl]
  }

  ## Re-ordering the data and removing temporary columns
  data.table::setorder(dt, tmp.ordem)
  dt[, c("tmp.ordem", "tmp.vrl") := NULL]
  x$cult.check <- as.character(dt$cult.check)

  return(x)
}

