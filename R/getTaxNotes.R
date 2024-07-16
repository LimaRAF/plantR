#' @title Get Taxonomic Notes
#'
#' @description Creates more user-friendly categories regarding the
#'   taxonomic and nomenclature status of taxon names based on the
#'   output of the taxon name checking process.
#'
#' @param x a data frame containing the results of the taxon name
#'   matching againt a reference backbone
#' @param match.cols a vector of column names containing the
#'   information on the name matching proccess. Defaults to the
#'   typical names used internally within the `plantR` function
#'   `prepSpecies()`
#' @param no.match a character indicating the tag for taxon names
#'   without match. Defaults to 'no_match'
#'
#' @return the same input data frame 'x' with an extra column called
#'   'notes'
#'
#' @details The function returns the following categories (or
#'   combination between them separated by a vertical bar): \describe{
#' \item{\code{accepted}}{A valid and accepted taxon name}
#' \item{\code{synonym}}{A valid name but that taxon that now goes by
#' a different accepted name}
#' \item{\code{name and/or author misspelled}}{A valid and accepted
#' taxon name that was written with typos on the taxon name and/or
#' authorship}
#' \item{\code{check +1 name}}{A name, generally without authorship
#' information, that has more than one accepted name}
#' \item{\code{check not resolved}}{A name currently not resolved that
#' has an 'unchecked', 'unplaced', 'doubtful' or empty taxonomic
#' status category in the reference backbone}
#' \item{\code{not found}}{A name not found after exact and fuzzy
#' matching, given the suggested maximum distance between input and
#' reference names }
#' \item{\code{bad match}}{A name found after fuzzy matching, but that
#' is finally above the suggested maximum distance (generally in the
#' case of large name + authorship combinations)} }
#'
#' @author Renato A. Ferreira de Lima
#'
#'
#' @importFrom stringdist stringdist
#' @importFrom dplyr left_join
#'
#' @export
#'
getTaxNotes <- function(x = NULL,
                        match.cols = c("suggestedName",
                                       "match_type",
                                       "multiple_match",
                                       "fuzzy_dist_name",
                                       "fuzzy_dist_author",
                                       "name.status",
                                       "taxon.status"),
                        no.match = "no_match") {

  if (!inherits(x, "data.frame"))
    stop("Input object needs to be a data frame!", call. = FALSE)

  if (dim(x)[1] == 0)
    stop("Input data frame is empty!", call. = FALSE)

  if (!any(match.cols %in% names(x))) {
    stop("Input data frame must have all columns listed in 'match.cols'",
         call. = FALSE)
  }

  nomes <- x[[match.cols[1]]]
  match <- x[[match.cols[2]]]
  multm <- x[[match.cols[3]]]
  dname <- x[[match.cols[4]]]
  dauth <- x[[match.cols[5]]]
  nstat <- x[[match.cols[6]]]
  tstat <- x[[match.cols[7]]]

  notes <- rep("", dim(x)[1])

  rep_these <- match %in% no.match | is.na(nomes)
  if (any(rep_these)) {
    notes[rep_these] <- "not found"
    multm[rep_these] <- FALSE
  }

  rep_these <- grepl("^bad_", match, perl = TRUE)
  if (any(rep_these))
    notes[rep_these] <- "bad match"

  rep_these <- nstat %in% "orthographic variant"
  if (any(rep_these))
    notes[rep_these] <- "orthographic variant"

  rep_these <- multm | grepl("\\|", tstat, perl = TRUE)
  if (any(rep_these))
      notes[rep_these] <- "check +1 name"

  rep_these <- !dname == 0 & !dauth == 0 & !is.na(dauth)
  rep_these[is.na(rep_these)] <- FALSE
  if (any(rep_these)) {
    empty <- rep_these & notes %in% ""
    if (any(empty))
      notes[empty] <- "name and author misspelled"
  }

  rep_these <- dname == 0 & !dauth == 0 & !is.na(dauth)
  rep_these[is.na(rep_these)] <- FALSE
  if (any(rep_these)) {
    empty <- rep_these & notes %in% ""
    if (any(empty))
      notes[empty] <- "author misspelled"
  }

  rep_these <- (!dname == 0 & dauth == 0 & !is.na(dauth)) |
                (!dname == 0 & is.na(dauth))
  rep_these[is.na(rep_these)] <- FALSE
  if (any(rep_these)) {
    empty <- rep_these & notes %in% ""
    if (any(empty))
      notes[rep_these] <- "name misspelled"
  }

  rep_these <- tstat %in% "synonym"
  if (any(rep_these)) {
    empty <- notes %in% ""
    if (any(empty))
      notes[rep_these & empty] <- "synonym"
    if (any(!empty))
      notes[rep_these & !empty] <- paste(notes[rep_these & !empty],
                                        "synonym",
                                        sep = "|")
  }

  rep_these <- tstat %in% c("", "unchecked", "unplaced", "doubtful")
  if (any(rep_these)) {
    empty <- notes %in% ""
    if (any(empty))
      notes[rep_these & empty] <- "check not resolved"
    if (any(!empty))
      notes[rep_these & !empty] <- paste(notes[rep_these & !empty],
                                          "check not resolved",
                                          sep = "|")
  }

  rep_these <- tstat %in% "accepted"
  if (any(rep_these)) {
    empty <- notes %in% ""
    if (any(empty))
      notes[rep_these & empty] <- "name accepted"
  }

  rep_these <- grepl("check +1 name|", notes, fixed = TRUE)
  if (any(rep_these))
    notes[rep_these & empty] <- "check +1 name"

  rep_these <- grepl("orthographic variant|", notes, fixed = TRUE)
  if (any(rep_these))
    notes[rep_these & empty] <- "orthographic variant"


  x$notes <- notes




  return(x)
}
