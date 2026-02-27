#' @title Remove Unwanted Spaces
#'
#' @param x a character or vector
#'
#' @return the character `x` without trailing or double spaces
#'
#' @keywords internal
#'
#' @noRd
#'
#' @author Renato A. F. de Lima
#'
#' @examples
#' \dontrun{
#' squish(" Lindsaea   lancea ")
#' }
#'
squish <- function (x) {

  x <- gsub("\\s\\s+", " ", as.character(x), perl = TRUE)
  x <- gsub("^ | $", "", x, perl = TRUE)

  return(x)
}
#'
#' @title Replace Whitespaces
#'
#' @description Replaces all types of whitespace characters (e.g.
#'   non-breaking, thin, hair) into a regular space character (i.e.
#'   "\U0020")
#'
#' @param x a vector of characters (e.g. taxon names)
#'
#' @return the input `x` with spaces standardized
#'
#' @keywords internal
#'
#' @author Renato A. F. de Lima
#'
#' @noRd
#'
fixSpaces <- function(x) {

  space.codes <- c("\U0020", "\U00A0", "\U2000", "\U2001", "\U2002",
                   "\U2003", "\U2004", "\U2005", "\U2006", "\U2007",
                   "\U2008", "\U2009", "\U200A", "\U205F", "\U3000")

  space.patt <- paste(space.codes, collapse = "|")

  x1 <- gsub(space.patt, " ", x, perl = TRUE)

  return(x1)
}
#' @title Display Progress Bar
#'
#' @description
#' A simple function to display the status of parallel processing.
#' Function adapted from ConR R package and written by Gilles Dauby.
#'
#' @param show.progress logical
#' @param max.pb integer
#'
#' @noRd
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @keywords internal
#'
progressBar <- function(show.progress = TRUE, max.pb) {
  if (show.progress) {
    pb <- utils::txtProgressBar(min = 0,
                     max = max.pb,
                     style = 3)

    progress <- function(n)
      utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
  } else {
    opts <- NULL
    pb <- NA
  }
  return(list(opts = opts, pb = pb))
}
#' @title Collapse Non-Empty Strings
#'
#' @description
#' A simple function, similar to the base function `paste0()` but
#' avoiding problems related to empty strings.
#'
#' @param x one or more objects to be collapsed
#' @param collapse character string to separate the result
#'
#' @noRd
#'
#'
#' @keywords internal
#'
paste1 <- function(x, sep = "", collapse = NULL) {

  x1 <- x[!x %in% c("", " ", NA)]
  x1 <- paste(x1, sep = sep, collapse = collapse)

  return(x1)
}
#'
#' @title Read File in Help
#'
#' @param file a path to the file with the script to be read
#' @param text a character or vector of additional text to be added to
#'   the tag
#' @param tag one of Roxygen's documentation tag. Defaults to 'note'.
#'
#' @keywords internal
#'
#' @noRd
#'
readScript <- function(file = NULL, text = "", tag = "note") {
  lns <- readLines(file)
  lns <- paste(sprintf("\\code{%s}", lns), collapse = "; ")
  return(paste("\\", tag, "{", text, "\n", lns, ".}", sep = ""))
}

#'
#' @title Check plantR Reserved Column Names
#'
#' @param x a data frame
#' @param group the group of column names (i.e. "format.occs",
#'   "format.locs", etc)
#'
#' @keywords internal
#'
#' @noRd
#'
checkColNames <- function(x = NULL, group = NULL) {
  reserved <- reservedColNames

  if (!is.null(group))
    reserved <- reserved[names(reserved) %in% group]

  check_names <- names(x) %in% reserved

  if (any(check_names)) {
    any.reserved <- names(x)[check_names]
    warning(paste("The following column names are reserved to plantR and will be overwritten: ",
                    paste(any.reserved, collapse = ", ")), call. = FALSE)
    x1 <- x
    for (i in seq_along(any.reserved))
      x1[[any.reserved[i]]] <- NULL
    return(x1)
  } else {
    return(x)
  }
}

#'
#' @title Get Duplicate Merge Categories
#'
#' @param x a data frame or a data table
#' @param dup.name character. The name of column in the input data
#'   frame with the duplicate group ID. Default to the __plantR__
#'   output 'dup.ID'.
#' @param prop.name character. The name of column in the input data
#'   frame with the proportion of duplicates found within the group
#'   ID. Default to the __plantR__ output 'dup.prop'.
#' @param prop numerical. The threshold value of proportion of
#'   duplicated values retrieved (i.e. dup.prop) to enter the merging
#'   routine. Should be between zero and one. Default to 0.75.
#' @param rec.ID character. The name of the columns containing the
#'   unique record identifier (see function `getTombo()`). Default to
#'   'numTombo'.
#'
#' @returns a data table with an extra column called 'dup.merge'
#'   containing the merge categories
#'
#' @details
#' The merge category is a logical vector in which TRUE means that the
#' records has a value of duplicated proportion (given in `prop.name`)
#' equal or above the threshold defined in `prop` or records with
#' duplicated proportion below the threshold but that have duplicated
#' catalog numbers within the duplicated IDs defined in the argument
#' `dup.name`.
#'
#' @keywords internal
#'
#' @importFrom data.table as.data.table setnames
#'
#' @noRd
#'
#' @examples
#' df <- data.frame(numTombo = c("a1","b2","c3","c3","d5","d5","e7","f4","g9"),
#'                   dup.ID = c("a1|b2","a1|b2","c3|c3","c3|c3","d5|d5|e7",
#'                              "d5|d5|e7","d5|d5|e7","f4",NA),
#'                   dup.prop = c(1, 1, 1, 1, 0.5, 1, 1, 1, NA))
#' getMergeCat(df)
#'
getMergeCat <- function(x = NULL,
                        dup.name = "dup.ID",
                        prop.name = "dup.prop",
                        prop = 0.75,
                        rec.ID = "numTombo")
  {

  if (!inherits(x, c("data.frame", "data.table")))
    stop("Input object needs to be a data frame or a data table!")

  if (inherits(x, "data.frame")) {
    dtb <- data.table::as.data.table(x)
  } else {
    dtb <- x
  }

  #Checking essential columns
  if (!dup.name %in% names(dtb))
    stop("Classification requires a column with the duplicate group ID")

  if (!prop.name %in% names(dtb)) {
    warning("Classification requires a column with the proportion of duplicates. Assuming to be 1")
    dtb[, c(prop.name) := 1]
  }

  if (!rec.ID %in% names(dtb)) {
    warning("Classification requires a column with the unique record identifier. Creating one")
    dtb[, c(rec.ID) := .I]
  }

  # if (any(dtb[[prop.name]] > 1 | dtb[[prop.name]] < 0, na.rm = TRUE))
  #     stop("Values provided in 'prop.name' must be between 0 and 1")

  # Creating the temporary columns
  dup.merge <- dup.rec.ID.wk <- temp.num.tombo.wk <- NULL
  wk.cols <- c("temp.dup.ID.wk", "temp.dup.prop.wk", "temp.num.tombo.wk")
  data.table::setnames(dtb, c(dup.name, prop.name, rec.ID), wk.cols)

  # Creating the duplicate categories
  dtb[, dup.merge := .SD >= prop, .SDcols = "temp.dup.prop.wk"][]

  # Making sure that duplicated catalog numbers stay in their dup.ID
  dtb[, dup.rec.ID.wk := duplicated(temp.num.tombo.wk) |
                        duplicated(temp.num.tombo.wk, fromLast = TRUE),
     by = "temp.dup.ID.wk"][]
  dtb[dup.rec.ID.wk & !dup.merge, dup.merge := TRUE, by = "temp.dup.ID.wk"][]
  dtb[, dup.rec.ID.wk := NULL][]

  data.table::setnames(dtb, c(wk.cols, "dup.merge"),
                       c(dup.name, prop.name, rec.ID, "dup.merge"))
  return(dtb)

}

