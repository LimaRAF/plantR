#'
#' @title Build Taxon Name
#'
#' @description Combine different columns with species name
#'   information (i.e. genus, epiteth, infra-epiteth, author) into a
#'   single taxon name
#'
#' @param x a data frame with the name information
#' @param col.names the name of the columns containing the information
#'   to be combined in the desired order of output
#'
#' @return a vector with the combined name information in `col.names`
#'
#' @author Renato A. F. de Lima
#'
#' @examples
#' \dontrun{
#' df <- data.frame(genus = c("Aa", "Ab"), specificEpithet = c("aa", "bb"))
#' buildName(df)
#' }
#'
#' @keywords internal
#'
buildName <- function(x, col.names = c("genus", "specificEpithet")) {

  if (!inherits(x, "data.frame"))
    stop("Input object needs to be a data frame!")

  x <- as.data.frame(x)

  if (dim(x)[1] == 0)
    stop("Input data frame is empty!")

  if (any(!col.names %in% colnames(x)))
    stop("One or more names in 'col.names' were not found in 'x'")

  cols <- names(x)[match(col.names, names(x), nomatch = 0)]

  if (length(cols) > 1) {

    for (i in seq_along(cols)) {
      empty.vec <- c("", " ", "NULL", "NA")
      if (any(x[[cols[i]]] %in% empty.vec)) {
        x[[cols[i]]][x[[cols[i]]] %in% empty.vec] <- NA
      }
    }

    name <- do.call(paste, x[, cols])
    rm.na <- paste0(rep(NA, length(cols)), collapse = " ")
    name[name %in% rm.na] <- NA_character_
    name <- gsub("NA\\s+", "", name, perl = TRUE)
    name <- gsub("\\sNA+", "", name, perl = TRUE)

    return(squish(name))
  } else {

    return(squish(x[[cols]]))
  }
}
