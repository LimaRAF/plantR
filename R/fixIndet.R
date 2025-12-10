#'
#' @title Fix Taxon Name Notation
#'
#' @description Standardizes the notation of taxon names with
#'   incomplete identifications (i.e. not at the species level).
#'
#' @param x a vector of characters with taxon names
#' @param rplc a character with the standard to be used as the
#'   replacement for the incomplete identification. Defaults to
#'   "Indet. sp.".
#'
#' @return the vector \code{x} with the standardize notation of the
#'   incomplete species identification
#'
#' @author Renato A. Ferreira de Lima
#'
#' @details The function solve most but not all possible notation of
#'   incomplete identifications, specially when the notation to
#'   separate morphotypes include letters instead of numbers.
#'
#' @examples
#' taxa <- c("Indeterminada1", "Indeterminada sp1", "undeterminedA",
#'   "Fabaceae1", "FabaceaeA", "indet", "Fabales2", "Andira sp. 2",
#'   "Andira sp 2", "Andira sp")
#' fixIndet(taxa)
#'
#' @export fixIndet
#'
fixIndet <- function(x, rplc = "Indet. sp.") {

  indets <- c("indet", "indeterminada", "unclassified", "undetermined")
  x[x %in% indets] <- rplc

  x <- gsub(paste0(paste0("^", indets," (?=[0-9])"), collapse = "|"),
            rplc, x, perl = TRUE, ignore.case = TRUE)
  x <- gsub(paste0(paste0("^", indets,"(?=[0-9])"), collapse = "|"),
            rplc, x, perl = TRUE, ignore.case = TRUE)
  x <- gsub(paste0(paste0("^", indets," sp(?=[0-9])"), collapse = "|"),
            rplc, x, perl = TRUE, ignore.case = TRUE)
  x <- gsub(paste0(paste0("^", indets," sp\\."), collapse = "|"),
            rplc, x, perl = TRUE, ignore.case = TRUE)
  for (i in seq_along(indets[-1]))
    x <- gsub(paste0("^(", indets[-1][i],")([A-Z])"),
              "Indet. sp.\\2", x, perl = TRUE)

  x <- gsub(paste0(paste0("^(", indets[1],")([A-Z])"), collapse = "|"),
            "Indet. sp.\\2", x, perl = TRUE)
  x <- gsub("^sp\\.(?=[0-9])|^sp(?=[0-9])",
            "Indet. sp.", x, perl = TRUE, ignore.case = TRUE)

  # higher <- c("eae", "ales")
  # for (i in seq_along(higher)) {
  #   x <- gsub(paste0("(", higher[i],")([A-Z])"),
  #             "\\1 sp.\\2", x, perl = TRUE)
  #   x <- gsub(paste0("(", higher[i],")([0-9])"),
  #             "\\1 sp.\\2", x, perl = TRUE)
  # }

  x <- gsub(" (sp\\.) ([0-9]+)$", " \\1\\2", x, perl = TRUE)
  x <- gsub(" (sp\\.) ([a-z])$", " \\1\\2", x, perl = TRUE)
  x <- gsub(" (sp\\.) ([A-Z]+)$", " \\1\\2", x, perl = TRUE)
  x <- gsub(" (sp\\.)_([0-9]+)", " \\1\\2", x, perl = TRUE)
  x <- gsub(" (sp) ([0-9]+)$", " \\1.\\2", x, perl = TRUE)
  x <- gsub(" (sp)([0-9]+)$", " \\1.\\2", x, perl = TRUE)
  x <- gsub(" (sp)_([0-9]+)$", " \\1.\\2", x, perl = TRUE)

  x <- gsub("(.* )(\\d{1,3})$", "\\1sp.\\2", x, perl = TRUE)
  x <- gsub("(.*[a-z])(\\d{1,3})$", "\\1 sp.\\2", x, perl = TRUE)

  x <- gsub("(.* )([A-Z])$", "\\1sp.\\2", x, perl = TRUE) # Adapt code to avoid names ending with L but should actually be L.? For now, no
  x <- gsub("(.*[a-z])([A-Z])$", "\\1 sp.\\2", x, perl = TRUE)

  x <- gsub(" sp$", " sp.", x, perl = TRUE)
  x <- gsub(" spp$", " spp.", x, perl = TRUE)
  x <- gsub(" spec$| indet$| sp $| sp \\.$| sp\\. $", " sp.", x,
            perl = TRUE)
  x <- gsub(" sp sp.", " sp.", x, fixed = TRUE)

  x <- squish(x)
  x[x %in% c("", " ", NA)] <- rplc

  return(x)
}
