#' @title Capitalize Name
#'
#' @description Capitalize the first letter of people's (last) names.
#'
#' @param x the character string or vector to be capitalized.
#'
#' @return the character string equal to \code{x} with the first letter
#'   capitalized.
#'
#' @details The function works for simple names and compound names that are
#'   separated by a space or by the character '-'. It also works for names that
#'   are entirely capitalized.
#'
#'   This function was conceived to edit people's last names. So, the function
#'   only works for up to two names and it works only partially for names
#'   containing initials. For strings with more than two names, the function
#'   `str_to_title()` from package `stringr::str_to_title` can be used.
#'
#' @author Renato A. F. de Lima
#'
#' @export capName
#'
#' @examples
#' # Simple names
#'   capName("gentry")
#'   capName("HATSCHBACH")
#'   capName(c("gentry", "HATSCHBACH"))
#'
#' # Names with generational suffixes
#'   capName("leitao filho")
#'
#' # Compound last names
#'   capName("saint-hilaire")
#'
#' # Full names (does not work)
#'   capName("hermogenes leitao filho")
#'   capName("auguste saint-hilaire")
#'
#' # Names with initials (does not work completely)
#'   capName("a.h. gentry")
#'
capName <- function(x) {

  ids.sp <- grepl(' ', x, fixed = TRUE)
  ids.hi <- grepl('-', x, fixed = TRUE)

  split <- strsplit(x, split = ' |-', perl = TRUE)
  split <- t(sapply(split, `length<-`, 2))
  split[is.na(split)] <- ""

  first <- paste0(toupper(substring(split[,1], 1, 1)),
                  tolower(substring(split[,1], 2)))

  mid <- rep("", length(x))
  mid[ids.sp] <- " "
  mid[ids.hi] <- "-"

  second <- paste0(toupper(substring(split[,2], 1, 1)),
                   tolower(substring(split[,2], 2)))

  paste(first, mid, second, sep = "")
}
