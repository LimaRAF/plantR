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
#'   containing initials.
#'
#'   For strings with more than two names, the function `str_to_title()` from
#'   package `stringr` can be used instead.
#'
#' @author Renato A. F. de Lima
#'
#' @keywords internal
#'
#' @examples
#'
#' \dontrun{
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
#' # Other name formats
#'   capName("o'brien")
#'   capName("john o'brien")
#'   capName("MacDonald")
#'   capName("john MacDonald")
#'
#' # Full (>2) names (does not work properly)
#'   capName("hermogenes leitao filho")
#'   capName("auguste saint-hilaire")
#'   capName("a.h. gentry")
#'   capName("AH gentry")
#'  }
#'
capName <- function(x) {

  ids.sp <- grepl(' ', x, fixed = TRUE)
  ids.hi <- grepl('-', x, fixed = TRUE)
  ids.oa <- grepl("O'|o'", x, perl = TRUE)
  ids.mac <- grepl('(Mac)([A-Z])', x, perl = TRUE)
  ids.mc <- grepl('(Mc)([A-Z])', x, perl = TRUE)

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

  cap.name <- paste(first, mid, second, sep = "")

  if (any(ids.oa))
    cap.name[ids.oa] <- gsub("(O')([a-z])|(o')([a-z])", "\\U\\1\\U\\2",
                             cap.name[ids.oa], perl = TRUE)

  if (any(ids.mac))
    cap.name[ids.mac] <- gsub("(Mac)([a-z])", "\\1\\U\\2",
                              cap.name[ids.mac], perl = TRUE)

  if (any(ids.mc))
    cap.name[ids.mc] <- gsub("(Mc)([a-z])", "\\1\\U\\2",
                             cap.name[ids.mc], perl = TRUE)

  return(cap.name)
}
