#' @title Create Name Initials
#'
#' @description Convert or standardize a vector of containing people's names
#' or their initials.
#'
#' @param x the character string or vector to be standardized
#'
#' @return the character string with the initials of each name separated by
#'   points, without spaces.
#'
#' @details The function has some basic assumptions in order to get initials for
#'   the most type of cases.
#'
#'   For multiple names/abbreviations separated by a space and/or an
#'   abbreviation point, the function takes the first letter of each
#'   name/abbreviation as the initials.
#'
#'   For single names or one-string initials the output depends on the presence
#'   of abbreviation points and if names are provided in all caps.
#'
#'
#'
#' The output is relatively stable regarding different name formats and notation
#' standards, but it doe not work for all of them (see Examples).
#'
#' @author Renato A. F. de Lima
#'
#' @export getInit
#'
#' @examples
#'   # Full names and both full and abbreviated names
#'   getInit("Alwyn")
#'   getInit("Alwyn Howard Gentry")
#'   getInit("Alwyn H. Gentry")
#'   getInit("A. Gentry")
#'   getInit("A. H. Gentry")
#'   getInit("A.H.Gentry")
#'
#'   # Abbreviations
#'   getInit("A")
#'   getInit("A H G")
#'   getInit("A. H. G.")
#'
#'   # Capitalized and lower-case names
#'   getInit("ALWYN HOWARD GENTRY")
#'   getInit("AHG")
#'   getInit("a.h. gentry")
#'   getInit("alwyn") # assumes as first name
#'
#'   # Other formats
#'   getInit("Auguste Saint-Hilaire")
#'   getInit("John MacDonald")
#'   getInit("John McDonald")
#'   getInit("John O'Brien")
#'
#'   # Cases when the function does not work properly
#'   getInit("ALWYN") # assumes as initials
#'   getInit("Carl F. P. von Martius") #takes name preposition as name
#'   getInit("AH gentry") # assumes initials as first name
#'   getInit("Gentry, A.") # ignores comma
#'   getInit("G., Alwyn") # ignores comma
#'   getInit("Ah. Gentry") # discard the lower-case initial
#'
#'
getInit <- function(x) {

  #Preparing the vector of names
  pts <- grepl("\\.", x, perl = TRUE)
  x[pts] <- gsub("[.]", ". ", x[pts], perl = TRUE)
  x[pts] <- gsub("\\s\\s+", " ", x[pts], perl = TRUE)
  x[pts] <- gsub("^ | $", "", x[pts], perl = TRUE)

  #Detecting the some general types of name formats: full, abbreviated or both
  words <- grepl(" ", x, fixed = TRUE)
  abrev <- grepl('([a-z;A-Z;à-ý;À-Ý]\\.)([a-z;A-Z;à-ý;À-Ý]\\.)+',
                 x, perl = TRUE)

  types <- rep(NA, length(x))
  types[words] <- "1"
  types[!words & abrev] <- "2"
  types[!words & !abrev] <- "3"

  #Identifying some particular cases
  hyphen <- grepl("-", x, fixed = TRUE)
  if (any(hyphen))
    x[hyphen] <- gsub("-", " - ", x[hyphen], fixed = TRUE)

  oa <- grepl("O'", x, fixed = TRUE)
  if (any(oa))
    x[oa] <- gsub("O'", "O ' ", x[oa], fixed = TRUE)

  mac <- grepl('(Mac)([A-Z])', x, perl = TRUE)
  if (any(mac))
    x[mac] <- gsub("(Mac)([A-Z])", "\\1 \\2", x[mac], perl = TRUE)

  mc <- grepl('(Mc)([A-Z])', x, perl = TRUE)
  if (any(mc))
    x[mc] <- gsub("(Mc)([A-Z])", "\\1 \\2", x[mc], perl = TRUE)

  #Extracting the initials for each type of format
  initials <- as.list(x)

  #Extract types 1: first letter of each word
  if (any(types %in% "1")) {
    split1 <- strsplit(x[types %in% "1"], " ", fixed = TRUE)
    initials[types %in% "1"] <- lapply(split1,
                                       function(x) substr(x, 1, 1))
  }

  #Extract type 2: single words, with abbreviations
  if (any(types %in% "2")) {
    split1 <- strsplit(x[types %in% "2"], "", fixed = TRUE)
    initials[types %in% "2"] <- lapply(split1,
                                       function(x) x[!x %in% c("."," ")])
  }

  #Extract type 3: single words, no abbreviations
  if (any(types %in% "3")) {
    split1 <- strsplit(x[types %in% "3"], "", fixed = TRUE)
    any.caps <- grepl('[A-ZÀ-Ý]', x[types %in% "3"], perl = TRUE)
    all.caps <- sapply(split1, function(x) all(grepl("[[:upper:]]",
                                                     x[grepl("[[:alpha:]]|-", x, perl = TRUE)], perl = TRUE)))
    all.low <- !all.caps & !any.caps

    initials[types %in% "3"][!all.low] <- lapply(split1[!all.low],
                                                 function(x) x[grepl("[A-ZÀ-Ý]|-|\\'", x, perl = TRUE)])

    hyphen <- grepl(" - ", initials[types %in% "3"][all.low],
                    fixed = TRUE)
    if (any(hyphen))
      initials[types %in% "3"][all.low][hyphen] <-
      sapply(initials[types %in% "3"][all.low][hyphen],
             strsplit, " ", fixed = TRUE)
    initials[types %in% "3"][all.low] <- lapply(initials[types %in% "3"][all.low],
                                                function(x) substr(x, 1, 1))
  }

  #Preparing the initials
  initials <- lapply(initials, toupper)

  numb.inits <- lengths(gregexpr("[A-Z;À-Ý]", initials, perl = TRUE))
  x[numb.inits > 1] <-
    sapply(initials[numb.inits > 1], function(x)
      paste0(paste0(x, collapse = "."), "."))
  x[numb.inits == 1] <-
    paste(initials[numb.inits == 1], ".", sep = "")

  # Final edits
  x <- gsub("\\.,\\.", ".", x, perl = TRUE)

  if (any(grepl("-\\.", x, perl = TRUE)))
    x[grepl("-\\.", x, perl = TRUE)] <-
    gsub("-\\.", "-", x[grepl("-\\.", x, perl = TRUE)])

  if (any(oa))
    x[oa] <- gsub("O\\.'\\.", "O'", x[oa], perl = TRUE)

  if (any(mac))
    x[mac] <- gsub("(M)(\\.)", "\\Mac", x[mac], perl = TRUE)

  if (any(mc))
    x[mc] <- gsub("(M)(\\.)", "\\Mc", x[mc], perl = TRUE)

  return(unlist(x))
}
