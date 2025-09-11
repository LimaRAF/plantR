#' @title Format State Names
#'
#' @description Simple function to standardize the notation of state,
#'   department and/or province name (administrative level 1) by
#'   converting codes to their long names and by removing special
#'   characters and some prepositions and separators.
#'
#' @param x a vector of a data frame with state names to be standardized
#' @param state.name character. The name of the column containing the
#'   state information. Defaults to "stateProvince" following the DWC
#'   standard.
#' @param country.name character. The name of the column containing the
#'   country information. Defaults to "country" following the DWC
#'   standard.
#' @param to.lower logical. Should the output names be return in lower
#'   cases? Default to TRUE.
#' @param special.char logical. Should special characters be
#'   maintained? Default to FALSE.
#' @param rm.abbrev logical. Should common name abbreviation be
#'   replaced? Default to TRUE.
#'
#' @return The input vector \code{x} in the standard name notation
#'   (see Details)
#'
#' @details State information is formatted into a standard notation
#'   require by `plantR`. By default, all letters are
#'   lower-cased (argument `to.lower`) and special characters
#'   (argument `special.char`) and common abbreviations (e.g. 'st.')
#'   are removed (argument `rm.abbrev`). These edits aim at reducing
#'   possible variation in state name notation and facilitate
#'   further data processing and comparison within the __plantR__
#'   workflow, increasing the chances of match against the
#'   package internal gazetteer.
#'
#'   All state information with less than four letters are treated
#'   as state codes or abbeviations and they are converted to the long format.
#'
#' @author Renato A. Ferreira de Lima
#'
#' @keywords internal
#'
#' @examples
#' # Creating a data frame with locality information
#' estados <- c("RJ", "Rio de Janeiro", "MG", "estado de Minas Gerais",
#'              "Minas Gerais state", "state of Minas Gerais", "Minas Geraes",
#'              NA, "", "Minas Gerais", "MI")
#' paises <- c(rep("Brazil", 9), "", "Argentina")
#' df <- data.frame(country = paises, stateProvince = estados)
#'
#' # Formating the locality information
#' prepState(df)
#' prepState(df, to.lower = FALSE)
#' prepState(df, rm.abbrev = FALSE)
#'
#' @export
#'
prepState <- function(x,
                      state.name = "stateProvince",
                      country.name = "country",
                      to.lower = TRUE,
                      special.char = FALSE,
                      rm.abbrev = TRUE)
{

  x0 <- x

  if (inherits(x, "character")) {
    x <- data.frame(x, check.names = FALSE, fix.empty.names = FALSE)
    colnames(x) <- state.name
  }

  if (!inherits(x, "data.frame"))
    stop("Input object needs to be a vector or data frame!",
         call. = FALSE)

  if (dim(x)[1] == 0)
    stop("Input data frame is empty!", call. = FALSE)

  if (!state.name %in% names(x))
    stop("Input data frame must have a column named: ", state.name,
         call. = FALSE)

  if (!country.name %in% names(x)) {
    x[[country.name]] <- NA
  } else {
    rep_these <- x[[country.name]] %in% c("", " ", "NULL")
    if (any(rep_these))
      x[[country.name]][rep_these] <- NA
  }

  rep_these <- x[[state.name]] %in% c("", " ", "NULL")
  if (any(rep_these))
    x[[state.name]][rep_these] <- NA

  # Identifying NAs
  check_these <- !is.na(x[[state.name]])
  if (any(check_these)) {
    x1 <- x[check_these, ]

    # Standardizing the notation
    patt <- "^estado d[eo]|^state of|^provincia de|^provincia of|^province de|^departamento de"
    x1[[state.name]] <- gsub(patt, "", x1[[state.name]],
                             perl = TRUE, ignore.case = TRUE)
    patt <- ".* estado d[eo]|.* state of|.* provincia de|.* provincia of|.* province de|.* departamento de"
    x1[[state.name]] <- gsub(patt, "", x1[[state.name]],
                             perl = TRUE, ignore.case = TRUE)

    patt <- "estado |state |provincia |departamento |province "
    x1[[state.name]] <- gsub(patt, "", x1[[state.name]],
                             perl = TRUE, ignore.case = TRUE)
    patt <- "state$| provincia$| province$"
    x1[[state.name]] <- squish(gsub(patt, "", x1[[state.name]],
                                    perl = TRUE, ignore.case = TRUE))

    dic <- replaceNames
    tmp1 <- dic[dic$class %in% "stateProvince" & !is.na(dic[ ,2]),]
    tmp1.1 <- strsplit(tmp1$pattern, "\\|", perl = TRUE)
    names(tmp1.1) <- tmp1$replace
    codes <- lapply(lapply(tmp1.1, function(x) x[nchar(x) <= 4]),
                    paste, collapse = "|")
    is.code <- !codes %in% c("", NA)
    codes <- codes[is.code]
    loc.names <- lapply(lapply(tmp1.1, function(x) x[nchar(x) > 4]),
                        paste, collapse = "|")
    is.name <- !loc.names %in% c("", NA)
    loc.names <- loc.names[is.name]

    # Converting any state codes into long names
    few_letters <- nchar(x1[[state.name]]) < 4
    if (any(few_letters)) {
      x2 <- x1[few_letters, , drop = FALSE]

      tmp2 <- names(codes)
      names(tmp2) <- unlist(codes)
      names(tmp2) <- gsub('\\.', "\\\\.", names(tmp2), perl = TRUE)
      cond0 <- unique(tmp1$condition0[is.code])

      available_patts <- gsub("\\^|\\$","", unlist(codes), perl = TRUE)
      available_patts <- unique(unlist(strsplit(available_patts, "\\|",
                                                perl = TRUE)))
      check_these <- tolower(x2[[state.name]]) %in% available_patts
      if (any(check_these)) {
        if (country.name %in% names(x2)) {
          x3 <- x2[check_these, , drop = FALSE]
          x3[[country.name]] <- tolower(x3[[country.name]])
          if (any(cond0 %in% unique(x3[[country.name]]))) {
            cond1 <- cond0[cond0 %in% unique(x3[[country.name]])]
            for (i in seq_along(cond1)) {
              tmp2.i <- tmp2[tmp1$condition0 %in% cond1[i]]
              rep.i <- tolower(x3[[state.name]][x3[[country.name]] %in% cond1[i]])
              rep.i <- stringr::str_replace_all(rep.i, tmp2.i)
              x3[[state.name]][x3[[country.name]] %in% cond1[i]] <-
                rep.i
            }
            x1[[state.name]][few_letters][check_these] <-
              x3[[state.name]]
          }
        }
      }
    }

    #  Replacing variants, typos, and non-standard names for other cases
    if (any(!few_letters)) {
      x2 <- x1[!few_letters, , drop = FALSE]

      tmp2 <- names(loc.names)
      names(tmp2) <- unlist(loc.names)
      names(tmp2) <- gsub('\\.', "\\\\.", names(tmp2), perl = TRUE)
      cond0 <- unique(tmp1$condition0[is.name])

      available_patts <- gsub("\\^|\\$","", unlist(loc.names), perl = TRUE)
      available_patts <- unique(unlist(strsplit(available_patts, "\\|",
                                                perl = TRUE)))
      check_these <- tolower(x2[[state.name]]) %in% available_patts
      if (any(check_these)) {
        if (country.name %in% names(x2)) {
          x3 <- x2[check_these, , drop = FALSE]
          x3[[country.name]] <- tolower(x3[[country.name]])
          if (any(cond0 %in% unique(x3[[country.name]]))) {
            cond1 <- cond0[cond0 %in% unique(x3[[country.name]])]
            for (i in seq_along(cond1)) {
              tmp2.i <- tmp2[tmp1$condition0 %in% cond1[i]]
              rep.i <- tolower(x3[[state.name]][x3[[country.name]] %in% cond1[i]])
              rep.i <- stringr::str_replace_all(rep.i, tmp2.i)
              x3[[state.name]][x3[[country.name]] %in% cond1[i]] <-
                rep.i
            }
            x1[[state.name]][!few_letters][check_these] <-
              x3[[state.name]]
          }
        }
      }
    }

    # Replacing '&' by 'and' in compound country names
    rep_these <- grep(" & ", x1[[state.name]], fixed = TRUE)
    if (length(rep_these) > 0L)
      x1[[state.name]][rep_these] <-
        gsub(" & ", " and ", x1[[state.name]][rep_these], fixed = TRUE)

    # Replacing abbreviated names
    if (rm.abbrev) {
      rep_these <- grep(".", x1[[state.name]], fixed = TRUE)
      if (length(rep_these) > 0L) {
        rep.i <- x1[[state.name]][rep_these]
        if (to.lower) {
          rep.i <- gsub("^st\\.\\s|^st\\s", "saint ", rep.i,
                        perl = TRUE, ignore.case = TRUE)
          rep.i <- gsub("\\sI\\.", " island", rep.i,
                        perl = TRUE, ignore.case = TRUE)
          rep.i <- gsub("\\sIs\\.", " islands", rep.i,
                        perl = TRUE, ignore.case = TRUE)
        } else {
          rep.i <- gsub("^st\\.\\s|^st\\s", "Saint ", rep.i,
                        perl = TRUE, ignore.case = TRUE)
          rep.i <- gsub("\\sI\\.", " Island", rep.i,
                        perl = TRUE, ignore.case = TRUE)
          rep.i <- gsub("\\sIs\\.", " Islands", rep.i,
                        perl = TRUE, ignore.case = TRUE)
        }
        x1[[state.name]][rep_these] <- rep.i
      }
    }

    # Removing some separators and prepositions from country names
    x1[[state.name]] <- gsub("-", " ", x1[[state.name]],
                             fixed = TRUE)
    x1[[state.name]] <- squish(x1[[state.name]])
    x1[[state.name]] <- gsub(" of ", " ", x1[[state.name]],
                             fixed = TRUE)
    x1[[state.name]] <- gsub(" and ", " ", x1[[state.name]],
                             fixed = TRUE)

    # Removing unwanted characters
    if (!special.char)
      x1[[state.name]] <- rmLatin(x1[[state.name]])

    # Lower-casing
    if (to.lower) {
      x1[[state.name]] <- tolower(x1[[state.name]])
    } else {
      rep_these <- x1[[state.name]] == tolower(x1[[state.name]])
      if (any(rep_these)) {
        x1[[state.name]][rep_these] <-
          gsub("\\b(\\p{Ll})", "\\U\\1", x1[[state.name]][rep_these],
               perl = TRUE)
      }
    }

    # Writing the results
    x[[state.name]][!is.na(x[[state.name]])] <- x1[[state.name]]

    if (inherits(x0, "character"))
      return(x[[state.name]])

    if (inherits(x0, "data.frame"))
      return(x)

  } else {
    return(x0)
  }
}
