#' @title Get Administrative Subdivisions from a Gazetteer
#'
#' @description Obtain the names of the administrative subdivisions
#'   for a given locality from a
#'   \href{https://en.wikipedia.org/wiki/Gazetteer}{gazetteer}. Users
#'   can use the default __plantR__ gazetteer or provide one to the
#'   function.
#'
#' @param x a vector or a dataframe containing the __plantR__ search
#'   string in the proper format (lowercase, separated by an
#'   underline).
#' @param gazet a data frame containing the gazetteer with the list of
#'   names and locality subdivisions. The default is "plantR", the
#'   internal __plantR__ gazetteer (focused on Latin America and the
#'   Caribbean).
#' @param str.name a character corresponding to the columns name
#'   containing the locality search string to match the input
#'   dataframe and the gazetteer. Defaults to 'loc.correct'.
#' @param gazet.names a vector of the column names containing the
#'   locality search string in the gazetteer, and the administrative
#'   subdivisions, in that order. If available, the source of the
#'   administrative information can be provided as the last name of
#'   this vector. Defaults to columns names of the __plantR__
#'   gazetteer: 'loc.correct', 'NAME_0', 'NAME_1', 'NAME_2', 'NAME_3
#'   and 'source'.
#'
#' @return A data frame containing the input locality string in the
#'   first column, the administrative names at the resolution
#'   retrieved (four following columns) and the source of the
#'   administrative names (last column)
#'
#' @details Although the internal gazetteer used in __plantR__ can
#'   handle common notation variants and typos in locality names (see
#'   function `getLoc`), the function `getAdmin` assumes that these
#'   issues were already solved. Otherwise, the function may not find
#'   the information required. See Examples below.
#'
#'   The retrieval of information depends on the completeness of the
#'   gazetteer itself. So, even if all variants and typos were solved
#'   for an existing locality, if the gazetteer does not include a
#'   specific locality, the query will return NAs. The gazetteer is
#'   permanently being improved. If you found an error or want to
#'   contribute with region-specific gazetteers, please send an email
#'   to <raflima@usp.br>.
#'
#'   In Brazil, NAME_0, NAME_1, NAME_2, AND NAME_3 correspond to the
#'   Country, State, Municipality and Locality, respectively. But this
#'   may not be the case of other countries if they adopt a different
#'   nomenclature for their administrative levels.
#'
#'   Finally, function `getAdmin` currently does not return
#'   information below the municipality level, although the default
#'   __plantR__ gazetteer can retrieve information at lower
#'   administrative levels (i.e. locality and sub-locality).
#'
#'   A different gazetteer than the default one can be provided using
#'   the argument `gazet`. If the column names of the user-provided
#'   gazetteer do not match those of the default gazetteer, the column
#'   names can be supplied using argument `gazet.names`.
#'
#' @author Renato A. F. de Lima
#'
#' @examples
#' str <- c("paraguay_paraguari",
#'          "brazil_parana_paranagua",
#'          "brazil_rio janeiro_parati", # variant in the locality name
#'          "brazil_rio janeiro_paraty",
#'          "brazil_sao paulo_sao miguel arcanjo_pe carlos botelho",
#'          "united states_florida") # valid location but not in the gazetteer
#' getAdmin(str)
#'
#' @importFrom dplyr left_join
#' @importFrom utils head
#'
#' @export getAdmin
#'
#'
getAdmin <- function(x,
                     gazet = "plantR",
                     str.name = "loc.correct",
                     gazet.names = c("loc.correct", "NAME_0", "NAME_1",
                                    "NAME_2", "NAME_3", "source")) {

  ## check input:
    if (inherits(x, "data.frame")) {
      if (!str.name %in% names(x))
        stop("Input object must have a column containing the locality search string")

      x1 <- x[grep(str.name, names(x))]

    } else {
      x1 <- data.frame(x, stringsAsFactors = FALSE)
      names(x1) <- str.name
    }

  ## Getting the administrative levels
  cols.gazet <- c("loc.correct", "NAME_0",
                  "NAME_1", "NAME_2",
                  "NAME_3", "source")
  class.gazet <- class(gazet)
  dic <- NULL
  if (class.gazet == "character") {
    if (all(gazet %in% c("plantR","plantr"))) {
      dic <- admin
      dic <- dic[, cols.gazet]
    } else {
      stop("Please chose between the default gazetteer or a user-provided one as a data frame")
    }
  }

  if (inherits(class.gazet, "data.frame")) {
    if (all(gazet.names %in% colnames(gazet))) {
      dic <- gazet[match(gazet.names, colnames(gazet))]
      for(i in 1:length(dic)) colnames(dic)[i] <- cols.gazet[i]

      if (!"source" %in% names(dic))
        dic$source <- NA

    } else {
      stop("The gazetteer must contain the columns speciefied in the argument `gazet.names`")
    }
  }

  if (is.null(dic))
    stop("Please chose between the default 'plantR' gazetteer or provide one as a data frame")

  ## Crossing the strings with the administrative levels
    if (!str.name %in% names(dic))
      names(dic)[grepl("loc.correct", names(dic))][1] <- str.name

    tmp <- dplyr::left_join(x1, dic, by = str.name)

  ## Second try, after excluding the 4th adm. level
    if (any(is.na(tmp$NAME_0) & is.na(tmp$NAME_1))) {
      tmp1 <- tmp[is.na(tmp$NAME_0) & is.na(tmp$NAME_1), str.name]
      tmp1 <- data.frame(tmp =
                sapply(
                  strsplit(tmp1, "_"),
                  function(x) paste0(utils::head(x, n = -1), collapse = "_")
                  # function(x) paste0(rm.tail(x), collapse = "_")
      ), stringsAsFactors = FALSE)
      names(tmp1)[1] <- str.name
      tmp1 <- dplyr::left_join(tmp1, dic, by = str.name)
      tmp[is.na(tmp$NAME_0) & is.na(tmp$NAME_1), ] <- tmp1
    }

    ## Third try, after excluding the 3rd and 4th adm. level
    if (any(is.na(tmp$NAME_0))) {
      tmp1 <- tmp[is.na(tmp$NAME_0), str.name]
      tmp1 <- data.frame(tmp =
                           sapply(
                             strsplit(tmp1, "_"),
                             function(x) paste0(utils::head(x, n = -1), collapse = "_")
                             # function(x) paste0(rm.tail(x, 1), collapse = "_")
                           ), stringsAsFactors = FALSE)
      names(tmp1)[1] <- str.name
      tmp1 <- dplyr::left_join(tmp1, dic, by = str.name)
      tmp[is.na(tmp$NAME_0), ] <- tmp1
    }

    ## Filtering the result and returning
    result <-
      tmp[, unique(c(str.name, cols.gazet[cols.gazet %in% names(tmp)]))]
    return(result)
}
