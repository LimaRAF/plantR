#' @title Get Administrative Subdivisions from a Gazetteer
#'
#' @description Obtain the names of the administrative subdivisions for a given
#'   locality from a \href{https://en.wikipedia.org/wiki/Gazetteer}{gazetteer}.
#'
#' @param x a dataframe or character vector formatted according to the __plantR__
#' format (lowercase, separated by an underline). If x is a data frame, this
#' column has to be named `loc.correct`
#' @param admin.names a data frame containing the gazetteer with the list of names
#' and locality subdivisions. The default is "plantR", the internal __plantR__
#' gazetteer (focusing on Latin America and the Caribbean)
#'
#' @return A data frame containing the input locality string in the first
#'   column, the administrative names at the resolution retrieved (four
#'   following columns) and the source of the administrative names (last
#'   column)
#'
#' @details Although the internal gazetteer used in __plantR__ can handle common
#'   notation variants and typos in locality names (see function `getLoc`), the
#'   function `getAdmin` assumes that these issues were already solved.
#'   Otherwise, the function may not find the information required. See examples
#'   below
#'
#'   The retrieval of information depends on the completeness of the gazetteer
#'   itself. So, even if all variants and typos were solved for an existing
#'   locality, if the gazetteer does not include a specific locality, the query
#'   will return NAs. The gazetteer is permanently being improved. If you found
#'   an error or want to contribute with region-specific gazetteers, please send
#'   an email to <renato.lima@naturalis.nl>.
#'
#'   In Brazil, NAME_0, NAME_1, NAME_2, AND NAME_3 correspond to the Country,
#'   State, Municipality and Locality, respectively. But this may not be the
#'   case of other countries if they adopt a different nomenclature. For
#'   instance, Argentina uses Province instead of State and Department instead
#'   of Municipality.
#'
#'   Finally, function `getAdmin` currently does not return information below
#'   the municipality level, although the default __plantR__ gazetteer can retrieve
#'   information at lower administrative levels (i.e. locality and sub-locality)
#'
#' @author Renato A. F. de Lima
#'
#' @examples
#' str <- c("paraguay_paraguari",
#'          "brazil_parana_paranagua",
#'          "brazil_rio janeiro_parati", # an example of a variant in the locality name
#'          "brazil_rio janeiro_paraty",
#'          "brazil_sao paulo_sao miguel arcanjo_pe carlos botelho",
#'          "united states_florida") # valid location but not in the default gazetteer
#' getAdmin(str)
#'
#' @importFrom dplyr left_join
#'
#' @export getAdmin
#'
getAdmin <- function(x, admin.names = "plantR") {

  ## check input:
    if (class(x) == "data.frame") {
      if (!any(grepl("^loc.correct", names(x))))
        stop("input object needs to have a column loc.correct with the locality strings")
      x1 <- x[my.tail(grep("^loc.correct", names(x)))]
      names(x1) <- gsub('[0-9]', '', names(x1))
    } else {
        x1 <- data.frame(loc.correct = x, stringsAsFactors = FALSE)
    }

  ## Getting the administrative levels
    if (all(admin.names %in% c("plantR","plantr"))) {
      dic <- admin
    } else {
      dic <- admin.names
    }

  ## Crossing the strings with the administrative levels
    tmp <- dplyr::left_join(x1, dic, by = "loc.correct")

  ## Second try, after excluding the 4th adm. level
    if (any(is.na(tmp$NAME_0) & is.na(tmp$NAME_1))) {
      tmp1 <- tmp[is.na(tmp$NAME_0) & is.na(tmp$NAME_1), "loc.correct"]
      tmp1 <- data.frame(loc.correct =
                sapply(
                  strsplit(tmp1, "_"),
                      function(x) paste0(rm.tail(x), collapse = "_")
      ), stringsAsFactors = FALSE)
      tmp1 <- dplyr::left_join(tmp1, dic, by = "loc.correct")
      tmp[is.na(tmp$NAME_0) & is.na(tmp$NAME_1), ] <- tmp1
    }

    ## Third try, after excluding the 3rd and 4th adm. level
    if (any(is.na(tmp$NAME_0))) {
      tmp1 <- tmp[is.na(tmp$NAME_0), "loc.correct"]
      tmp1 <- data.frame(loc.correct =
                           sapply(
                             strsplit(tmp1, "_"),
                             function(x) paste0(rm.tail(x, 1), collapse = "_")
                           ), stringsAsFactors = FALSE)
      tmp1 <- dplyr::left_join(tmp1, dic, by = "loc.correct")
      tmp[is.na(tmp$NAME_0), ] <- tmp1
    }

  ## Filtering the result and returning
    result <- tmp[ ,c("loc.correct","NAME_0","NAME_1","NAME_2","NAME_3","source")]
    return(result)
}
