#' @title Get Administrative Names
#'
#' @description Obtain the names of the administrative subdivisions for a given
#'   locality from a \href{https://en.wikipedia.org/wiki/Gazetteer}{gazetteer}.
#'
#' @param x a character or vector containing the `plantR` string to search localty information.
#' @param admin.names a data frame containing the list of names of the locality subdivisions.
#' The default is "plantR", the internal `plantR` gazetteer (biased towards Latin America)
#'
#' @return A data frame containing the input locality string in the first
#'   column, the administrative names at the resolution retrieved (four
#'   following columns) and the source of the administrative names (last
#'   column).
#'
#' @details Although the internal gazetteer used in `plantR` can handle common
#'   notation variants and typos in locality names (see function `getLoc`), the
#'   function `getAdmin` assumes that these issues were already solved.
#'   Otherwise, the function may not find the information required. See examples
#'   below.
#'
#'   The retrieval of information depends on the completeness of the gazetteer
#'   itself. So, even if all variants and typos were solved for an existing
#'   locality, if the gazetteer does not include a specific locality, the query
#'   will return NAs. The gazetteer is permanently being improved. If you found
#'   an error or want to contribute with region-specific gazetteers, please send
#'   an email to <renato.lima@naturalis.nl>.
#'
#'   In Brazil, NAME_0, NAME_1, NAME_2, AND NAME_3 correspond to the Country,
#'   State, Municipality and Locality, respectively. But these may not be the
#'   case of other countries if they adopt a different nomenclature. For
#'   instance, Argentina uses Province instead of State and Department instead
#'   of Municipality.
#'
#'   Finally, function `getAdmin` currently does not returns information below
#'   the municipality level, although the default `plantR` gazetteer can retrieve
#'   information at lower administrative levels (i.e. locality and sub-locality)
#'
#' @author Renato A. F. de Lima
#'
#' @examples
#' getAdmin(x = c("paraguay_paraguari",
#'                "brazil_parana_paranagua",
#'                "brazil_rio janeiro_parati", # an example of a variant in the locality name
#'                "brazil_rio janeiro_paraty",
#'                "brazil_sao paulo_sao miguel arcanjo_pe carlos botelho",
#'                "united states_florida") # existing and valid location but not in the default gazetteer
#'                )
#'
#' @importFrom dplyr left_join
#'
#' @export getAdmin
#'
getAdmin <- function(x, admin.names = "plantR") {

  ## check input:
    if (class(x) == "data.frame") {
      if (!any(grepl("^loc.correct$", names(x)))) { stop("input object needs to have a column loc.correct with the locality strings") }
      x1 = x[which(colnames(x) %in% c("loc.correct"))]
    } else {
      x1 = data.frame(loc.correct = x, stringsAsFactors= FALSE)
    }

  ## Getting the administrative levels
    if(all(admin.names %in% c("plantR","plantr"))) {
      #load("./R/sysdata.rda")
      dic <- plantR:::admin
    } else {
      dic <- admin.names
    }

  ## Crossing the strings with the administrative levels
    tmp <- dplyr::left_join(x1, dic, by = "loc.correct")

  ## Filtering the result and returning
    result <- tmp[ ,c("loc.correct","NAME_0","NAME_1","NAME_2","NAME_3","source")]
    return(result)
}
