#' @title Get Administrative Names
#'
#' @description Obtain the names of the administrative subdivisions for a given locality from the gazetteer.
#'
#' @param x a character or vector containing the locality search strings
#' @param admin.names a data frame containing the list of names of the locality subdivisions;
#' the default is "plantR" (biased towards Latin America)
#'
#' @return ...
#'
#' @details The function ...
#'
#' @author Lima, R.A.F.
#'
#' @example
#' getAdmin(x = c("paraguay_paraguari","brazil_parana_paranagua","brazil_sao paulo_sao miguel arcanjo_pe carlos botelho"))
#'
#' @importFrom dplyr left_join
#' @importFrom stringr str_count
#'
#' @export getAdmin
#'
getAdmin = function(x, admin.names = "plantR") {

  ## check input:
    if (class(x) == "data.frame") {
      if (!any(grepl("^loc.correct$", names(x)))) { stop("input object needs to have a column loc.correct with the locality strings") }
      x1 = x[which(colnames(x) %in% c("loc.correct"))]
    } else {
      x1 = data.frame(loc.correct = x, stringsAsFactors= FALSE)
    }

  ## Getting the administrative levels
    if(all(admin.names %in% c("plantR","plantr"))) {
      load("./R/sysdata.rda")
      dic <- admin
    } else {
      dic <- admin.names
    }

  ## Crossing the strings with the administrative levels
    tmp = left_join(x1, dic, by = "loc.correct")

  ## Filtering the result and returning
    result = tmp[ ,c("loc.correct","NAME_0","NAME_1","NAME_2","NAME_3","source")]
    return(result)
}
