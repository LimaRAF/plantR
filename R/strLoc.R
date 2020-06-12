#' @title Construct Locality String
#'
#' @description Combine locality fields to create the standard plantR locality string
#'
#' @param x a data frame.
#'
#' @return a data frame with the locality fields and the the locality strings.
#'
#' @details The function combines the information provided in the locality fields to create
#' a standard string that `plantR` uses to retrieve information from gazetteers. This string
#' consists is build by concatenating the country, state, municipality and locality fields at
#' the best resolution available. This hierarchical format decreases the chances of retrieving
#' information from localities with same names in different regions. The standard gazetteer
#' provided with `plantR` uses this standard locality string to make queries (see `plantR`
#' function `getLoc` for details).
#'
#' The input data frame should preferably be the output of the `plantR` function `fixLoc`.
#' This function returns the edited standard locality fields (i.e. country.new, stateProvince.new,
#' municipality.new, and locality.new) and, if chosen, the extra edtied locality field 'locality.scrap'.
#' If this is the case, `strLoc` will also return an alternative string based on missing locality
#' information extracted from the field 'locality'.
#'
#' @author Lima, R.A.F.
#'
#' @examples ...
#'
#'
strLoc <- function(x) {
  ## check input:
    if (!class(x) == "data.frame") { stop("input object needs to be a data frame!") }
    if (!any(c("country.new","stateProvince.new","municipality.new") %in%
             colnames(x))) { stop("input object needs to have at least the following fields: country.new, stateProvince.new, municipality.new") }

  ## putting the input data in the right order
    x1 = x[which(colnames(x) %in%
                 c("country.new","stateProvince.new","municipality.new","locality.new","locality.scrap","resol.orig"))]


  ## Defining a unique code for each county, state/province or county/commune ##
    loc = rep(NA, dim(x1)[1])
    # county-level
    loc[!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3])] =
      paste(
        x1[1][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3])],
        x1[2][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3])],
        x1[3][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3])],
        sep="_")
    # state-level
    loc[is.na(loc)&!is.na(x1[1])&!is.na(x1[2])] =
      paste(
        x1[1][is.na(loc) & !is.na(x1[1]) & !is.na(x1[2])],
        x1[2][is.na(loc) & !is.na(x1[1]) & !is.na(x1[2])],
        sep="_")
    # country-level
    loc[is.na(loc) & !is.na(x1[1])] =
      x1[1][is.na(loc) & !is.na(x1[1])]

  ## Definig a unique code for each locality (if provided)
    if("locality.new" %in% names(x1)) {
      loc1 = rep(NA, dim(x1)[1])
      #locality-level
      loc1[!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[3]) & !is.na(x1[4])] =
        paste(x1[1][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[4])],
              x1[2][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[4])],
              x1[3][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[4])],
              x1[4][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[4])],
              sep="_")

      # county-level, but missing stateProvince
      loc1[is.na(loc1) & !is.na(x1[1]) & is.na(x1[2]) & !is.na(x1[3])] =
        paste(
          x1[1][is.na(loc1) & !is.na(x1[1]) & is.na(x1[2]) & !is.na(x1[3])],
          x1[2][is.na(loc1) & !is.na(x1[1]) & is.na(x1[2]) & !is.na(x1[3])],
          x1[3][is.na(loc1) & !is.na(x1[1]) & is.na(x1[2]) & !is.na(x1[3])],
          sep="_")
    }

  ## Definig a unique code for an alternative way of getting missing info from the locality field (if provided)
    if("locality.scrap" %in% names(x1)) {
      loc2 = rep(NA, dim(x1)[1])
      #county-level when county is not given
      loc2[!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[5])] =
        paste(
          x1[1][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[5])],
          x1[2][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[5])],
          x1[5][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[5])],
          sep="_")
      #locality-level, but missing stateProvince and county
      loc2[is.na(loc2) & !is.na(x1[4]) & x1$resol.orig %in% "country"] =
        paste(
          x1[1][is.na(loc2) & !is.na(x1[4]) & x1$resol.orig %in% "country"],
          NA,
          NA,
          x1[4][is.na(loc2) & !is.na(x1[4]) & x1$resol.orig %in% "country"],
          sep="_")
    }

  ## Merging and returning the result
    result = x1
    result$loc.string = loc
    if("locality.new" %in% names(x1)) result$loc.string1 = loc1
    if("locality.scrap" %in% names(x1)) result$loc.string2 = loc2
    return(result)
}
