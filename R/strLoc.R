#' @title Construct Locality String
#'
#' @description Combine locality fields to create the standard plantR locality string
#'
#' @param x a data frame.
#'
#' @return A data frame with the locality fields provided in \code{x} and the
#'   locality strings constructed ('loc.string', 'loc.string1' and
#'   'loc.string2').
#'
#' @details The function combines the information provided in the locality
#'   fields to create a standard string that __plantR__ uses to retrieve
#'   information from gazetteers. This string is built by concatenating the
#'   country, state, municipality and locality fields at the best resolution
#'   available. This hierarchical format decreases the chances of retrieving
#'   information from localities with the same names in different regions. The
#'   standard gazetteer provided with __plantR__ uses this standard locality
#'   string to make queries (see __plantR__ function `getLoc` for details).
#'
#' The input data frame should preferably be the output of the __plantR__ function
#'  `fixLoc`, as part of the validation workflow used by __plantR__. This function
#'  returns the edited standard locality fields (i.e. country.new,
#'  stateProvince.new, municipality.new, and locality.new) and, if chosen, the
#'  extra locality field 'locality.scrap'. In this case, `strLoc` also returns an
#'  alternative string ('loc.string2').
#'
#' If used separately, users must provide a data frame with an specific set of
#'  column names (i.e. country.new, stateProvince.new, municipality.new,
#'  locality.new, and, if chosen, locality.scrap). See the
#'  examples below.
#'
#' @author Renato A. F. de Lima
#'
#' @export strLoc
#'
#' @examples
#'
#' ## Using the function separately (need to provide a data in a specific format)
#' # Creating a data frame with locality information
#' (occs.new <- data.frame(country.new = c("brazil", "brazil", "brazil"),
#'                      stateProvince.new = c("rio de janeiro", "rio de janeiro", "rio de janeiro"),
#'                      municipality.new = c("parati", "paraty", "paraty"),
#'                      locality.new = c(NA,"paraty-mirim", NA),
#'                      locality.scrap = c(NA, NA, "trindade")
#'                      ))
#' # Creating locality strings used to query the gazetteer
#' strLoc(occs.new)
#'
#'
#' ## Using the function under the __plantR__ cleaning workflow
#' # Creating a data frame with locality information
#' (occs <- data.frame(country = c("BR", "Brazil", "Brasil"),
#'                      stateProvince = c("RJ", "Rio de Janeiro", "Rio de Janeiro"),
#'                      municipality = c("Parati", "Paraty", "Paraty"),
#'                      locality = c(NA,"Paraty-Mirim", "Trindade")
#'                      ))
#'
#' # Formating the locality information
#' occs.fix <- fixLoc(occs)
#'
#' # Creating locality strings used to query the gazetteer
#' strLoc(occs.fix)
#'
#'
strLoc <- function(x) {

  ## check input:
  if (!class(x) == "data.frame")
    stop("input object needs to be a data frame!")

  if (!any(c("country.new", "stateProvince.new", "municipality.new") %in% colnames(x)))
    stop("input object needs to have at least the following fields: country.new, stateProvince.new, municipality.new")

  ## putting the input data in the right order
  x1 <- x[which(colnames(x) %in%
                  c("country.new","stateProvince.new","municipality.new","locality.new","locality.scrap","resol.orig"))]

  ## Defining a unique code for each county, state/province or county/commune ##
  loc <- rep(NA, dim(x1)[1])
  # county-level
  loc[!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3])] <-
    paste(
      x1[1][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3])],
      x1[2][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3])],
      x1[3][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3])],
      sep="_")
  # state-level
  loc[is.na(loc)&!is.na(x1[1])&!is.na(x1[2])] <-
    paste(
      x1[1][is.na(loc) & !is.na(x1[1]) & !is.na(x1[2])],
      x1[2][is.na(loc) & !is.na(x1[1]) & !is.na(x1[2])],
      sep="_")
  # country-level
  loc[is.na(loc) & !is.na(x1[1])] <-
    x1[1][is.na(loc) & !is.na(x1[1])]

  ## Definig a unique code for each locality (if provided)
  if("locality.new" %in% names(x1)) {
    loc1 <- rep(NA, dim(x1)[1])
    #locality-level
    loc1[!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[3]) & !is.na(x1[4])] <-
      paste(x1[1][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[4])],
            x1[2][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[4])],
            x1[3][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[4])],
            x1[4][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[4])],
            sep="_")

    # county-level, but missing stateProvince
    loc1[is.na(loc1) & !is.na(x1[1]) & is.na(x1[2]) & !is.na(x1[3])] <-
      paste(
        x1[1][is.na(loc1) & !is.na(x1[1]) & is.na(x1[2]) & !is.na(x1[3])],
        x1[2][is.na(loc1) & !is.na(x1[1]) & is.na(x1[2]) & !is.na(x1[3])],
        x1[3][is.na(loc1) & !is.na(x1[1]) & is.na(x1[2]) & !is.na(x1[3])],
        sep="_")
  }

  ## Definig a unique code for an alternative way of getting missing info from the locality field (if provided)
  if("locality.scrap" %in% names(x1)) {
    loc2 <- rep(NA, dim(x1)[1])
    #county-level when county is not given
    loc2[!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[5])] <-
      paste(
        x1[1][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[5])],
        x1[2][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[5])],
        x1[5][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[5])],
        sep="_")
    #locality-level, but missing stateProvince and county
    loc2[is.na(loc2) & !is.na(x1[4]) & x1$resol.orig %in% "country"] <-
      paste(
        x1[1][is.na(loc2) & !is.na(x1[4]) & x1$resol.orig %in% "country"],
        NA,
        NA,
        x1[4][is.na(loc2) & !is.na(x1[4]) & x1$resol.orig %in% "country"],
        sep="_")
  }

  ## Merging and returning the result
  result <- x1
  result$loc.string <- loc
  if ("locality.new" %in% names(x1))
    result$loc.string1 <- loc1
  if ("locality.scrap" %in% names(x1))
    result$loc.string2 <- loc2
  return(result)
}
