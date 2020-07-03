#' @title Validate Specimen Locality Information
#'
#' @description Compare the resolution of the original locality information provided with the
#' one retrieved from the gazetteer.
#'
#' @param x a data frame.
#'
#' @return the data frame \code{x} with a new column with the result of the checking process.
#'
#' @details The function simply compares if the original information provided with each specimen is found
#' in the locality gazetteer and at which resolution (i.e. country, state, municipality). This comparison
#' is strongly dependent on the completeness of the gazetteer used and please remind that the gazetteer
#' provided with `plantR` is srongly biased towards Latin America, particularly Brazil.
#'
#' @author Lima, R.A.F.
#'
#' @export validateLoc
#'
validateLoc = function(x) {

  ## check input:
  if (!class(x) == "data.frame") { stop("input object needs to be a data frame!") }
  if (!all(c("resol.orig","resolution.gazetteer") %in% colnames(x))) { stop("input object needs to have the following fields: resol.orig, resolution.gazetteer") }

  ## putting the input data in the right order
  x1 <- x[which(colnames(x) %in% c("resol.orig","resolution.gazetteer"))]

  ## Flagging localities with changes in their resolution
  resol.gazet = gsub("state", "stateProvince", x1$resolution.gazetteer)
  resol.gazet = gsub("county", "municipality", resol.gazet)
  resol.gazet[is.na(resol.gazet)] = "no_info"
  x1$loc.check = resol.gazet == x1$resol.orig
    # OK: change to a better resolution
    x1$loc.check[x1$loc.check == FALSE &
                   resol.gazet %in% "locality" &
                   x1$resol.orig %in% "country"] = "ok_country2locality"
    x1$loc.check[x1$loc.check == FALSE &
                   resol.gazet %in% "municipality" &
                   x1$resol.orig %in% "country"] = "ok_country2municip."
    x1$loc.check[x1$loc.check == FALSE &
                   resol.gazet %in% "stateProvince" &
                   x1$resol.orig %in% "country"] = "ok_country2state"
    x1$loc.check[x1$loc.check == FALSE &
                   resol.gazet %in% "locality" &
                   x1$resol.orig %in% "stateProvince"] = "ok_state2locality"
    x1$loc.check[x1$loc.check == FALSE &
                   resol.gazet %in% "municipality" &
                   x1$resol.orig %in% "stateProvince"] = "ok_state2municip."
    x1$loc.check[x1$loc.check == FALSE &
                   resol.gazet %in% "locality" &
                   x1$resol.orig %in% "municipality"] = "ok_municip.2locality"

    # Mismatches (check): locality info is given but was not found in the gazetteer
    x1$loc.check[x1$loc.check == FALSE &
                   resol.gazet %in% "no_info" &
                   x1$resol.orig %in% "country"] = "check_country2no.info"
    x1$loc.check[x1$loc.check == FALSE &
                   resol.gazet %in% "no_info" &
                   x1$resol.orig %in% "stateProvince"] = "check_state2no.info"
    x1$loc.check[x1$loc.check == FALSE &
                   resol.gazet %in% "no_info" &
                   x1$resol.orig %in% "municipality"] = "check_municip.2no.info"
    x1$loc.check[x1$loc.check == FALSE &
                   resol.gazet %in% "no_info" &
                   x1$resol.orig %in% "locality"] = "check_local.2no.info"
    x1$loc.check[x1$loc.check == FALSE &
                   resol.gazet %in% "country" &
                   x1$resol.orig %in% "locality"] = "check_local.2country"
    x1$loc.check[x1$loc.check == FALSE &
                   resol.gazet %in% "stateProvince" &
                   x1$resol.orig %in% "locality"] = "check_local.2state"
    x1$loc.check[x1$loc.check == FALSE &
                   resol.gazet %in% "municipality" &
                   x1$resol.orig %in% "locality"] = "check_local.2municip."
    x1$loc.check[x1$loc.check == FALSE &
                   resol.gazet %in% "country" &
                   x1$resol.orig %in% "municipality"] = "check_municip.2country"
    x1$loc.check[x1$loc.check == FALSE &
                   resol.gazet %in% "stateProvince" &
                   x1$resol.orig %in% "municipality"] = "check_municip.2state"
    x1$loc.check[x1$loc.check == FALSE &
                   resol.gazet %in% "country" &
                   x1$resol.orig %in% "stateProvince"] = "check_state2country"

    # No changes and information not found even at country level (no_info)
    x1$loc.check[x1$loc.check == TRUE] = "ok_same_resolution"
    x1$loc.check[is.na(x1$loc.check)] = "no_info"

  ## Reporting the validation results
    print("Resolution of the localities in the original data vs. the edited data:")
    print(table(edited = resol.gazet, original = x1$resol.orig))

  ## Binding the results with the original data.frame and returning
    result = cbind.data.frame(x, loc.check = x1$loc.check)
    return(result)
}
