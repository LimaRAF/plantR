#' @title Validate Record Locality Information
#'
#' @description Compares the resolution of the locality information provided in
#'   the original record with the resolution retrieved from the gazetteer.
#'
#' @param x a data frame containing the resolution of the original and the
#'   gazetteer locality information.resolution from the gazetteer.
#' @param res.orig character. The name of the column containing the resolution
#'   of the locality information from the original records. Default to
#'   'resol.orig'.
#' @param res.gazet character. The name of the column containing the resolution
#'   of the locality information from which the gazetteer coordinates were
#'   obtained. Default to 'resolution.gazetteer'.
#'
#' @return the data frame \code{x} with a new column 'loc.check' with the result
#'   of the locality validation.
#'
#'
#' @details The function simply compares if the original information provided
#'   with each specimen is found in the locality gazetteer and at which
#'   resolution (i.e. country, state, municipality). This comparison is strongly
#'   dependent on the completeness of the gazetteer used. Please remind that the
#'   gazetteer provided with __plantR__ is strongly biased towards Latin
#'   America, particularly Brazil.
#'
#'   The records whose locality was not found in the gazetteer at the same
#'   resolution are flagged with a "check_...". For these records, the locality
#'   resolution is downgraded until a locality is found in the gazetteer. If
#'   even the country name is not found, then the locality is flagged as
#'   "no_info". Specimens retaining their resolution are flagged with a
#'   "ok_same_resolution" and those found at a better resolution are flagged
#'   with an "ok_" (e.g. 'ok_state2municipality': a record which had its
#'   original resolution at the ' 'stateProvince' level is now at the
#'   'municipality' level).
#'
#'   The records flagged with the class "check_local.2municip." should not be
#'   seen as problematic, since the __plantR__ gazetteer contain a list of
#'   county names that is much more comprehensive than the list of locality
#'   names.
#'
#' @author Renato A. F. de Lima
#'
#' @examples
#' (df <- data.frame(
#' resol.orig = c("locality", "municipality","stateProvince","country"),
#' resolution.gazetteer = c("county", "county","country", "county"),
#' stringsAsFactors = FALSE))
#'
#' res <- validateLoc(df)
#' res
#'
#' @export validateLoc
#'
validateLoc <- function(x,
                        res.orig = "resol.orig",
                        res.gazet = "resolution.gazetteer") {

  ## check input:
  if (!class(x)[1] == "data.frame")
    stop("Input object needs to be a data frame!")

  if (dim(x)[1] == 0)
    stop("Input data frame is empty!")

  if (!all(c(res.orig, res.gazet) %in% colnames(x))) {
    stop(paste0("input object needs to have the following fields: ",
                res.orig," and ",  res.gazet))
  }

  ## putting the input data in the right order
  x1 <- x[, which(colnames(x) %in% c(res.orig, res.gazet))]

  ## Flagging localities with changes in their resolution
  resol.gazet <- gsub("state", "stateProvince", x1[, res.gazet], perl = TRUE)
  resol.gazet <- gsub("county", "municipality", resol.gazet, perl = TRUE)
  resol.gazet[is.na(resol.gazet)] <- "no_info"
  x1$loc.check <- resol.gazet == x1[, res.orig]
  # OK: change to a better resolution
  x1$loc.check[x1$loc.check == FALSE &
                 resol.gazet %in% "locality" &
                 x1[, res.orig] %in% "country"] <- "ok_country2locality"
  x1$loc.check[x1$loc.check == FALSE &
                 resol.gazet %in% "municipality" &
                 x1[, res.orig] %in% "country"] <- "ok_country2municip."
  x1$loc.check[x1$loc.check == FALSE &
                 resol.gazet %in% "stateProvince" &
                 x1[, res.orig] %in% "country"] <- "ok_country2state"
  x1$loc.check[x1$loc.check == FALSE &
                 resol.gazet %in% "locality" &
                 x1[, res.orig] %in% "stateProvince"] <- "ok_state2locality"
  x1$loc.check[x1$loc.check == FALSE &
                 resol.gazet %in% "municipality" &
                 x1[, res.orig] %in% "stateProvince"] <- "ok_state2municip."
  x1$loc.check[x1$loc.check == FALSE &
                 resol.gazet %in% "locality" &
                 x1[, res.orig] %in% "municipality"] <- "ok_municip.2locality"

  # Mismatches (check): locality info is given but was not found in the gazetteer
  x1$loc.check[x1$loc.check == FALSE &
                 resol.gazet %in% "no_info" &
                 x1[, res.orig] %in% "country"] <- "check_country2no.info"
  x1$loc.check[x1$loc.check == FALSE &
                 resol.gazet %in% "no_info" &
                 x1[, res.orig] %in% "stateProvince"] <- "check_state2no.info"
  x1$loc.check[x1$loc.check == FALSE &
                 resol.gazet %in% "no_info" &
                 x1[, res.orig] %in% "municipality"] <- "check_municip.2no.info"
  x1$loc.check[x1$loc.check == FALSE &
                 resol.gazet %in% "no_info" &
                 x1[, res.orig] %in% "locality"] <- "check_local.2no.info"
  x1$loc.check[x1$loc.check == FALSE &
                 resol.gazet %in% "country" &
                 x1[, res.orig] %in% "locality"] <- "check_local.2country"
  x1$loc.check[x1$loc.check == FALSE &
                 resol.gazet %in% "stateProvince" &
                 x1[, res.orig] %in% "locality"] <- "check_local.2state"
  x1$loc.check[x1$loc.check == FALSE &
                 resol.gazet %in% "municipality" &
                 x1[, res.orig] %in% "locality"] <- "check_local.2municip."
  x1$loc.check[x1$loc.check == FALSE &
                 resol.gazet %in% "country" &
                 x1[, res.orig] %in% "municipality"] <- "check_municip.2country"
  x1$loc.check[x1$loc.check == FALSE &
                 resol.gazet %in% "stateProvince" &
                 x1[, res.orig] %in% "municipality"] <- "check_municip.2state"
  x1$loc.check[x1$loc.check == FALSE &
                 resol.gazet %in% "country" &
                 x1[, res.orig] %in% "stateProvince"] <- "check_state2country"

  # No changes and information not found even at country level (no_info)
  x1$loc.check[x1$loc.check == TRUE] <- "ok_same_resolution"
  x1$loc.check[is.na(x1$loc.check)] <- "no_info"

  ## Reporting the validation results
  print("Locality resolution in the original data vs. edited data:")
  print(table(edited = resol.gazet, original = x1[, res.orig]))

  ## Binding the results with the original data.frame and returning
  result <- cbind.data.frame(x, loc.check = x1$loc.check)
  invisible(result)
}
