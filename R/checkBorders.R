#' Checks if two countries share a border
#'
#' @param country1 First country to check
#' @param country2 Second country to check
#'
#' @importFrom textclean replace_non_ascii
#' @importFrom sf st_union st_cast
#'
#' @author Andrea Sánchez-Tapia & Sara Mortara
#'
#' @keywords internal
#'
#' @export
shares_border <- function(country1 = "brazil",
                          country2 = "argentina") {
  w <- spData::world
  w$nome <- tolower(textclean::replace_non_ascii(w$name_long))

  v <- w[w$nome %in% c(country1),]
  z <- w[w$nome %in% c(country2),]
  y <- sf::st_union(v, z)

  v <- sf::st_cast(v, "POLYGON")
  z <- sf::st_cast(z, "POLYGON")
  y <- sf::st_cast(y, "POLYGON")
  polis <- nrow(v) + nrow(z)
  poli_u <- nrow(y)
  if (polis == poli_u) return(FALSE)
  if (poli_u < polis) return(TRUE)
}


#' Flags border points
#'
#' @param x occurrence data frame
#' @param geo.check Name of the column with the validation of the coordinates
#' against
#' @param country.shape Name of the column with the country that comes from the shapefile
#' @param country.gazetteer Name of the column with the country that comes from the gazetteer
#'
#' @importFrom dplyr left_join if_else
#'
#' @author Andrea Sánchez-Tapia & Sara Mortara
#'
#' @export checkBorders
#'
checkBorders <- function(x,
                         geo.check = "geo.check",
                         country.shape = "NAME_0",
                         country.gazetteer = "loc.correct") {

  ## Check input
  if (!class(x)[1] == "data.frame")
    stop("Input object needs to be a data frame!")

  if (!all(c(geo.check, country.shape, country.gazetteer) %in% colnames(x)))
    stop("One or more column names declared do not match those of the input object: please rename or specify the correct names")

  ## Check the map country information
  if (any(grepl("_", x[, country.shape], fixed = TRUE))) {
    country.shp <- sapply(
      strsplit(x[, country.shape], "_", fixed = TRUE), function (x) x[1])
  } else {
    country.shp <- x[, country.shape]
  }

  ## Check the gazetteer country information
  if (any(grepl("_", x[, country.gazetteer], fixed = TRUE))) {
    country.gazet <- sapply(
      strsplit(x[, country.gazetteer], "_", fixed = TRUE), function (x) x[1])
  } else {
    country.gazet <- x[, country.gazetteer]
  }

  ## Checking borders for selected records
  check_these <- grepl("bad_country", x[, geo.check], perl = TRUE)
  shares_bord <- Vectorize(shares_border)
  share_border <- suppressMessages(suppressWarnings(
    shares_bord(country.shp[check_these],
                country.gazet[check_these])))
  border.check <-
    dplyr::if_else(share_border == TRUE,
            "check_borders", "check_inverted")

  ## Preparing to return
  #rafl: alguma chance de `shares_bord()` mudar a ordem do data frame, se não sugiro:
  x$border.check <- x$share_border <- NA
  x$border.check[check_these] <-
    border.check
  x$share_border[check_these] <-
    share_border
  # x1 <- dplyr::left_join(x, x1)
  return(x)
}
