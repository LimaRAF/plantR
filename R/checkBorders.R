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
#' @export
#'
checkBorders <- function(x,
                         geo.check = "geo.check",
                         country.shape = "NAME_0",
                         country.gazetteer = "country.gazet") {
  check_these <- grepl(pattern = "country_bad", x[,geo.check], perl = TRUE)
  check_country <- x[check_these,]
  shares_bord <- Vectorize(shares_border)
  check_country$share_border <-
    shares_bord(check_country[,country.shape],
                check_country[,country.gazetteer])
  check_country$border.check <-
    dplyr::if_else(check_country$share_border == TRUE,
            "check_borders", "check_inverted")

  #rafl: alguma chance de `shares_bord()` mudar a ordem do data frame, se não sugiro:
  x$border.check <- x$share_border <- NA
  x$border.check[check_these] <-
    check_country$border.check
  x$share_border[check_these] <-
    check_country$share_border
  # x1 <- dplyr::left_join(x, check_country)
  return(x)
}
