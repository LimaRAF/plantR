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
  y <- st_union(v, z)

  v <- st_cast(v, "POLYGON")
  z <- st_cast(z, "POLYGON")
  y <- st_cast(y, "POLYGON")
  polis <- nrow(v) + nrow(z)
  poli_u <- nrow(y)
  if (polis == poli_u) return(FALSE)
  if (poli_u < polis) return(TRUE)
}


#' Flags border points
#'
#' @param x occurrence data frame
#' @param country_shape Name of the column with the country that comes from the shapefile
#' @param country_gazetteer Name of the column with the country that comes from the gazetteer
#' @importFrom dplyr left_join if_else
#'
#' @author Andrea Sánchez-Tapia & Sara Mortara
#'
#' @export

checkBorders <- function(x = data.frame(occs),
                          country_shape = "NAME_0",
                          country_gazetteer = "country") {
  check_these <- grepl(pattern = "*country_bad*", x$country.check)
  check_country <- x[check_these,]
  shares_bord <- Vectorize(shares_border)
  check_country$share_border <-
    shares_bord(check_country[,country_shape],
                check_country[,country_gazetteer])
  check_country$border.check <-
    if_else(check_country$share_border == TRUE,
            "check_borders", "check_inverted")
  occs1 <- left_join(x, check_country)
}


