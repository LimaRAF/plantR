#' @title Check Geographical Coordinates
#'
#' @description This function makes the first check of the coordinates
#' against the worldMap and LatamMap shapefiles
#'
#' @param x Data.frame with coordinates in decimal degrees.
#' @param lon Column with the longitude to be validated
#' @param lat Column with the latitude to be validated
#'
#' @importFrom dplyr select one_of rename mutate if_else filter ends_with
#' @importFrom tidyr separate
#' @importFrom sf st_crs st_as_sf st_join st_intersects st_set_crs
#'
#' @author Andrea Sánchez-Tapia, Sara Mortara & Renato A. F. de Lima
#'
#' @export
checkCoord <- function(x = occs,
                          lon = "decimalLongitude.new",
                          lat = "decimalLatitude.new") {
  #   ##Loading the occurrence data
  x$order <- 1:nrow(x)
  x$loc.correct[x$loc.correct == "no_loc"] <- NA #porque nao é pais
  ##Defining the country, state and county columns
  x <- tidyr::separate(
    data = x,
    col = loc.correct,
    sep = "_",
    into = c("country", "state", "county", "locality", "sublocality"),
    remove = FALSE,
    fill = "right"
    )

  ##Creating the geo.check column
  x$geo.check <- x$origin.coord
  tmp <- x[x$origin.coord == "coord_original", ]
  tmp <- tmp[!is.na(tmp[, lon]), ]
  tmp <- tmp[!is.na(tmp[, lat]), ]

  ##Getting data frame with spatial coordinates and standardizing the projection
  tmp <- sf::st_as_sf(tmp, coords = c(lon, lat))
  # set spatial coordinates
  prj <- st_crs(4326)
  tmp <- st_set_crs(tmp, prj)
  tmp <- st_join(tmp, worldMap, join = st_intersects)
  tmp <- rename(tmp, pais_wo = NAME_0)

  ##Comparing the spatial data frame with the selected country shapefiles
  latam_all <- bind_rows(latamMap) # ö checar poligonos faltantes tipo Manaus
  x2 <- st_join(tmp, latam_all, join = st_intersects)
  x2 <- rename(x2,
               pais_latam = NAME_0
               #estado = NAME_1,
               #municipio = NAME_2
               #localidade = NAME_3
               #vai ter um NAME_4 e talvez mais
               )
  #checa diferencas paises e preenche com latam se faltar no mundo
  x2 <- mutate(x2, NAME_0 = if_else(is.na(pais_wo) &
                                      !is.na(pais_latam), pais_latam, pais_wo))
  # cria o vetor para checar
  x2$loc.coord <- paste(x2$NAME_0, x2$NAME_1, x2$NAME_2, sep = "_")
  x2$loc.coord[x2$loc.coord %in% "NA_NA_NA"] <- NA
  # recupera todas as linhas
  x3 <- left_join(x, x2)

  ### GEO-VALIDATION STEPS ###
  ##1- Validating the coordinates at different levels - exact matches
  #1.1 Cases with original coordinates but without country, state or county
  #information (cannot check)
  x3$geo.check[is.na(x3[, lon]) & is.na(x3$NAME_0) |
                 is.na(x3[, lat]) & is.na(x3$NAME_0)
                 ] <- "no_cannot_check"
  #1.2 Country-level: good country? All countries
  x3$country.check <- if_else(x3$country == x3$NAME_0, "ok_country", "country_bad")
  #1.3 State-level: good state? All countries
  x3$state.check <- if_else(x3$state == x3$NAME_1, "ok_state", "estado_bad")
  #1.4 County-level. All countries
  x3$county.check <- if_else(x3$county == x3$NAME_2, "ok_county", "county_bad")

  #creates geo.check
  x3 <- tidyr::unite(x3,
                     "geo.check",
                     c("geo.check",
                       "country.check",
                       "state.check",
                       "county.check"),
                     sep = "/", na.rm = TRUE, remove = FALSE)
  return(x3)
}

