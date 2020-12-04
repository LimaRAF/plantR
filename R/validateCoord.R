#' @title Validate Geographical Coordinates
#'
#' @description This function...
#'
#' @param x Data.frame with coordinates in decimal degrees.
#'
#'
#' @importFrom dplyr select one_of rename mutate if_else filter ends_with
#' @importFrom tidyr separate
#' @importFrom sf st_crs st_as_sf st_join st_intersects st_set_crs
validateCoord <- function(x,
                          lat = "decimalLatitude.new",
                          lon = "decimalLongitude.new"
                          ) {
  # ##Getting the file paths for the herbarium data
  #   ### PRE-VALIDATION STEPS ###
  #   ##Loading the occurrence data
  #   ##Removing unwanted columns
  x1 <- x
  x1$order <- 1:nrow(x) #putting the data back on its original order. para o join
  x1$loc.correct[x1$loc.correct == "no_loc"] <- NA
  ##Defining the country, state and county columns
  x1 <- tidyr::separate(
    data = x1,
    col = loc.correct,
    sep = "_",
    into = c("country", "state", "county", "locality", "sublocality"),
    remove = FALSE,
    )

  ##Creating the geo.check columns
  x1$geo.check <- NA
  x1$geo.check[x1$origin.coord %in% "coord_original"] <-  "coord_original"
  x1$geo.check[x1$origin.coord %in% "coord_gazet"] <-  "coord_gazet"
  x1$geo.check[x1$origin.coord %in% "no_coord"] <- "no_coord"

  tmp <- x1[x1$origin.coord %in% "coord_original", ]
  tmp <- tmp[!is.na(tmp[, lon]), ]
  tmp <- tmp[!is.na(tmp[, lat]), ]


  ##Getting data frame with spatial coordinates (points) and standardizing the projection
  tmp <- sf::st_as_sf(tmp, coords = c(lon, lat))
  # set spatial coordinates
  prj <- st_crs(4326)
  tmp <- st_set_crs(tmp, prj)  # define projection system of our data
  over_res <- st_join(tmp, worldMap, join = st_intersects) #agora estão no mesmo crs! :)
  over_res <- rename(over_res, pais_wo = NAME_0)

  ##Comparing the spatial data frame with the selected country shapefiles
  latam_all <- bind_rows(latamMap) # ö checar poligonos faltantes tipo Manaus
  x2 <- st_join(over_res, latam_all, join = st_intersects)
###
  x2 <- rename(x2,
               pais_latam = NAME_0
               #estado = NAME_1,
               #municipio = NAME_2
               #localidade = NAME_3
               #vai ter um NAME_4 e talvez mais
               ) # we need to know what to do here.
  #checa diferencas paises e preenche com latam se faltar no mundo
  x2 <- mutate(x2, NAME_0 = if_else(is.na(pais_wo) &
                                      !is.na(pais_latam), pais_latam, pais_wo))
  # cria o vetor para checar
  x2$loc.coord <- paste(x2$NAME_0, x2$NAME_1, x2$NAME_2, sep = "_")
  x2$loc.coord[x2$loc.coord %in% "NA_NA_NA"] <- NA
  # recupera todas as linhas
  x3 <- left_join(x1, x2) #ast até aqui a primeira função? nao precisa de tmp se fizer aqui
  ### GEO-VALIDATION STEPS ###
  ##1- Validating the coordinates at different levels - exact matchs
  #1.1 Cases with original coordinates but without country, state or county information
  #(cannot check)
  x3$geo.check[is.na(x3$decimalLatitude.new) & is.na(x3$NAME_0) ] <- "no_cannot_check"
  #   #1.2 Country-level: good country? All countries
  x3$country.check <- if_else(x3$country == x3$NAME_0, "ok_country", "country_bad")


   #1.3 State-level: good state? All countries
   x3$state.check <- if_else(x3$state == x3$NAME_1, "ok_state", "estado_bad")


#   #1.4 County-level. All countries
   x3$county.check <- if_else(x3$county == x3$NAME_2, "ok_county", "county_bad") #if OK or not

   x3 <- tidyr::unite(x3,
                      "geo.check",
                      c("geo.check", "country.check", "state.check" , "county.check"),
                        sep = "/", na.rm = TRUE, remove = FALSE)
   return(x3)
   bad_countries <- filter(x3, country.check == "country_bad")
   bad_countries <<- select(bad_countries,
                            order,
                            NAME_0,
                            country,
                            decimalLatitude,
                            decimalLongitude,
                            decimalLatitude.new,
                            decimalLongitude.new,
                            latitude.gazetteer,
                            longitude.gazetteer,
                            geo.check)
   bad_states <- filter(x3, state.check == "estado_bad")
   bad_states <<- select(bad_states,
                         order,
                         NAME_1,
                         state,
                         decimalLatitude,
                         decimalLongitude,
                         decimalLatitude.new,
                         decimalLongitude.new,
                         latitude.gazetteer,
                         longitude.gazetteer, geo.check)
   bad_counties <- dplyr::filter(x3, county.check == "county_bad")
   bad_counties <<- select(bad_counties,
                           order,
                           NAME_2,
                           county,
                           NAME_1,
                           state,
                           NAME_0,
                           country,
                           decimalLatitude,
                           decimalLongitude,
                           decimalLatitude.new,
                           decimalLongitude.new,
                           latitude.gazetteer,
                           longitude.gazetteer, geo.check)
   }

#occs <- readr::read_csv("data-raw/occs_for_geographic_validation.csv")
#names(occs)
