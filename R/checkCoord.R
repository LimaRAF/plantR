#' @title Check Geographical Coordinates
#'
#' @description This function makes the first check of the coordinates
#' against the world and Latin-American maps
#'
#' @param x Data.frame with coordinates in decimal degrees.
#' @param lon Column with the longitude to be validated
#' @param lat Column with the latitude to be validated
#' @param str.name Column with the locality search string
#' @param orig.coord Column with the origin of the coordinates (tipically the
#'   output of function `getCoord()`)
#' @param drop.cols character. Name of columns that should be dropped from the
#'   results.
#'
#' @importFrom dplyr select one_of rename mutate if_else filter ends_with
#' @importFrom tidyr separate unite
#' @importFrom sf st_crs st_as_sf st_join st_intersects st_set_crs
#'
#' @author Andrea Sánchez-Tapia, Sara Mortara & Renato A. F. de Lima
#'
#' @export
checkCoord <- function(x,
                          lon = "decimalLongitude.new",
                          lat = "decimalLatitude.new",
                          str.name = "loc.correct",
                          orig.coord = "origin.coord",
                          drop.cols = c("geometry","country.check","state.check","county.check")) {

  ## check input
  if (!class(x) == "data.frame")
    stop("Input object needs to be a data frame!")

  if (!all(c(lat, lon, str.name, orig.coord) %in% colnames(x)))
    stop("One or more column names declared do not match those of the input object: please rename or specify the correct names")

  ##Preliminary edits
  x$tmp.order <- 1:nrow(x)
  x[, str.name][x[, str.name] %in% "no_loc"] <- NA #porque nao é pais (rafl: concordo, mas não achei nehuma funcao onde esse 'no_loc' é gerado; melhor alterar direto na função que obtém o string, getLoc()?)

  ##Defining the country, state and county columns
  x1 <- tidyr::separate(
    data = x,
    col = str.name,
    sep = "_",
    #into = c("country", "state", "county", "locality", "sublocality"), #rafl: aqui vc sobrescreve as colunas já existentes/originais... é essa a idéia?
    into = c("country.gazet", "state.gazet", "county.gazet", "locality.gazet", "sublocality.gazet"), #rafl: melhor assim talvez?
    remove = FALSE,
    fill = "right"
    )

  ##Creating the geo.check column
  x1$geo.check <- x1[, orig.coord]
  tmp <- x1[x1$origin.coord == "coord_original", ]
  tmp <- tmp[!is.na(tmp[, lon]), ]
  tmp <- tmp[!is.na(tmp[, lat]), ]

  #rafl: Add an step where user can choose between plantR and user-provided shapefiles?

  ##Getting data frame with spatial coordinates and standardizing the projection
  tmp <- sf::st_as_sf(tmp, coords = c(lon, lat))
  # set spatial coordinates
  prj <- sf::st_crs(4326)
  tmp <- sf::st_set_crs(tmp, prj)
  tmp <- sf::st_join(tmp, worldMap, join = sf::st_intersects)
  tmp <- dplyr::rename(tmp, pais_wo = NAME_0) #rafl: tive que atualizar meu dplyr por causa de um erro aqui... precisamos mesmo depender do dplyr para um passo tão simples? Inclui o 'dplyr::'
  #names(tmp)[which(names(tmp) == "NAME_0")] <- "pais_wo" # opcao sem depender do dplyr

  ##Comparing the spatial data frame with the selected country shapefiles
  latam_all <- dplyr::bind_rows(latamMap) # ö checar poligonos faltantes tipo Manaus ##rafl: dava erro bind_rows pois older versions of dplyr não pode ser usado para sf objects; funcionou agora (loko nem o proprio sf faz isso para sf com diferentes numeros de colunas)
  x2 <- sf::st_join(tmp, latam_all, join = sf::st_intersects)
  x2 <- dplyr::rename(x2,
               pais_latam = NAME_0
               #estado = NAME_1,
               #municipio = NAME_2
               #localidade = NAME_3
               #vai ter um NAME_4 e talvez mais
               )
  #checa diferencas paises e preenche com latam se faltar no mundo
  x2 <- dplyr::mutate(x2, NAME_0 = if_else(is.na(pais_wo) &
                                      !is.na(pais_latam), pais_latam, pais_wo))
  # cria o vetor para checar
  x2$loc.coord <- paste(x2$NAME_0, x2$NAME_1, x2$NAME_2, sep = "_")
  x2$loc.coord[x2$loc.coord %in% "NA_NA_NA"] <- NA
  x2$loc.coord <- gsub("_NA_NA$", "", x2$loc.coord, perl = TRUE) #rafl: necessário, certo?
  x2$loc.coord <- gsub("_NA$", "", x2$loc.coord, perl = TRUE) #rafl: necessário, certo?

  # recupera todas as linhas
  x3 <- dplyr::left_join(x,
                         x2[,c("tmp.order", "country.gazet",
                               "state.gazet", "county.gazet",
                               "geo.check", "NAME_0", "NAME_1",
                               "NAME_2", "NAME_3", "loc.coord")],
                         by = "tmp.order")

  ### GEO-VALIDATION STEPS ###
  ##1- Validating the coordinates at different levels - exact matches
  #1.1 Cases with original coordinates but without country, state or county
  #information (cannot check)
  x3$geo.check[is.na(x3[, lon]) & is.na(x3$NAME_0) |
                 is.na(x3[, lat]) & is.na(x3$NAME_0)
                 ] <- "no_cannot_check"
  #1.2 Country-level: good country? All countries
  # x3$country.check <- if_else(x3$country == x3$NAME_0, "ok_country", "country_bad")
  x3$country.check <- dplyr::if_else(x3$country.gazet == x3$NAME_0, 1L, 0L, missing = -9L)

  #1.3 State-level: good state? All countries
  # x3$state.check <- if_else(x3$state == x3$NAME_1, "ok_state", "estado_bad")
  x3$state.check <- dplyr::if_else(x3$state.gazet == x3$NAME_1, 1L, 0L, missing = -9L)

  #1.4 County-level. All countries
  # x3$county.check <- if_else(x3$county == x3$NAME_2, "ok_county", "county_bad")
  x3$county.check <- dplyr::if_else(x3$county.gazet == x3$NAME_2, 1L, 0L, missing = -9L)

  #creates geo.check
  # x4 <- tidyr::unite(x3,
  #                    "geo.check",
  #                    c("geo.check",
  #                      "country.check",
  #                      "state.check",
  #                      "county.check"),
  #                    sep = "/", na.rm = TRUE, remove = FALSE)
  check.paste <- apply(x3[ , c("country.check",
                               "state.check",
                               "county.check")], 1, paste, collapse="")
  # rafl: some classes can be simplified, but I kept most of the raw classes so we can decide together
  #move to sys.data...
  repl.check <- c(
                 #common cases
                 "111" = "ok_county", # ok!
                 "110" = "ok_state", # ok!
                 "100" = "ok_country", # ok!
                 "000" = "country_bad", # ok!
                 "11-9" = "ok_state/no_county", # ok_state?
                 "10-9" = "ok_country/state_bad/no_county", # ok_country?
                 "00-9" = "country_bad/state_bad/no_county", # country_bad?
                 "1-9-9" = "ok_country/no_state/no_county", # ok_country?
                 "-9-9-9" = "coord_bad", # ok?? Or country_bad? Or no_cannot_check?
                 #rare cases
                 "101" = "ok_county/state_bad", # ok_county?
                 "1-91" = "ok_county/no_state", # ok_county?
                 "1-90" = "ok_country/no_state/county_bad", # ok_country?
                 "011" = "ok_county/country_bad", # ok_county?
                 "010" = "ok_state/country_bad/county_bad", # country_bad or ok_state?
                 "01-9" = "ok_state/country_bad/no_county", # country_bad or ok_state?
                 "001" = "ok_county/country_bad/state_bad", # do not trust this one; country_bad?
                 "0-91" = "ok_county/country_bad/no_state", # do not trust this one; country_bad?
                 "0-90" = "country_bad/no_state/county_bad", # country_bad?
                 "0-9-9" = "country_bad/no_state/no_county", # country_bad?
                 "-911" = "ok_county/no_country", # ok_county?
                 "-910" = "ok_state/no_country/state_bad", # country_bad or ok_state?
                 "-91-9" = "ok_state/no_country/no_state", # country_bad or ok_state?
                 "-901" = "ok_county/no_country/state_bad", # do not trust this one; country_bad or no_cannot_check?
                 "-900" = "state_bad/no_country", # country_bad or state_bad or no_cannot_check?
                 "-90-9" = "state_bad/no_country/no_county", # country_bad or state_bad or no_cannot_check?
                 "-9-91" = "ok_county/no_country/no_state", # do not trust this one; country_bad or no_cannot_check?
                 "-9-90" = "county_bad/no_country/no_state") # country_bad or no_cannot_check?
  check.paste <- stringr::str_replace_all(check.paste, repl.check)
  x3$geo.check[!x3$geo.check %in% "no_cannot_check"] <-
    check.paste[!x3$geo.check %in% "no_cannot_check"]

  #preparing to return
  x3 <- x3[order(x3$tmp.order), ]
  new.cols <- names(x3)[!names(x3) %in% names(x)]

  if (!is.null(drop.cols))
    new.cols <- new.cols[!new.cols %in% drop.cols]

  x <- x[, -dim(x)[2]] # remove temporary order
  x4 <- cbind.data.frame(x, x3[, new.cols])
  return(x4)
}

