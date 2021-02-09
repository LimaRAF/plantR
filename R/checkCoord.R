#' @title Check Geographical Coordinates
#'
#' @description This function makes the first check of the coordinates
#' against the world and Latin-American maps. It optionally returns the
#' distance between the original coordinates and those from a gazetteer
#' for coordinates not validated at the county level.
#'
#' @param x Data.frame with coordinates in decimal degrees.
#' @param lon Column with the longitude to be validated
#' @param lat Column with the latitude to be validated
#' @param str.name Column with the verified locality search string
#' @param orig.coord Column with the origin of the coordinates (tipically the
#'   output of function `getCoord()`)
#' @param res.gazet Column with the locality resolution level retrieved from the
#'   gazetteer
#' @param dist.center Logical. Should the distance between the original
#'   coordinates and those retrieved in the gazetteer be returned. Defaults to
#'   FALSE.
#' @param lon.gazet Column with the longitude obtained from a gazetteer
#' @param lat.gazet Column with the latitude obtained from a gazetteer
#' @param keep.cols character. Name of columns that should be kept in the
#'   output.
#'
#' @importFrom dplyr select one_of rename mutate if_else filter ends_with
#' @importFrom tidyr separate
#' @importFrom sf st_crs st_as_sf st_join st_intersects st_set_crs
#' @importFrom spatialrisk haversine
#'
#' @author Andrea Sánchez-Tapia, Sara Mortara & Renato A. F. de Lima
#'
#' @export
checkCoord <- function(x,
                       lon = "decimalLongitude.new",
                       lat = "decimalLatitude.new",
                       str.name = "loc.correct",
                       orig.coord = "origin.coord",
                       res.gazet = "resolution.gazetteer",
                       dist.center = FALSE,
                       lon.gazet = "longitude.gazetteer",
                       lat.gazet = "latitude.gazetteer",
                       keep.cols = c("geo.check", "distCentroid_m")) {

  ## check input
  if (!class(x) == "data.frame")
    stop("Input object needs to be a data frame!")

  if (!all(c(lat, lon, str.name, orig.coord) %in% colnames(x)))
    stop("One or more column names declared do not match those of the input object: please rename or specify the correct names")

  if (dist.center)
    if (any(!c(lon.gazet, lat.gazet) %in% names(x)))
      stop("If 'dist.center' is TRUE, the input must contain the longitude/latitude obtained from a gazetteer: please rename or specify the correct names")

  ##Preliminary edits
  cols.x <- names(x) # original data column names
  x$tmp.order <- 1:nrow(x)
  x[, str.name][x[, str.name] %in% "no_loc"] <- NA #porque nao é pais (rafl: concordo, mas não achei nehuma funcao onde esse 'no_loc' é gerado; melhor alterar direto na função que obtém o string, getLoc()?)

  ##Defining the country, state and county columns
  x <- tidyr::separate(
    data = x,
    col = str.name,
    sep = "_",
    #into = c("country", "state", "county", "locality", "sublocality"), #rafl: aqui vc sobrescreve as colunas já existentes/originais... é essa a idéia?
    into = c("country.gazet", "state.gazet", "county.gazet", "locality.gazet", "sublocality.gazet"), #rafl: melhor assim talvez?
    remove = FALSE,
    fill = "right"
    )

  ##Creating the geo.check column and assigning the gazetteer classes and missing classes
  x$geo.check <- NA
  ids.gazet <- x[, orig.coord] %in% "coord_gazet"
  x$geo.check[ids.gazet] <-
    paste0("ok_", x[ids.gazet, res.gazet], "_gazet")
  ids.no.coord <- x[, orig.coord] %in% "no_coord"
  x$geo.check[ids.no.coord] <- "no_cannot_check"
  #rafl: chcar com mais dados se pode ter NAs ou outras classes

  ## subsetting data for checking
  tmp <- x[is.na(x$geo.check), ]
  # tmp <- tmp[!is.na(tmp[, lon]), ] #rafl: não mais necessário
  # tmp <- tmp[!is.na(tmp[, lat]), ] #rafl: não mais necessário

  #rafl: Add an step where user can choose between plantR and user-provided shapefiles?

  ##Getting data frame with spatial coordinates and standardizing the projection
  tmp <- sf::st_as_sf(tmp, coords = c(lon, lat))
  # set spatial coordinates
  prj <- sf::st_crs(4326)
  tmp <- sf::st_set_crs(tmp, prj)
  tmp <- sf::st_join(tmp, worldMap, join = sf::st_intersects)
  names(tmp)[which(names(tmp) == "NAME_0")] <- "pais_wo"

  ##Defining which coordinates fall into the sea
  tmp$geo.check <- tmp$pais_wo

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
# ast: na real loc.coord nao é usado mais.então tudo isto poderia sumir.

  # recupera todas as linhas
  x3 <- dplyr::left_join(x,
                         x2[,c("tmp.order",
                               "geo.check",
                               "NAME_0", "NAME_1", "NAME_2", "NAME_3",
                               "loc.coord")])
  #ast: eu nao sei se vc está tirando colunas aqui mas pelo menos tirei o by que ia criar colunas duplicadas.

  ### GEO-VALIDATION STEPS ###
  ##1- Validating the coordinates at different levels - exact matches
  #1.1 Cases with original coordinates but without country, state or county
  #information (cannot check)
  # x3$geo.check[is.na(x3[, lon]) & is.na(x3$NAME_0) | # rafl: no longer necessary?
  #                is.na(x3[, lat]) & is.na(x3$NAME_0)
  #                ] <- "no_cannot_check"
  #1.2 Country-level: good country? All countries
  x3$country.check <- dplyr::if_else(x3$country.gazet == x3$NAME_0, "ok_country", "country_bad")

  #1.3 State-level: good state? All countries
  x3$state.check <- dplyr::if_else(x3$state.gazet == x3$NAME_1, "ok_state", "estado_bad")

  #1.4 County-level. All countries
  x3$county.check <- dplyr::if_else(x3$county.gazet == x3$NAME_2, "ok_county", "county_bad")
  #creates geo.check - eu usei unite porque lidava bem com os NA. desde que o paste lide bem eu to de boas
  # x4 <- tidyr::unite(x3,
  #                    "geo.check",
  #                    c("geo.check",
  #                      "country.check",
  #                      "state.check",
  #                      "county.check"),
  #                    sep = "/", na.rm = TRUE, remove = FALSE)
  check.paste <- apply(x3[ , c("country.check",
                               "state.check",
                               "county.check")], 1, paste, collapse = "")
  #ast. sem usar os códigos numéricos podemos trazer o que estava no wrapper:
  repl.check <- c(
    "ok_country" = "ok_country",
    "ok_country/ok_state" = "ok_state",
    "ok_country/ok_state/ok_county" = "ok_county",
    "ok_country/estado_bad" = "check_gazetteer_state",
    "ok_country/estado_bad/county_bad" = "check_gazetteer_state_county",
    "ok_country/estado_bad/ok_county" = "check_gazetteer_state",
    "ok_country/ok_state/county_bad" = "check_gazetteer_county")
  #(eu estava usando case_when) pergunta: precisamos das 27 categorias? tem outras funções que dão conta de parte destes problemas (tipo country_bad, mas para esse subset já tem a coluna country_check)
  # e depois um
  check.paste <- stringr::str_replace_all(check.paste, repl.check)
  # mas não entendo por que não fazer isto em geo.check e sim modificar check.paste.já temos as colunas de checagem individual. agora vamos ter um geo.check e um check.paste. geo.check agora está com os países desde a linha 96 mas não tem nada a respeito de países, states, counties.
  x3$geo.check[!x3$geo.check %in% "no_cannot_check"] <-
    check.paste[!x3$geo.check %in% "no_cannot_check"]

  ## Calculating the distance between the original and the gazetter coordinates
  ## Optional part added by renato
  if (dist.center) {
    x3$distCentroid_m <- NA
    if (dim(tmp)[1] > 0) {
      ids.dist <- !grepl("ok_county|no_cannot_check", x3$geo.check) &
        !(is.na(x3[, lon.gazet]) | is.na(x3[, lat.gazet]))
      tmp <- x3[ids.dist, c(lat, lon, lat.gazet, lon.gazet, "distCentroid_m"), ]
      tmp$distCentroid_m <- # 0.4 secs for ~2 million records (1 sec using fields::rdist.earth.vec)
        spatialrisk::haversine(tmp[, 1], tmp[, 2], tmp[, 3], tmp[, 4])
      x3$distCentroid_m[ids.dist] <-
        tmp$distCentroid_m
    }
  }

  ## Preparing to return
  x3 <- x3[order(x3$tmp.order), ]
  new.cols <- names(x3)[!names(x3) %in% cols.x]

  if (!is.null(keep.cols))
    new.cols <- new.cols[new.cols %in% keep.cols]

  x <- x[, -which(names(x) == "tmp.order")] # remove temporary order
  x4 <- cbind.data.frame(x, x3[, new.cols])
  return(x4)
}

