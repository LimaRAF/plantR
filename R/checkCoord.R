#' @title Check Geographical Coordinates
#'
#' @description This function makes the check of the coordinates
#' against the world and Latin-American maps. Optionally, it returns the
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
#' @param dist.center Logical. Should the distance (in meters) between the
#'   original coordinates and those retrieved in the gazetteer be returned?
#'   Defaults to FALSE.
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
#' @details
#'
#' By default the function returns only the geographical validation column
#' ('geo.check') and the distance between the original coordinates and those
#' from a gazetteer, if `dist.center` is TRUE. Other columns available for the
#' output that may be relevant are:
#' - the name administrative levels obtained from the maps: 'NAME_0', 'NAME_1',
#'  'NAME_2' and 'NAME_3';
#' - the locality string in the __plantR__ format combining the localty
#' obtained from the maps: 'loc.coord';
#' - the checks between the locality info obtained from `str.name` and the
#' administrative levels from the maps: 'country.check', 'state.check' and
#' 'county.check'.
#'
#'
#' @export checkCoord
#'
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

  #Escaping R CMD check notes from using dplyr syntax
  NAME_0 <- pais_latam <- pais_wo <- NULL
  #Escaping R CMD check notes
  worldMap <- worldMap
  latamMap <- latamMap

  ## check input
  if (!class(x)[1] == "data.frame")
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
    into = c("country.gazet", "state.gazet", "county.gazet",
             "locality.gazet", "sublocality.gazet"),
    remove = FALSE,
    fill = "right"
  )

  ##Creating the geo.check column and assigning the gazetteer and missing classes
  geo.check <- rep(NA_character_, dim(x)[1])
  ids.gazet <- x[, orig.coord] %in% "coord_gazet"
  geo.check[ids.gazet] <-
    paste0("ok_", x[ids.gazet, res.gazet], "_gazet")
  ids.no.coord <- x[, orig.coord] %in% "no_coord"
  geo.check[ids.no.coord] <- "no_cannot_check"
  #rafl: checar com mais dados se pode ter NAs ou outras classes

  ## Subsetting data for geographical checking
  tmp <- x[is.na(geo.check), ]

  ##Getting data frame with spatial coordinates and standardizing the projection
  tmp <- sf::st_as_sf(tmp, coords = c(lon, lat))
  # set spatial coordinates
  prj <- sf::st_crs(4326)
  tmp <- sf::st_set_crs(tmp, prj)
  tmp <- suppressMessages(
    sf::st_join(tmp, worldMap, join = sf::st_intersects))
  names(tmp)[which(names(tmp) == "NAME_0")] <- "pais_wo"

  ##Defining which coordinates fall into the sea (i.e. original coordinates but no country, state or county)
  geo.check[is.na(geo.check)][is.na(tmp$pais_wo)] <- "sea"

  ##Comparing the spatial data frame with the selected country shapefiles
  latam_all <- dplyr::bind_rows(latamMap) # ö checar poligonos faltantes tipo Manaus ##rafl: dava erro bind_rows pois older versions of dplyr não pode ser usado para sf objects; funcionou agora (loko nem o proprio sf faz isso para sf com diferentes numeros de colunas)
  x2 <- suppressMessages(
    sf::st_join(tmp, latam_all, join = sf::st_intersects))
  x2 <- dplyr::rename(x2,
                      pais_latam = NAME_0
                      #estado = NAME_1,
                      #municipio = NAME_2
                      #localidade = NAME_3
                      #vai ter um NAME_4 e talvez mais
  )
  #checa diferencas paises e preenche com latam se faltar no mundo
  x2 <- dplyr::mutate(x2, NAME_0 = dplyr::if_else(is.na(pais_wo) &
                                                    !is.na(pais_latam), pais_latam, pais_wo))
  # cria o vetor para checar
  x2$loc.coord <- paste(x2$NAME_0, x2$NAME_1, x2$NAME_2, sep = "_")
  x2$loc.coord[x2$loc.coord %in% "NA_NA_NA"] <- NA_character_
  x2$loc.coord <- gsub("_NA_NA$", "", x2$loc.coord, perl = TRUE) #rafl: necessário, certo?
  x2$loc.coord <- gsub("_NA$", "", x2$loc.coord, perl = TRUE) #rafl: necessário, certo?
  # ast: na real loc.coord nao é usado mais.então tudo isto poderia sumir.
  # rafl: vdd, mas acho legal a possibilidade de retornar essa info. Pode ajudar na gestão/correção de coleções.

  # recupera todas as linhas
  x3 <- suppressMessages(
    dplyr::left_join(x,
                     x2[,c("tmp.order",
                           "NAME_0", "NAME_1", "NAME_2", "NAME_3",
                           "loc.coord")]))
  #ast: eu nao sei se vc está tirando colunas aqui mas pelo menos tirei o by que ia criar colunas duplicadas.
  #rafl: ok! removi o geo.check e adicionei o suppressWarnings

  ### GEO-VALIDATION STEPS ###
  ##1- Validating the coordinates at different levels - exact matches
  #1.1 Country-level: good country? All countries
  x3$country.check <- dplyr::if_else(x3$country.gazet == x3$NAME_0,
                                     "ok_country", "bad_country", missing = "no_country")

  #1.2 State-level: good state? All countries
  x3$state.check <- dplyr::if_else(x3$state.gazet == x3$NAME_1,
                                   "ok_state", "bad_state", missing = "no_state")

  #1.3 County-level. All countries
  x3$county.check <- dplyr::if_else(x3$county.gazet == x3$NAME_2,
                                    "ok_county", "bad_county", missing = "no_county")

  ## Updating geo.check
  tmp1 <- apply(x3[ , c("country.check",
                        "state.check",
                        "county.check")], 1, paste, collapse="/")
  geo.check[is.na(geo.check)] <- tmp1[is.na(geo.check)]

  ## Simplifying geo.check
  #ast. sem usar os códigos numéricos podemos trazer o que estava no wrapper:
  #rafl1: re-adicionei as 27 categorias e está tudo no 01_sysdata agora
  #rafl2: agora estou editando tudo direto no geo.check (paste.check removido)
  repl.check <- simpGeoCheck
  geo.check <- stringr::str_replace_all(geo.check, repl.check)
  # repl.check <- c(
  #   "ok_country" = "ok_country",
  #   "ok_country/ok_state" = "ok_state",
  #   "ok_country/ok_state/ok_county" = "ok_county",
  #   "ok_country/estado_bad" = "check_gazetteer_state",
  #   "ok_country/estado_bad/county_bad" = "check_gazetteer_state_county",
  #   "ok_country/estado_bad/ok_county" = "check_gazetteer_state",
  #   "ok_country/ok_state/county_bad" = "check_gazetteer_county")
  #(eu estava usando case_when) pergunta: precisamos das 27 categorias? tem outras funções que dão conta de parte destes problemas (tipo country_bad, mas para esse subset já tem a coluna country_check)
  # rafl: por enquanto vamos deixar as 27 e em seguida a simplificacao. Quando fechamors tudo voltamos e revemos se precisa ou nao

  ## Calculating the distance between the original and the gazetter coordinates
  if (dist.center) {
    x3$distCentroid_m <- NA
    if (dim(tmp)[1] > 0) {
      ids.dist <- !grepl("ok_county|ok_locality|no_cannot_check", x3$geo.check) &
        !(is.na(x3[, lon.gazet]) | is.na(x3[, lat.gazet]))
      tmp2 <- x3[ids.dist, c(lat, lon, lat.gazet, lon.gazet, "distCentroid_m"), ]
      tmp2$distCentroid_m <- # 0.4s for ~2 million (1s using fields::rdist.earth.vec)
        spatialrisk::haversine(tmp2[, 1], tmp2[, 2], tmp2[, 3], tmp2[, 4])
      x3$distCentroid_m[ids.dist] <-
        tmp2$distCentroid_m
    }
  }

  ## Preparing to return
  #re-ordering (for safety), adding geo.check and removing unecessary columns from the tmp object
  x3 <- x3[order(x3$tmp.order), ]
  x3$geo.check <- geo.check
  x3 <- x3[, -which(names(x3) %in% c("tmp.order"))]

  #defining the new columns from the tmp object to be returned
  new.cols <- names(x3)[!names(x3) %in% cols.x]
  if (!is.null(keep.cols))
    new.cols <- new.cols[new.cols %in% keep.cols]

  #removing unecessary column from the original data frame and returning
  tmp.cols <- c("tmp.order", "country.gazet", "state.gazet", "county.gazet",
                "locality.gazet", "sublocality.gazet")
  x <- x[, -which(names(x) %in% tmp.cols)]
  if (length(new.cols) == 0) {
    return(x)
  } else {
    x4 <- x3[, which(names(x3) %in% new.cols), drop = FALSE]
    return(cbind.data.frame(x, x4))
  }
}

