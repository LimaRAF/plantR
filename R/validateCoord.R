#' Workflow for geographical coordinates validation
#'
#' This function performs the workflow for geographical coordinates validation
#'
#' @inheritParams checkCoord
#' @inheritParams checkBorders
#' @inheritParams checkInverted
#' @inheritParams fixInverted
#' @inheritParams checkSea
#'
#' @export
#' @author Andrea Sánchez-Tapia & Sara R. Mortara
#'
validateCoord <- function(x,
                          lon = "decimalLongitude.new",
                          lat = "decimalLatitude.new",
                          country.shape = "NAME_0",
                          country.gazetteer = "country.gazet") {
  occs1 <- checkCoord(x,
                      lon = lon,
                      lat = lat,
                      dist.center = FALSE,
                      keep.cols = c("geo.check", "NAME_0", "country.gazet")) # "distCentroid_m": not using for now
  occs2 <- checkBorders(occs1,
                        country.shape = country.shape,
                        country.gazetteer = country.gazetteer)
  occs4 <- checkInverted(x = occs2,
                         country.gazetteer = country.gazetteer)
  # occs4 <- fixInverted(occs3)
  #MAS! importante: o default é ".new.new" que são as coordenadas corrigidas em fixInverted
  occs5 <- checkSea(occs4) #nem parametrizo para não correr o risco de mudar o default

  #rafl: parei aqui
  #cria a coluna final
  occs <- occs5 %>%
    dplyr::mutate(final_check = dplyr::case_when(
      geo.check == "coord_original/ok_country/ok_state/ok_county" ~ "ok_county",
      geo.check == "coord_original/ok_country/ok_state" ~ "ok_state",
      geo.check == "coord_original/ok_country" ~ "ok_country",
      geo.check == "coord_original/ok_country/estado_bad" ~ "check_gazetteer",
      geo.check == "coord_original/ok_country/estado_bad/county_bad" ~ "check_gazetteer",
      geo.check == "coord_original/ok_country/estado_bad/ok_county" ~ "check_gazetteer",
      geo.check == "coord_original/ok_country/ok_state/county_bad" ~ "check_gazetteer",
      inv.check == "inverted_lat" ~ "inverted",
      inv.check == "inverted_lon" ~ "inverted",
      inv.check == "inverted_both" ~ "inverted",
      inv.check == "transposed" ~ "inverted",
      inv.check == "transposed_inv_lat" ~ "inverted",
      inv.check == "transposed_inv_both" ~ "inverted",
      border.check == "check_borders" ~ "check_borders",
      sea.shore.check == "sea" ~ "sea",
      sea.shore.check == "shore" ~ "shore",
      sea.shore.check == "land" ~ "land",
      border.check == "check_inverted" ~ "check_inverted",
      geo.check == "no_cannot_check" ~ "no",
      geo.check == "coord_gazet" ~ "no",
      geo.check == "coord_original" ~ "falta"
    ))
  return(occs)
}

