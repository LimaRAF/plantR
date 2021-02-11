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

  ## First coordinate check
  x1 <- checkCoord(x,
                      lon = lon,
                      lat = lat,
                      dist.center = FALSE,
                      keep.cols = c("geo.check", country.shape, country.gazetteer))

  ## Checking bad coordinates close to countries frontiers
  x2 <- checkBorders(x1,
                        country.shape = country.shape,
                        country.gazetteer = country.gazetteer)

  ## Checking bad coordinates in the sea but close to the shore
  x3 <- checkShore(x2,
                 geo.check = "geo.check",
                 lon = lon,
                 lat = lat)

  ## Checking inverted and swapped coordinates
  x4 <- checkInverted(x3, overwrite = TRUE,
                         country.gazetteer = country.gazetteer)


  #### REAPLICAR coordCheck
  x4.tmp <- x4[grepl("\\[", x4$geo.check, perl = TRUE), ]

  #### CHECAR COLUNAS SOBRANDO
  # "tmp.ordem", "country.gazet", "NAME_0"
  # "share_corder" ou "border.check"?

  # occs4 <- fixInverted(occs3)
  #MAS! importante: o default é ".new.new" que são as coordenadas corrigidas em fixInverted

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

