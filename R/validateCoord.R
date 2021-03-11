#' @title Spatial Validation of Species Records
#'
#' @description This function performs the crossing of the geographical
#' coordinates with the world and Latin-american maps, and it checks for
#' coordinates falling near the sea shore, open sea and country boundaries. It
#' also test if problematic coordinates are not inverted or swapped. Finally,
#' the function searches for records taken from cultivated individuals and
#' for the presence of spatial outliers for each species.
#'
#' @param x Data.frame with records and their coordinates in decimal degrees.
#' @param lon Column with the longitude to be validated. Default to 'decimalLongitude.new'
#' @param lat Column with the latitude to be validated. Default to 'decimalLatitude.new'
#' @param country.shape Name of the column with the country name obtained from
#'   the world map based on the original record coordinates. Default to
#'   'NAME_0'
#' @param country.gazetteer Name of the column with the country name obtained
#'   from the gazetteer, based on the description of the record locality.
#'   Default to 'loc.correct'
#' @param tax.name character. Name of the columns containing the species name.
#'   Default to "scientificName.new"
#' @param output a character string with the type of output desired: 'new.col'
#'   (columns with the new results for each type of validation added to the
#'   input data) or 'same.col' (results are stored only in overwritten into
#'   column `geo.check`). Default to 'same.col'.
#'
#' @return The input data frame, plus the new columns with the results of the
#' geographical coordinates (e.g. 'geo.check').
#'
#' @details The function works similarly to a wrapper function, where the
#'   individuals steps of the proposed __plantR__ workflow for the validation
#'   of the spatial information associated to each record (e.g. geographical
#'   coordinates) are performed altogether (see the __plantR__ tutorial for
#'   details).
#'
#' @seealso
#'  \link[plantR]{checkCoord}, \link[plantR]{checkBorders}, \link[plantR]{checkShore},
#'  \link[plantR]{checkInverted}, \link[plantR]{checkOut}, \link[plantR]{getCult}
#'
#' @author Andrea Sánchez-Tapia, Sara R. Mortara & Renato A. F. de Lima
#'
#' @export validateCoord
#'
validateCoord <- function(x,
                          lon = "decimalLongitude.new",
                          lat = "decimalLatitude.new",
                          country.shape = "NAME_0",
                          country.gazetteer = "country.gazet",
                          tax.name = "scientificName.new",
                          output = "same.col") {

  ## First coordinate check
  x1 <- checkCoord(x,
                   lon = lon,
                   lat = lat,
                   dist.center = FALSE,
                   keep.cols = c("geo.check", country.shape, country.gazetteer))

  ## Checking bad coordinates close to countries frontiers
  x2 <- checkBorders(x1,
                     geo.check = "geo.check",
                     country.shape = country.shape,
                     country.gazetteer = country.gazetteer,
                     output = output)

  ## Checking bad coordinates in the sea but close to the shore
  x3 <- checkShore(x2,
                   geo.check = "geo.check",
                   lon = lon,
                   lat = lat,
                   output = output)

  ## Checking inverted and swapped coordinates
  x4 <- checkInverted(x3,
                      country.gazetteer = country.gazetteer,
                      output = output)

  ## Re-applying checkCoord() to the inverted/swapped coordinates
  if (output == "same.col")
    check_these <- grepl("invert_|trans", x4$geo.check, perl = TRUE)

  if (output == "new.col")
    check_these <- grepl("invert_|trans", x4$geo.check.new, perl = TRUE)

  x4.1 <- x4[check_these, ]
  x4.1 <- x4.1[, -which(names(x4.1) %in% c("geo.check", country.shape, country.gazetteer))]
  x4.2 <- checkCoord(x4.1,
                     lon = lon,
                     lat = lat,
                     dist.center = FALSE,
                     keep.cols = c("geo.check"))

  if (output == "same.col") {
    #Writing new checks for the inverted/swapped coordinates
    x4$geo.check[check_these] <- paste0(x4.2$geo.check,
                                        gsub(".*(?=\\[)", "", x4$geo.check[check_these], perl= TRUE))
    #Removing unecessary columns and returning
    x4 <- x4[, -which(names(x4) %in% c(country.shape, country.gazetteer))]
  }

  if (output == "new.col") {
    ## REVER ESSA PARTE...
    #Writing new checks for the inverted/swapped coordinates
    x4$geo.check.new[check_these] <- paste0(x4.2$geo.check,
                                            gsub(".*(?=\\[)", "", x4$geo.check.new[check_these], perl= TRUE))

    #cria a coluna final; ## rafl: Rever aqui a necessidade; não fazendo por enquanto
    # x5 <- x4 %>%
    #   dplyr::mutate(final_check = dplyr::case_when(
    #     geo.check == "coord_original/ok_country/ok_state/ok_county" ~ "ok_county",
    #     geo.check == "coord_original/ok_country/ok_state" ~ "ok_state",
    #     geo.check == "coord_original/ok_country" ~ "ok_country",
    #     geo.check == "coord_original/ok_country/estado_bad" ~ "check_gazetteer",
    #     geo.check == "coord_original/ok_country/estado_bad/county_bad" ~ "check_gazetteer",
    #     geo.check == "coord_original/ok_country/estado_bad/ok_county" ~ "check_gazetteer",
    #     geo.check == "coord_original/ok_country/ok_state/county_bad" ~ "check_gazetteer",
    #     inv.check == "inverted_lat" ~ "inverted",
    #     inv.check == "inverted_lon" ~ "inverted",
    #     inv.check == "inverted_both" ~ "inverted",
    #     inv.check == "transposed" ~ "inverted",
    #     inv.check == "transposed_inv_lat" ~ "inverted",
    #     inv.check == "transposed_inv_both" ~ "inverted",
    #     border.check == "check_borders" ~ "check_borders",
    #     sea.shore.check == "sea" ~ "sea",
    #     sea.shore.check == "shore" ~ "shore",
    #     sea.shore.check == "land" ~ "land",
    #     border.check == "check_inverted" ~ "check_inverted",
    #     geo.check == "no_cannot_check" ~ "no",
    #     geo.check == "coord_gazet" ~ "no",
    #     geo.check == "coord_original" ~ "falta"
    #   ))
  }

  ## Checking for records from cultivated individuals
  x5 <- getCult(x4)

  ## Checking for spatial outliers
  if (output == "same.col") {
    x6 <- checkOut(x5,
                   lon = lon, lat = lat,
                   tax.name = tax.name,
                   geo.name = "geo.check",
                   cult.name = "cult.check",
                   clas.cut = 3, rob.cut = 16)
  }

  if (output == "new.col") {
    x6 <- checkOut(x5,
                   lon = lon, lat = lat,
                   tax.name = tax.name,
                   geo.name = "geo.check.new",
                   cult.name = "cult.check",
                   clas.cut = 3, rob.cut = 16)
  }

  return(x6)
}

