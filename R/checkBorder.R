#' Joins occs and shapefiles
#'
#' @param occs   Data.frame with coordinates and GADM or DarwinCore levels columns
#' @param lon    Name of the longitude columun, defaults to decimalLongitude.new
#' @param lat    Name of the latitude columun, defaults to decimalLatitude.new
#' @param NAME_0 Name of the column for the highest administrative level (NAME_0= in GADM or country in DwC)
#' @param NAME_1 Name of the column for the second administrative level (NAME_1= in GADM or stateProvince in DwC)
#' @param NAME_2 Name of the column for the third administrative level (NAME_2= in GADM or county in DwC)
#' @param NAME_3 Name of the column for the fourth administrative level (NAME_3= in GADM or municipality in DwC)
#' @param NAME_4 Name of the column for the fifth administrative level (NAME_4= in GADM or locality in DwC)
#' @param NAME_4 Name of the column for the fifth administrative level (NAME_4= in GADM or locality in DwC)
#' @param shape
#'
#' @return
#' @importFrom dplyr left_join bind_rows
#' @importFrom sf st_as_sf st_crs st_set_crs st_join
#' @examples
joinsOccsShape <- function(occs,
                           lat = "decimalLatitude.new",
                           lon = "decimalLongitude.new",
                           #NAME_0 = "NAME_0",
                           #NAME_1 = "NAME_1",
                           #NAME_2 = "NAME_2",
                           #NAME_3 = "NAME_3",
                           #NAME_4 = "NAME_4",
                           shape = latamMap) {
  occs$id <- 1:nrow(occs)
  coord <- occs[,c("id", lon, lat)]
  coord <- na.omit(coord)
  occs <- left_join(coord, occs)
  occs1 <- sf::st_as_sf(occs, coords = c(lon, lat))
  prj <- st_crs(4326)
  occs1 <- st_set_crs(occs1, prj)
  shape_all <- bind_rows(shape)
  joined <- st_join(occs1, shape_all, join = st_intersects)
  return(joined)
}

#' Checks if two countries share a border
#'
#' @param country1
#' @param country2
#' @importFrom spData world
#' @importFrom textclean replace_non_ascii
#' @importFrom sf st_union st_cast
#'
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

checksBorders <- function(x = data.frame(occs),
                          country_shape = "NAME_0",
                          country_gazetteer = "country") {
  check_these <- grepl(pattern = "*country_bad*", x$country.check)
  check_country <- x[check_these,]
  shares_front <- Vectorize(shares_frontier)
  check_country$share_frontier <- shares_front(check_country[,country_shape], check_country[,country_gazetteer])
  check_country$border.check <- if_else(check_country$share_frontier == TRUE, "check_borders", "check_inverted")
  occs1 <- left_join(x, check_country)
}


#' Checks inverted
#'
#' Solves points in the sea


#' Checks points sea
checksSea <- function(x = data.frame(occs)) {
  check_these <- is.na(x$country.check)
  check_sea <- x[check_these,]
  shares_front <- Vectorize(shares_frontier)
  check_country$share_frontier <- shares_front(check_country[,country_shape], check_country[,country_gazetteer])
  check_country$border.check <- if_else(check_country$share_frontier == TRUE, "check_borders", "check_inverted")
  occs1 <- left_join(x, check_country)
}

