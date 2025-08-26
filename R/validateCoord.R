#' @title Spatial Validation of Species Records
#'
#' @description This function performs the crossing of the
#'   geographical coordinates with the world and Latin American maps,
#'   and it checks for coordinates falling near the sea shore, open
#'   sea and country boundaries. It also test if problematic
#'   coordinates are not inverted or swapped. Finally, the function
#'   searches for records taken from cultivated individuals, for the
#'   presence of spatial outliers and also doubtful distribution.
#'
#' @param x a data frame with the species records and their
#'   coordinates in decimal degrees.
#' @param country.shape Name of the column with the country name
#'   obtained from the world map based on the original record
#'   coordinates. Default to 'NAME_0'
#' @param country.gazetteer Name of the column with the country name
#'   obtained from the gazetteer, based on the description of the
#'   record locality. Default to 'loc.correct'
#' @param tax.name character. Name of the columns containing the
#'   species name. Default to "scientificName.new"
#' @param output a character string with the type of output desired:
#'   'new.col' (columns with the new results for each type of
#'   validation added to the input data) or 'same.col' (results are
#'   stored only in overwritten into column `geo.check`). Default to
#'   'same.col'.
#'
#' @return The input data frame, plus the new columns with the results
#'   of the geographical coordinates (e.g. 'geo.check').
#'
#' @details The function works as a wrapper function, where the
#'   individuals steps of the __plantR__ workflow for the validation
#'   of the spatial information associated to each record (e.g.
#'   geographical coordinates) are performed altogether (see the
#'   __plantR__ tutorial for details).
#'
#' @inheritParams checkCoord
#' @inheritParams checkDist
#'
#' @seealso \link[plantR]{checkCoord}, \link[plantR]{checkBorders},
#' \link[plantR]{checkShore}, \link[plantR]{checkInverted},
#' \link[plantR]{checkOut}, \link[plantR]{getCult}, \link[plantR]{checkDist}
#'
#' @author Andrea Sánchez-Tapia, Sara R. Mortara, Guilherme S. Grittz
#'   & Renato A. Ferreira de Lima
#'
#' @encoding UTF-8
#'
#' @export validateCoord
#'
validateCoord <- function(x,
                          lon = "decimalLongitude.new",
                          lat = "decimalLatitude.new",
                          low.map = "plantR",
                          high.map = "plantR",
                          country.shape = "NAME_0",
                          country.gazetteer = "country.gazet",
                          tax.name = "suggestedName",
                          tax.author = "suggestedAuthorship",
                          sep = "_",
                          loc = "loc.correct",
                          source = "bfo",
                          output = "same.col") {
  ## Check input
  if (!inherits(x, "data.frame"))
    stop("Input object needs to be a data frame!")

  if (dim(x)[1] == 0)
    stop("Input data frame is empty!")

  if (!output %in% c("same.col", "new.col"))
    stop("Please choose an output between 'same.col' and 'new.col'")

  # Checking the presence of reserved columns in the input dataset
  x <- checkColNames(x, group = "validate.coords")

  ## First coordinate check
  x1 <- checkCoord(x,
                   lon = lon,
                   lat = lat,
                   low.map = low.map,
                   high.map = high.map,
                   dist.center = FALSE,
                   keep.cols = c("geo.check",
                                 country.shape, country.gazetteer))

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
  #Selecting the right column(s)
  good.col <- ifelse(output == "same.col",
                     "geo.check", "geo.check.new")
  check_these <- grepl("invert_|trans", x4[, good.col], perl = TRUE)

  if (any(check_these)) {
    #Filtering the target coordinates
    x4.1 <- x4[check_these, ]
    x4.1 <- x4.1[, -which(names(x4.1) %in%
                            c("geo.check",
                              country.shape, country.gazetteer))]
    #Checking the new coordinates
    x4.2 <- checkCoord(x4.1,
                       lon = ifelse(output == "same.col",
                                    lon, paste0(lon, ".new")),
                       lat = ifelse(output == "same.col",
                                    lat, paste0(lat, ".new")),
                       low.map = low.map,
                       high.map = high.map,
                       dist.center = FALSE,
                       keep.cols = c("geo.check"))
    #Writing new checks for the inverted/swapped coordinates
    x4[check_these, good.col] <- paste0(x4.2$geo.check,
                                        gsub(".*(?=\\[)", "",
                                             x4[check_these, good.col], perl= TRUE))
  } else {
    lon.new <- ifelse(output == "same.col", lon, paste0(lon, ".new"))
    lat.new <- ifelse(output == "same.col", lat, paste0(lat, ".new"))
    x4[[lon.new]] <- x4[,lon, drop = TRUE]
    x4[[lat.new]] <- x4[,lat, drop = TRUE]
  }

  ## Removing unecessary columns and returning
  x4 <- x4[, -which(names(x4) %in% c(country.shape, country.gazetteer))]

  if (output == "new.col") {
    #Renaming the check columns
    x4$border.check.new[x4$border.check.new %in% TRUE] <-
      "bad_country[border]"
    x4$border.check.new[x4$border.check.new %in% c(FALSE, "FALSE")] <-
      "bad_country"
    x4$shore.check.new[x4$shore.check.new %in% TRUE] <-
      "shore"
    x4$shore.check.new[x4$shore.check.new %in% c(FALSE, "FALSE")] <-
      "open_sea"
  }

  ## Checking for records from cultivated individuals
  x5 <- getCult(x4)

  ## Checking for spatial outliers
  x6 <- checkOut(x5,
                 lon = ifelse(output == "same.col",
                              lon, paste0(lon, ".new")),
                 lat = ifelse(output == "same.col",
                              lat, paste0(lat, ".new")),
                 tax.name = tax.name,
                 geo.name = ifelse(output == "same.col",
                                   "geo.check", "geo.check.new"),
                 cult.name = "cult.check",
                 clas.cut = 3, rob.cut = 16)

  ## Checking for doubtful distribution
  x7 <- checkDist(x6,
                  tax.name = tax.name,
                  tax.author = tax.author,
                  sep = sep,
                  loc = loc,
                  source = source)

  return(x7)
}

