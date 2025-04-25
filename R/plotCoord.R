#' @title Plot Validation of Geographical Coordinates
#'
#' @description This function plots the comparison between the geographical
#' coordinates of records before and after their validation
#'
#' @param x a data frame with the occurrence data and the columns containing the
#'   outputs of the __plantR__ validation functions.
#' @param orig.lon character. Name of the column with the original longitude.
#'   Default to 'decimalLongitude.new'
#' @param orig.lat character. Name of the column with the original latitude.
#'   Default to 'decimalLatitude.new'
#' @param new.lon character. Name of the column with the edited/validated
#'   longitude. Default to 'decimalLongitude.new'
#' @param new.lat character. Name of the column with the edited/validated
#'   latitude. Default to 'decimalLatitude.new'
#' @param geo.check character. Column with the result from the coordinate
#'   checking. Default to 'geo.check'
#'
#'
#' @details This is a simple function to visually inspect the results of the
#'   __plantR__ routines to standardize, retrieve missing coordinates and
#'   validate the original ones.
#'
#'
#' @importFrom sf st_geometry
#' @importFrom grDevices dev.new
#' @importFrom graphics legend par points
#'
#' @seealso
#'  \link[plantR]{prepCoord}, \link[plantR]{getCoord},
#'  \link[plantR]{checkCoord}, , \link[plantR]{checkOut}, \link[plantR]{getCult}
#'
#' @keywords internal
#'
#'
plotCoord <- function(x, orig.lon = "decimalLongitude",
                      orig.lat = "decimalLatitude",
                      new.lon = "decimalLongitude.new",
                      new.lat = "decimalLatitude.new",
                      geo.check = "geo.check"
                      ) {

  #Escaping R CMD check notes
  world <- world

  ## check input
  if (!inherits(x, "data.frame"))
    stop("Input object needs to be a data frame!")

  if (dim(x)[1] == 0)
    stop("Input data frame is empty!")

  if (!all(c(orig.lon, orig.lat, new.lon, new.lat) %in% colnames(x)))
    stop("One or more column names declared do not match those of the input object: please rename or specify the correct names")

  ## Making sure coordinates are numerical
  x[[orig.lon]] <- suppressWarnings(as.numeric(x[[orig.lon]]))
  x[[new.lon]] <- suppressWarnings(as.numeric(x[[new.lon]]))
  x[[orig.lat]] <- suppressWarnings(as.numeric(x[[orig.lat]]))
  x[[new.lat]] <- suppressWarnings(as.numeric(x[[new.lat]]))

  ## Preparing the axes for plotting background map
  range.x.orig <- range(x[[orig.lon]], na.rm = TRUE)
  range.x.new <- range(x[[new.lon]], na.rm = TRUE)
  range.y.orig <- range(x[[orig.lat]], na.rm = TRUE)
  range.y.new <- range(x[[new.lat]], na.rm = TRUE)

  ## Preparing the original coordinates
  orig.coords <- is.na(x[[orig.lon]]) &
                      is.na(x[[orig.lat]])
  n.orig <- sum(!orig.coords, na.rm = TRUE)

  ## Preparing validated coordinates
  new.coords <- is.na(x[[new.lon]]) &
                  is.na(x[[new.lat]])
  n.new <- sum(!new.coords, na.rm = TRUE)
  cats.new <- x[[geo.check]][!new.coords]
  pchs.new <- ifelse(grepl("gazet", cats.new, fixed = TRUE), 24, 21)
  cats.new <- gsub("_gazet", "", cats.new, fixed = TRUE)
  cols.new <- rep(1, n.new)
  cols.new[cats.new %in% "ok_country"] <- "red"
  cols.new[cats.new %in% "ok_state"] <- 7
  cols.new[cats.new %in% "ok_county"] <- 3
  # cols.new[cats.new %in% "ok_locality"] <- "#006400"
  cols.new[cats.new %in% "ok_locality"] <- 3
  cols.new[cats.new %in% "shore"] <- 5
  cols.new[cats.new %in% "bad_country"] <- "magenta"
  cols.new[cats.new %in% "open_sea"] <- "magenta"
  cols.new[cats.new %in% "bad_country[border]"] <- "blue"

  ##Plotting
  w <- world
  grDevices::dev.new(width = 870, height = 450, noRStudioGD = TRUE)
  graphics::par(mfrow = c(1,2), tcl = -.25, las = 1, mgp = c(2, 0.3, 0),
      mar = c(4,2.5,2,0.25), xpd = FALSE)
  plot(sf::st_geometry(w),
       axes = TRUE, cex.axis = 0.7,
       xlim = range.x.orig, ylim = range.y.orig,
       main = paste0(n.orig, " records w/ original coords."),
       cex.main = 1)
  graphics::points(x[!orig.coords , c(orig.lon, orig.lat)],
         cex = 0.75, col = 1, pch = 21, bg = "red")
  graphics::par(xpd = TRUE)
  graphics::legend("bottomleft", "Original coordinates", inset=c(0,-0.15),
         # pch = 19, col = 1,
         pch = 21, col = 1, pt.bg = "red",
         cex = 0.7)

  graphics::par(mar = c(4,2,2,0.75), xpd = FALSE)
  plot(sf::st_geometry(w),
       axes = TRUE,  cex.axis = 0.7,
       xlim = range.x.new, ylim = range.y.new,
       main = paste0(n.new, " records w/ original+gazetter coords."),
       cex.main = 1)
  graphics::points(x[!new.coords , c(new.lon, new.lat)],
         cex = 0.75, col = 1, pch = pchs.new, bg = cols.new)
  graphics::par(xpd = TRUE)
  graphics::legend("bottomleft", c("Original","Gazetteer"),
                pch = c(21,24), #bty = "n",
                cex = 0.7, inset=c(0.0,-0.17))
  pos <- graphics::legend("bottomright", c("country","state","county/loc.","shore","bad","border"),
         pch = 21, col = 1, x.intersp = 1, ncol = 3,
         adj = c(-0.125, 0.4),
         pt.bg = c("red",7,3,5,"magenta","blue"),
         #col = c(1,1,1,1,1,"white"),
         cex = 0.7, inset=c(0,-0.17))
  # Plot symbols in two columns, shifted to the left by 3 and 1 respectively
  graphics::points(x=pos$text$x + pos$text$x*0.002,
         y=pos$text$y,
         pch=24, cex = 0.6,
         bg = c("red",7,3,5,"magenta","blue"))
  graphics::par(xpd = FALSE)
}

