#' Dataset to illustrate the data editing and validation
#'
#' A dataset containing the retrieved records of the species *Trema micrantha*
#' and *Casearia sylvestris* downloaded from the speciesLink network in June
#' 2020.
#'
#' @keywords datasets
#' @name example
#' @usage data(example)
#' @format A data frame with 16,491 rows and 51 variables
#' @source \url{http://splink.cria.org.br}
"example"

#' World Map
#'
#' Map used to perform the validation of the original geographical coordinates.
#' Country names were edited to match the __plantR__ default gazetteer and
#' original polygons were simplified (tolerance 0.001 decimal degrees).
#' Reference system: WSG84
#'
#' @keywords datasets
#' @name worldMap
#' @usage data(worldMap)
#' @format A Multipolygon 'sf' object with 256 features
#' @source \url{https://gadm.org}
"worldMap"

#' Maps of Latin American countries
#'
#' Maps used to perform the validation of the original geographical coordinates.
#' For each country, the map contains the lowest administrative level
#' available. Country, state and county (if present) names were edited to match
#' the __plantR__ default gazetteer and original polygons were simplified
#' (tolerance 0.0001 decimal degrees). Reference system: WSG84
#'
#' @keywords datasets
#' @name latamMap
#' @usage data(latamMap)
#' @format A list of 44 Multipolygon 'sf' objects
#' @source \url{https://gadm.org}
"latamMap"

#' Buffer of World Land Map
#'
#' Map used to support the validation of the original geographical coordinates.
#' It consists in a 0.5 degree buffer (~ 55 km in the Equator) around the world
#' land map. The original polygons were simplified (tolerance 0.005 decimal
#' degrees). Reference system: WSG84
#'
#' @keywords datasets
#' @name landBuff
#' @usage data(landBuff)
#' @format an MULTIPOLYGON 'sf' object with one class: land.
#' @source \url{http://www.naturalearthdata.com/downloads/10m-physical-vectors/}
"landBuff"

#' World Shore Lines
#'
#' Map used to support the validation of the original geographical coordinates.
#' It consists the shore lines of the world. The original polygons were
#' simplified (tolerance 0.005 decimal degrees). Reference system: WSG84
#'
#' @keywords datasets
#' @name shoreLines
#' @usage data(shoreLines)
#' @format an MULTILINESTRING 'sf' object.
#' @source \url{http://www.naturalearthdata.com/downloads/10m-physical-vectors/}
"shoreLines"
