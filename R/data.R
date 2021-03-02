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
#' It consists in a 0.5 degree buffer (~ 55 km in the Equator) around continents
#' and major islands (scalerank < 3) and a 0.25 degree buffer (~ 30 km in the
#' Equator) around medium islands (scalerank < 3). All buffers were then
#' aggregated and the aggregated buffers were simplified (tolerance 0.01 decimal
#' degrees). Note that buffers were generated at the largest scale available
#' in 'Natural Earth' (i.e. 1:10 m) and simplification was performed after
#' generating and aggregating the buffered polygons. Reference system: WSG84
#'
#' @keywords datasets
#' @name landBuff
#' @usage data(landBuff)
#' @format a MULTIPOLYGON 'sf' object with one class: land.
#' @source \url{https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-land/}
"landBuff"

#' Buffer of World Minor Islands
#'
#' Map used to support the validation of the original geographical coordinates.
#' It consists in a 0.25 degree buffer (~ 30 km in the Equator) around the minor
#' islands of the world (2 square km or less in size; scalerank >5). All buffers
#' were aggregated and the aggregated buffers were simplified (tolerance 0.01
#' decimal degrees). Note that buffers were generated at the largest scale
#' available in 'Natural Earth' (i.e. 1:10 m) and simplification was performed
#' after generating and aggregating the buffered polygons. Reference system:
#' WSG84
#'
#' @keywords datasets
#' @name islandsBuff
#' @usage data(islandsBuff)
#' @format a MULTIPOLYGON 'sf' object with one class: land.
#' @source \url{https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-minor-islands/}
"islandsBuff"

#' World Shore Lines
#'
#' Map used to support the validation of the original geographical coordinates.
#' It was generated from the map available in 'Natural Earth' at medium scale
#' (i.e. 1:50 m), which includes continents and major islands. Original polygons
#' of each country were aggregated, then simplified (tolerance 0.001 decimal
#' degrees) and finally converted into 'spatial lines'. For some reason this
#' procedure generated objects smaller in size than the 'coastline' maps
#' available in 'Natural Earth'. Reference system: WSG84
#'
#' @keywords datasets
#' @name shoreLines
#' @usage data(shoreLines)
#' @format a MULTILINESTRING 'sf' object.
#' @source \url{https://www.naturalearthdata.com/downloads/50m-physical-vectors/50m-land/}
"shoreLines"

#' Simplified World Map
#'
#' Map used to support the validation of the original geographical coordinates,
#' mainly regarding the detection of problematic coordinates fallign just
#' outside its country corders. It was generated from the smallest scale available
#' in 'Natural Earth' (i.e. 1:110 m), which consists in 255 polygons for 247
#' countries in the world. Country long names were edited to match the
#' __plantR__ default gazetteer and original polygons were simplified (tolerance
#' 0.001 decimal degrees). Only the ISO 3166-1 alpha-2 codes and the long names
#' of each country are stored with the polygons. Reference system: WSG84
#'
#' @keywords datasets
#' @name world
#' @usage data(world)
#' @format a MULTIPOLYGON 'sf' object with 255 features and 2 fields: iso_a2
#'   (country code) and 'name' (country name).
#' @source \url{https://www.naturalearthdata.com/downloads/110m-cultural-vectors/110m-admin-0-countries/}
"world"
