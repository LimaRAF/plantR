#' Dataset to illustrate the data editing and validation
#'
#' A dataset containing the retrieved records of the species
#' *Trema micrantha* and *Casearia sylvestris* downloaded from the
#' speciesLink network in May 2024. Data was filtered to contain only
#' preserved specimens and the columns most relevant to `plantR`
#'
#' @keywords datasets
#' @name example
#' @usage data(example)
#' @format A data frame with 18,855 rows and 19 variables
#' @source \url{https://specieslink.net/}
"example"

#' Dataset for the package introductory vignette
#'
#' A dataset containing the retrieved records of the species
#' *Euterpe edulis* downloaded from the speciesLink network
#' in May 2024. Data was filtered to contain only the columns most
#' relevant to `plantR`
#'
#' @keywords datasets
#' @name example_intro
#' @usage data(example_intro)
#' @format A data frame with 713 rows and 45 variables
#' @source \url{https://specieslink.net/}
"example_intro"


#' World Map
#'
#' Map used to perform the validation of the original geographical
#' coordinates at the lowest administrative level (e.g. country).
#' Names were edited to match the __plantR__ default gazetteer, using
#' the same __plantR__ function used for locality editing (i.e.
#' `fixLoc()`).
#'
#' Original polygons were simplified (tolerance: 0.001 decimal
#' degrees) to make maps smaller in size and thus less burdensome for
#' the geographical validation procedures.
#'
#' Maps currently used are from the version 4.1 of [GADM](https://gadm.org/)
#'
#' Reference system: WSG84
#'
#' @keywords datasets
#' @name worldMap
#' @usage data(worldMap)
#' @format A Multipolygon 'sf' object with 263 features
#' @source \url{https://geodata.ucdavis.edu/gadm/gadm4.1/}
"worldMap"

#' Maps of Latin American countries and dependent territories
#'
#' Maps used to perform the validation of the original geographical
#' coordinates. For each country, the map contains the lowest
#' administrative level available at [GADM](https://gadm.org). We
#' considered an initial total of 51 countries and dependent
#' territories for Latin America, but since some of them are
#' available only at the country level (e.g. Aruba), they were not
#' included in this map (total of 46 countries). The Latin
#' American definition used here include all American territories but
#' the United States and Canada.
#'
#' Locality names (e.g. country, state, county) were edited to match
#' the __plantR__ default gazetteer, using the same __plantR__
#' functions used for locality editing (i.e. `fixLoc()`, `prepLoc()`,
#' , `prepCountry()`, `prepState()`).
#'
#' We cross-checked and corrected issues in GADM (version 4.1), mostly
#' for Brazil, related to municipality names with typos, old toponyms,
#' switched names and even bad name assignment to polygons (e.g.
#' Manaus assigned under another municipality, Maués). An internal
#' list of those changes (`gadmCheck`) is provided for
#' reproducibility.
#'
#' Finally, polygons were simplified (tolerance: 0.0001 decimal
#' degrees) to make maps smaller in size and thus less burdensome for
#' the geographical validation procedures.
#'
#' Reference system: WSG84
#'
#' @keywords datasets
#' @name latamMap
#' @usage data(latamMap)
#' @format A list of 46 Multipolygon 'sf' objects
#' @source \url{https://gadm.org/download_country.html}
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
#' generating and aggregating the buffered polygons.
#'
#' Reference system: WSG84
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
#' after generating and aggregating the buffered polygons.
#'
#' Reference system: WSG84
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
#' of each country were aggregated, then simplified (tolerance: 0.001 decimal
#' degrees) and finally converted into 'spatial lines'. For some reason this
#' procedure generated objects smaller in size than the 'coastline' maps
#' available in 'Natural Earth'.
#'
#' Reference system: WSG84
#'
#' @keywords datasets
#' @name shoreLines
#' @usage data(shoreLines)
#' @format a MULTILINESTRING 'sf' object.
#' @source \url{https://www.naturalearthdata.com/downloads/50m-physical-vectors/50m-land/}
"shoreLines"

#' Simplified World Map
#'
#' Map used to support the validation of the original geographical
#' coordinates, mainly regarding the detection of problematic
#' coordinates falling outside its country borders. It was generated
#' from the smallest scale available in 'Natural Earth' (i.e. 1:110
#' m), which consists in 183 polygons for 178 countries in the world,
#' plus the Caspian Sea. This map was complemented with 77 polygons
#' for missing countries and dependent territories (mainly islands),
#' not included in 'Natural Earth', so that this map can match the
#' other maps used in __plantR__.
#'
#' Country long names were edited to match the format of the
#' __plantR__ default gazetteer and original polygons were simplified
#' (tolerance: 0.001 decimal degrees). Only the ISO 3166-1 alpha-2
#' codes and the long names of each country are stored with the
#' polygons.
#'
#' Reference system: WSG84
#'
#' @keywords datasets
#' @name world
#' @usage data(world)
#' @format a MULTIPOLYGON 'sf' object with 261 features and 2 fields:
#'   iso_a2 (country code) and 'name' (edited country name).
#' @source \url{https://www.naturalearthdata.com/downloads/110m-cultural-vectors/110m-admin-0-details/}
#'
"world"

#' @title Brazilian Flora Online Vascular Plant Taxonomy
#'
#' @description A dataset containing the essential taxonomic
#'   information of vascular plants stored in the
#'   [Brazilian Flora 2020](https://floradobrasil.jbrj.gov.br/consulta/)
#'   taxonomic backbone, a.k.a. the Flora and Funga of Brazil. It
#'   includes all taxonomic levels (i.e. infra-species, species,
#'   genus, family and so on). See package __plantRdata__ for more
#'   details on how this table was obtained.
#'
#' @keywords datasets
#' @name bfoNames
#' @usage data(bfoNames)
#' @source \url{https://ipt.jbrj.gov.br/jbrj/resource?r=lista_especies_flora_brasil}
#' @evalRd readScript("data-raw/last_update_bfo.txt",
#'  "Last update/change of the downloaded backbone (day month year):")
#' @format An object of class \code{data.frame} with 14 columns and
#'   over 125 thousand rows.
#'
"bfoNames"

