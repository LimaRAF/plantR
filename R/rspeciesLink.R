#' Gets occurrence data from speciesLink
#'
#' This function access version beta 0.1 of speciesLink API and returns
#' occurrence data of species.
#'
#' @param dir Path to directory where the file will be saved. Default is to create a "results/" directory
#' @param filename Name of the output file
#' @param basisOfRecord Character. Any in 'PreservedSpecimen', 'LivingSpecimen', 'FossilSpecimen',
#' 'HumanObservation', 'MachineObservation' or 'MaterialSample'. Default is 'PreservedSpecimen' for museum and herbarium search
#' @param species Genus and epithet separated by space. More than one should be concatenated in a vector
#' @param collectionCode Any collection available at speciesLink. Example: ALCB, E, INPA, MOBOT_BR.  Accepts a vector of names
#' @param country Any country name. No ASCII characters allowed. Accepts a vector of names
#' @param county Any municipality name. No ASCII characters allowed. Accepts a vector of names
#' @param stateProvince Any state or province. No ASCII characters allowed. Accepts a vector of names
#' @param Coordinates Specify if records should have coordinates. Default is "no check" but it also accepts "Yes", No", "Original", "Automatic", "Blocked" or "no check"
#' @param CoordinatesQuality Any character in "Good" or "Bad" to select specific type of coordinates
#' @param Scope Group to be required. If NULL searches all groups. Any in "plants", "animals", "microrganisms" or "fossils"
#' @param Synonyms If species names should be checked for synonyms in a specific dictionary. Set to "species2000" for search in Catálogo da Vida species2000, "flora2020" for Flora do Brasil 2020, "MycoBank" for MycoBank, "AlgaeBase" for AlgaeBase, "DSMZ" for  DSMZ Prokaryotic Nomenclature Up-to-Date, "Moure" for Catálogo de Abelhas Moure or "no synonyms".
#' @param Typus Logic. If TRUE select only typus
#' @param Images If select only records with images. Default is NULL. It accepts: "Yes", "Live", "Polen", "Wood"
#' @param RedList Logic. If TRUE only species in the IUCN Red List are returned
#' @param MaxRecords Numeric. Maximum number of records to be required
#' @return A list of two elements. The first element is a character string containing the url search and the second element is a data.frame with the search result. It also saves the output on disk
#' @author Sara Mortara
#'
#' @examples
#'\dontrun{
#'ex01 <- rspeciesLink(filename = "ex01",
#'                     species =  c("Eugenia platyphylla", "Chaetocalyx acutifolia"),
#'                     Scope = "plants")
#'}
#'
#' @importFrom jsonlite fromJSON
#' @importFrom utils write.table
#' @export
rspeciesLink <- function(dir = "results/",
                         filename = "output",
                         basisOfRecord = NULL,
                         species = NULL,
                         collectionCode = NULL,
                         country = NULL,
                         stateProvince = NULL,
                         county = NULL,
                         Coordinates = NULL, #		Yes | No | Original | Automatic | Blocked
                         CoordinatesQuality = NULL,	#Good | Bad
                         Scope = NULL, #			plants, animals, microrganisms,fossils
                         Synonyms = "no synomyms", #species2000 | flora2020 | MycoBank | AlgaeBase | DSMZ | Moure no synonyms
                         Typus = FALSE,
                         Images = NULL,
                         RedList = FALSE,
                         MaxRecords = NULL #		n > 0	 all records
) { # Yes | No | Live | Polen | Wood
  # speciesLink url
  my_url <- "https://api.splink.org.br/records/"
  # creting dir
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  # helper function
  url_query <-  function(vector, name) {
    char <- paste(paste0(vector, "/"), collapse = "")
    url <- paste0(name, "/", char)
    return(url)
  }
  # basis of record
  if (is.null(basisOfRecord)) {
    my_url
  } else {
    if (basisOfRecord %in% c(
      'PreservedSpecimen',
      'LivingSpecimen',
      'FossilSpecimen',
      'HumanObservation',
      'MachineObservation',
      'MaterialSample'
    )) {
      br <- url_query(basisOfRecord, "basisOfRecord")
      my_url <- paste0(my_url, br)
    }
  }
  # Species name
  if (is.null(species)) {
    my_url
  }
  else  {
    if (is.character(species)) {
      species <- gsub(" ", "%20", species)
      sp <- url_query(species, "scientificName")
      my_url <- paste0(my_url, sp)
    }
    else {
      stop("species must be a character")
    }
  }
  # Collection code
  if (is.null(collectionCode)) {
    my_url
  } else {
    if (is.character(collectionCode)) {
      cc <- url_query(collectionCode, "collectionCode")
      my_url <- paste0(my_url, cc)
    }
  }
  # country
  if (is.null(country)) {
    my_url
  } else {
    if (is.character(country)) {
      country <- gsub(" ", "%20", country)
      ct <- url_query(country, "country")
      my_url <- paste0(my_url, ct)
    }
  }
  # stateProvince
  if (is.null(stateProvince)) {
    my_url
  } else {
    if (is.character(stateProvince)) {
      stateProvince <- gsub(" ", "%20", stateProvince)
      st <- url_query(stateProvince, "stateProvince")
      my_url <- paste0(my_url, st)
    }
  }
  # county
  if (is.null(county)) {
    my_url
  } else {
    if (is.character(county)) {
      county <- gsub(" ", "%20", county)
      co <- url_query(county, "county")
      my_url <- paste0(my_url, co)
    }
  }
  # Coordinates
  if (is.null(Coordinates)) {
    my_url
  } else {
    if (Coordinates %in% c("Yes", "No", "Original", "Automatic", "Blocked")) {
      xy <- url_query(Coordinates, "Coordinates")
      my_url <- paste0(my_url, xy)
    }
  }
  # Coordinates quality
  if (is.null(CoordinatesQuality)) {
    my_url
  } else {
    if (CoordinatesQuality %in% c("Good", "Bad")) {
      cq <- url_query(CoordinatesQuality, "CoordinatesQuality")
      my_url <- paste0(my_url, cq)
    }
  }
  # Scope
  if (is.null(Scope)) {
    my_url
  } else {
    if (Scope %in% c("plants", "animals", "microrganisms", "fossils")) {
      sc <- url_query(Scope, "Scope")
      my_url <- paste0(my_url, sc)
    }
  }
  #  Synonyms
  # if (length(species) > 9) {
  #    stop("Function does not support synonym check of more than nine species")
  #  } else {
  if (is.null(Synonyms)) {
    my_url
  } else {
    if (Synonyms %in% c("species2000", "flora2020",
                        "MycoBank", "AlgaeBase", "DSMZ")) {
      sy <- url_query(Synonyms, "Synonyms")
      my_url <- paste0(my_url, sy)
    }
  }
  #  }
  #  Typus
  if (Typus == FALSE) {
    my_url
  } else {
    my_url <- paste0(my_url, "Typus/Yes/")
  }
  # Images # "Yes", "Live", "Polen", "Wood"
  if (is.null(Images)) {
    my_url
  } else {
    if (Images %in% c("Yes", "Live", "Polen", "Wood")) {
      im <- url_query(Images, "Images")
      my_url <- paste0(my_url, im)
    }
  }
  # RedList
  if (RedList == FALSE) {
    my_url
  } else {
    my_url <- paste0(my_url, "RedList/Yes/")
  }
  # MaxRecords
  if (is.null(MaxRecords)) {
    my_url
  } else {
    if (is.numeric(MaxRecords)) {
      mr <- url_query(MaxRecords, "MaxRecords")
      my_url <- paste0(my_url, mr)
    }
  }
  # making request
  my_url <- paste0(my_url, "Format/JSON/") #Model/DwC is already default
  message("Making request to speciesLink...")
  #r <- httr::GET(my_url)
  #message("Extracting content ...")
  #rr <- httr::content(r, as="parse") # text content
  # requesting JSON format
  rrr <- jsonlite::fromJSON(my_url)$result
  #rrr <- readr::read_tsv(rr, locale = readr::locale(encoding = "UTF-8"))
  fullname <- paste0(dir, filename, ".csv")
  message(paste0("Writing ", fullname, " on disk."))
  write.table(rrr,
              fullname,
              sep = ",",
              row.names = FALSE,
              col.names = TRUE)
  # if output is empty, return message
  if (is.null(dim(rrr))) {
    message("Output is empty. Check your request.")
  }
  return(list(data = rrr, url = my_url))
}
