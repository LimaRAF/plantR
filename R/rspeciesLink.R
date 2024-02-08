#' Gets occurrence data from speciesLink
#'
#' This function access version 1.0 of the
#' [speciesLink API](https://specieslink.net/ws/1.0/) and returns occurrence data of
#' species.
#'
#' @param key Character. Your personal specieslink API key. 
#' Generate one at \url{specieslink.net/aut/profile/apikeys}
#' @param dir Path to directory where the file will be saved. Default is to
#' create a "results/" directory
#' @param filename Name of the output file
#' @param save Logical. Save output to filename? Defaults to FALSE
#' @param basisOfRecord Character. Any in 'PreservedSpecimen', 'LivingSpecimen',
#'  'FossilSpecimen', 'HumanObservation', 'MachineObservation' or
#'  'MaterialSample'. Default is 'PreservedSpecimen' for museum and herbarium
#'  search
#' @param family Family name. More than one name should be concatenated in a
#'   vector
#' @param species Genus or genus and epithet separated by space. More than one
#'   species can be concatenated into a vector. The request cannot be done
#'   with more than 50 species at a time. Use lapply, or any sort of loop when
#'   dealing with multiple species.
#' @param collectionCode Any collection available at speciesLink. Example: ALCB,
#'  E, INPA, MOBOT_BR.  Accepts a vector of names
#' @param country Any country name. No ASCII characters allowed. Accepts a
#' vector of names
#' @param county Any municipality name. No ASCII characters allowed. Accepts a
#' vector of names
#' @param stateProvince Any state or province. No ASCII characters allowed.
#' Accepts a vector of names
#' @param Coordinates Specify if records should have coordinates. Default is
#' "no check" but it also accepts "Yes", No", "Original", "Automatic", "Blocked"
#'  or "no check"
#' @param CoordinatesQuality Any character in "Good" or "Bad" to select specific
#'  type of coordinates
#' @param Scope Group to be required. If NULL searches all groups. Any in
#' "p", "a", "m", "f" or "b" (plants, animals, microorganisms, fossil and broad(?))
#' @param Synonyms If species names should be checked for synonyms in a specific
#'  dictionary. Set to "species2000" for search in Catálogo da Vida species2000,
#'   "flora2020" for Flora do Brasil 2020, "MycoBank" for MycoBank, "AlgaeBase"
#'   for AlgaeBase, "DSMZ" for  DSMZ Prokaryotic Nomenclature Up-to-Date,
#'   "Moure" for Catálogo de Abelhas Moure or "no synonyms".
#' @param Typus Logic. If TRUE select only typus
#' @param Images If select only records with images. Default is NULL.
#' It accepts: "Yes", "Live", "Polen", "Wood"
#' @param RedList Logic. If TRUE only species in the IUCN Red List are returned
#' @param limit Numeric. Maximum number of records to be required. Default is 200, maximum is 5000.
#' @param file.format Character. The file extension to be used for saving ('csv'
#'   or 'rds'). Default to 'csv'.
#' @param compress Logical. Should the file be compressed? Default to FALSE.
#'
#' @return A data.frame with the search result, which can also be saved on disk
#'
#' @details The time necessary to download records from the speciesLink API will
#' depend on the number of records and species in the query and from the speed
#' of the internet connection.
#'
#' The speciesLink API does not allow the download of ~50 or more taxa at a
#' time. So to download records from larger lists of species, you will probably
#' need to make the queries in a loop (see Examples).
#'
#' @author Sara Mortara and João Vieira
#'
#' @examples
#'\dontrun{
#' # Example for a single species, saving into a file called "ex01"
#' ex01 <-
#'  rspeciesLink(filename = "ex01",
#'                     species =  c("Eugenia platyphylla"),
#'                     Scope = "p")
#' # Use lapply or similar for more than 50 species
#' # Making a request for multiple species
#'sp_list <- lapply(sp, function(x) rspeciesLink(species = x,
#'                                               Scope = "p",
#'                                               basisOfRecord = "PreservedSpecimen",
#'                                               Synonyms = "flora2020"))
#'# Adding species names to each element of the list
#'names(sp_list) = sp
#'# Binding all searchs and keeping a column w/ the named used in the search
#'sp_df <- bind_rows(sp_list, .id = "original_search")
#'}
#'
#' @import data.table
#' @importFrom jsonlite fromJSON
#' @export
rspeciesLink <- function(key = NULL,
                         dir = "results/",
                         filename = "output",
                         save = FALSE,
                         basisOfRecord = NULL,
                         family = NULL,
                         species = NULL,
                         collectionCode = NULL,
                         country = NULL,
                         stateProvince = NULL,
                         county = NULL,
                         Coordinates = NULL, #		Yes | No | Original | Automatic | Blocked
                         CoordinatesQuality = NULL,	#Good | Bad
                         Scope = NULL, #	a (animais) p (plantas) b (abrangente) m (microorganismos)  f (fósseis)
                         Synonyms = "no synomyms", #species2000 | flora2020 | MycoBank | AlgaeBase | DSMZ | Moure no synonyms
                         Typus = FALSE,
                         Images = NULL,
                         RedList = FALSE,
                         limit = NULL,
                         file.format = "csv",
                         compress = FALSE
) { # Yes | No | Live | Polen | Wood
  # speciesLink url
  my_url <- "https://specieslink.net/ws/1.0/search?"

  # helper function
  url_query <-  function(vector, name) {
    char <- paste(paste0(vector, "&"), collapse = "")
    url <- paste0(name, "=", char)
    return(url)
  }
  # api key
  if (is.null(key)) {
    stop("Please provide an API key! Get one at specieslink.net/aut/profile/apikeys")
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
  } else  {
    if (is.character(species)) {
      if (length(species) > 50)
        stop("Please make request of no more than 50 species at a time!")
      species <- gsub(" ", "+", species)
      sp <- url_query(species, "scientificName")
      my_url <- paste0(my_url, sp)
    } else {
      stop("species must be a character")
    }
  }
  # Family name
  if (is.null(family)) {
    my_url
  } else  {
    if (is.character(family)) {
      fam <- url_query(family, "family")
      my_url <- paste0(my_url, fam)
    }
    else {
      stop("family name must be a character")
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
      country <- gsub(" ", "+", country)
      ct <- url_query(country, "country")
      my_url <- paste0(my_url, ct)
    }
  }
  # stateProvince
  if (is.null(stateProvince)) {
    my_url
  } else {
    if (is.character(stateProvince)) {
      stateProvince <- gsub(" ", "+", stateProvince)
      st <- url_query(stateProvince, "stateProvince")
      my_url <- paste0(my_url, st)
    }
  }
  # county
  if (is.null(county)) {
    my_url
  } else {
    if (is.character(county)) {
      county <- gsub(" ", "+", county)
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
    if (Scope %in% c("p", "a", "m", "f", "b")) {
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
  if (is.null(limit)) {
    my_url
  } else {
    if (is.numeric(limit)) {
      mr <- url_query(limit, "limit")
      my_url <- paste0(my_url, mr)
    }
  }
  # making request
  my_url <- paste0(my_url,"apikey=", key) #Model/DwC is already default
  message("Making request to speciesLink...")
  #r <- httr::GET(my_url)
  #message("Extracting content ...")
  #rr <- httr::content(r, as="parse") # text content
  # requesting JSON format
  df <- jsonlite::fromJSON(my_url)$features$properties
  #rrr <- readr::read_tsv(rr, locale = readr::locale(encoding = "UTF-8"))

  if (save) {
    # creating dir
    if (!dir.exists(dir)) { dir.create(dir) }

    # fullname <- paste0(dir, filename, ".csv")
    # message(paste0("Writing ", fullname, " on disk."))
    # write.csv(df,
    #           fullname,
    #           row.names = FALSE)

    if (file.format == "csv") {

      if (compress) {
        fullname <- paste0(dir, filename, ".csv.zip")
        message(paste0("Writing ", fullname, " on disk."))
        data.table::fwrite(df, file = fullname, compress = "gzip")

      } else {
        fullname <- paste0(dir, filename, ".csv")
        message(paste0("Writing ", fullname, " on disk."))
        data.table::fwrite(df, file = fullname)
      }
    }

    if (file.format == "rds") {

      fullname <- paste0(dir, filename, ".rds")
      message(paste0("Writing ", fullname, " on disk."))

      if (compress) {
        saveRDS(df, file = fullname, compress = "gzip")

      } else {
        saveRDS(df, file = fullname, compress = FALSE)
      }
    }
  }
  # if output is empty, return message
  if (is.null(dim(df))) {
    warning("Output is empty. Check your request.")
  }

  warning("Please make sure that the restrictions and citation indicated by
  each speciesLink/CRIA data provider are observed and respected.")


  return(df)
}
