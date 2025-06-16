#' Gets occurrence data from speciesLink
#'
#' This function access the Web service API v1.0 of the
#' [speciesLink](https://specieslink.net/ws/1.0/) network and returns
#' species occurrence data
#'
#' @param dir Path to directory where the file will be saved. Default
#'   is to create a "results/" directory
#' @param filename Name of the output file
#' @param save Logical. Save output to filename? Defaults to FALSE
#' @param key Character. Your personal specieslink API key.
#' @param basisOfRecord Character. Any in 'PreservedSpecimen',
#'   'LivingSpecimen', 'FossilSpecimen', 'HumanObservation',
#'   'MachineObservation' or 'MaterialSample'. Default is
#'   'PreservedSpecimen' for museum and herbarium records.
#' @param family Character. The family name. More than one name should
#'   be concatenated in a vector.
#' @param species Character. The genus name or the genus and epithet separated by
#'   a space. More than one species can be concatenated into a vector.
#'   The request cannot be done with more than 50 species at a time.
#'   Use lapply, or any sort of loop when dealing with multiple
#'   species (see example).
#' @param collectionCode Character. Any collection available at speciesLink.
#'   Example: ALCB, E, INPA, MOBOT_BR.  Accepts a vector of names
#' @param country Character. Any country name. No ASCII characters allowed.
#'   Accepts a vector of names
#' @param county Character. Any municipality name. No ASCII characters allowed.
#'   Accepts a vector of names
#' @param stateProvince Character. Any state or province. No ASCII characters
#'   allowed. Accepts a vector of names
#' @param Coordinates Character. Specify if records should have coordinates.
#'   Default is "no check" but it also accepts "Yes", No", "Original",
#'   "Automatic", "Blocked" or "no check"
#' @param CoordinatesQuality Character. Any character in "Good" or "Bad" to
#'   select specific type of coordinates (argument is now deprecated)
#' @param Scope Character. Major group to be required. If NULL
#'   searches all groups. Any in "p" (plants), "a" (animals), "m"
#'   (microorganisms), "f" (fossil) or "b" (broad scope)
#' @param Synonyms If species names should be checked for synonyms in
#'   a specific dictionary. Set to "sp2000" for search in
#'   Catálogo da Vida species2000, "flora2020" for Flora do Brasil
#'   2020, "MycoBank" for MycoBank, "AlgaeBase" for AlgaeBase, "DSMZ"
#'   for  DSMZ Prokaryotic Nomenclature Up-to-Date, "Moure" for
#'   Catálogo de Abelhas Moure or "no synonyms".
#' @param Typus Logical. If TRUE select only typus. Defaults to FALSE.
#' @param Images Logical. If TRUE select only records with images.
#'   Defaults to FALSE.
#' @param RedList Character. Any of the following threat categories:
#'   "CR" (critically endangered), "PEX" (Probably extinct), "EN"
#'   (endangered), "EW" (Extinct in nature), "EX" (Extinct), "RE"
#'   (Extinct in Brazil) or "VU" (Vulnerable)
#' @param MaxRecords Numeric. Maximum number of records to be
#'   required. Default is 200, maximum is 5000.
#' @param file.format Character. The file extension to be used for
#'   saving ('csv' or 'rds'). Default to 'csv'.
#' @param compress Logical. Should the file be compressed? Default to
#'   FALSE.
#' @param offset Index of the first record to be returned, used for pagination. Starts at zero.
#'
#' @return A data.frame with the search result, which can also be
#'   saved on disk
#'
#' @details The time necessary to download records from the
#'   speciesLink API will depend on the number of records and species
#'   in the query and from the speed of the internet connection.
#'
#'   The speciesLink API does not allow the download of more than 5000
#'   records at a time. So, to download records from species with
#'   large number of records (>5000), you will probably need to make
#'   the queries in a loop (see Examples). Use offset to skip the
#'   records you have already downloaded.
#'
#'   Also, the speciesLink API does not allow the download of ~50 or
#'   more taxa at a time. So to download records from larger lists of
#'   species, you will also need to make the queries in a loop
#'   (see Examples).
#'
#' @author Sara Mortara, João Vieira and Renato A. Ferreira de Lima
#'
#' @examples
#'\dontrun{
#' # Example for a single species, saving into a file called "ex01"
#'
#' key <- (ADD HERE YOUR PERSONAL API KEY)
#'
#' ex01 <- rspeciesLink(species =  c("Eugenia platyphylla"),
#'                      Scope = "p",
#'                      key = key)
#'
#'
#' # Use lapply or similar for more than 50 species
#'
#' # Making a request for multiple species
#' sp <- c("Eugenia neoriedeliana", "Eugenia platyphylla")
#' sp_list <- lapply(sp, function(x) rspeciesLink(species = x,
#'                                               Scope = "p",
#'                                               basisOfRecord = "PreservedSpecimen",
#'                                               Synonyms = "flora2020",
#'                                               key = key))
#' # Adding species names to each element of the list
#' names(sp_list) = sp
#' # Binding all searchs and keeping a column w/ the named used in the search
#' sp_df <- bind_rows(sp_list, .id = "original_search")
#'
#' # Getting more than 5000 records
#'
#' offset <- 0
#' ex <- rspeciesLink(county = "Rio de Janeiro",
#'                      Scope = "p",
#'                      MaxRecords = 5000,
#'                      key = key)
#' rj_df <- ex
#'
#' while(nrow(ex) == 5000)
#'    offset <- offset + 5000
#'    ex <- rspeciesLink(county = "Rio de Janeiro",
#'                      Scope = "p",
#'                      MaxRecords = 5000,
#'                      offset = offset,
#'                      key = key)
#'    rj_df <- bind_rows(rj_df, ex)
#'}
#'
#' @importFrom data.table fwrite
#' @importFrom jsonlite fromJSON
#'
#' @export
rspeciesLink <- function(dir = "results/",
                         filename = "output",
                         save = FALSE,
                         key = NULL,
                         basisOfRecord = NULL,
                         family = NULL,
                         species = NULL,
                         collectionCode = NULL,
                         country = NULL,
                         stateProvince = NULL,
                         county = NULL,
                         Coordinates = NULL,
                         CoordinatesQuality = NULL,
                         Scope = NULL,
                         Synonyms = "no synomyms",
                         Typus = FALSE,
                         Images = FALSE,
                         RedList = NULL,
                         MaxRecords = 5000,
                         file.format = "csv",
                         compress = FALSE,
                         offset = 0,
                         ...)
{

  if (is.null(key))
    stop("Please provide an API key! Get one at specieslink.net/aut/profile/apikeys")

  if (!missing("CoordinatesQuality"))
    warning("Argument deprecated")

  # speciesLink url
  my_url <- "https://specieslink.net/ws/1.0/search?"

  # helper function
  url_query <-  function(vector, name) {
    # char <- paste(paste0(vector, "&"), collapse = "")
    char <- paste0(paste0(vector, collapse = ","), "&")
    url <- paste0(name, "=", char)
    url <- gsub(" ", "+", url)
    return(url)
  }
  # basis of record
  if (is.null(basisOfRecord)) {
    my_url
  } else {
    if (tolower(basisOfRecord) %in% c(
                              'preservedspecimen',
                              'livingspecimen',
                              'fossilspecimen',
                              'humanobservation',
                              'machineobservation',
                              'materialsample')) {
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
        stop("Please make request of less than 50 species at a time!")
      species <- gsub(" ", "+", species)
      species <- paste0(species, collapse = ",")
      sp <- url_query(species, "scientificName")
      my_url <- paste0(my_url, sp)
    } else {
      stop("Genus or species name must be a character")
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
      stop("Family name must be a character")
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
    if (tolower(Coordinates) %in% c(
                           "yes",
                           "no",
                           "original",
                           "automatic",
                           "blocked",
                           "suspect",
                           "consistent")) {
      xy <- url_query(Coordinates, "Coordinates")
      my_url <- paste0(my_url, xy)
    }
  }
  # Scope
  if (is.null(Scope)) {
    my_url
  } else {
    if (tolower(Scope) %in% c("p", "a", "m", "f", "b")) {
      sc <- url_query(Scope, "Scope")
      my_url <- paste0(my_url, sc)
    }
  }

  #  Synonyms
  # if (length(species) > 9) {
  #    stop("Function does not support synonym check of >9 species")
  #  } else {
  if (is.null(Synonyms)) {
    my_url
  } else {
    if (Synonyms %in% c("sp2000", "flora2020", "mycobank",
                        "algaebase", "dsmz", "moure")) {
      sy <- url_query(Synonyms, "Synonyms")
      my_url <- paste0(my_url, sy)
    }
  }
  #  }

  #  Typus
  if (Typus == FALSE) {
    my_url
  } else {
    typ <- url_query("Yes", "Typus")
    my_url <- paste0(my_url, typ)
  }

  # Images # "Yes", "Live", "Polen", "Wood"
  if (Images == FALSE) {
    my_url
  } else {
      im <- url_query("Yes", "Images")
      my_url <- paste0(my_url, im)
  }

  # RedList
  if (is.null(RedList)) {
    my_url
  } else {
    if (tolower(RedList) %in% c("cr", "pex", "en", "ew", "ex",
                                "re", "vu")) {
      red <- url_query(RedList, "RedList")
      my_url <- paste0(my_url, red)
    }
  }

  # MaxRecords
  if (is.null(MaxRecords)) {
    my_url
  } else {
    if (is.numeric(MaxRecords)) {
      mr <- url_query(MaxRecords, "limit")
      my_url <- paste0(my_url, mr)
    }
  }

  # Offset
  if (!is.null(offset)) {
    os <- as.integer(offset)
    if(is.na(os)) {
      stop(paste("Offset must be an integer number. Got", offset))
    }
    q <- url_query(offset, "offset")
    my_url <- paste0(my_url, q)
  }

  # Add extra parameters
  my_params <- list(...)
  ns <- names(my_params)

  for(name in ns) {
    q <- url_query(my_params[[name]], name)
    my_url <- paste0(my_url, q)
  }


  # making the request
  my_url <- paste0(my_url, "apikey=", key)
  message("Making request to speciesLink...")
  df <- jsonlite::fromJSON(my_url)$features$properties

  if (save) {
    # creating dir
    if (!dir.exists(dir)) { dir.create(dir) }

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

  message("Important note: Please make sure that the restrictions and citation indicated by each speciesLink/CRIA data provider are observed and respected.")

  return(df)
}
