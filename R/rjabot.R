#' Gets occurrence data from jabot
#'
#' This function gets occurrence data in the Jabot database (http://jabot.jbrj.gov.br/v3/consulta.php).
#'
#' @inheritParams rspeciesLink
#'
#' @param collectionCode any herbarium in the Jabot system (http://jabot.jbrj.gov.br/v3/herbarios.php)
#'#' @return A list of two elements. The first element is a character string containing the url search and the second element is a data.frame with the search result. It also saves the output on disk
#' @author Sara Mortara
#'
#' @examples
#'
#'ex_jabot <- rjabot(filename = "ex-jabot",
#'                   scientificName =  c("Eugenia platyphylla", "Chaetocalyx acutifolia"))
#'
#' @importFrom jsonlite fromJSON
#' @importFrom utils write.table
#'
#' @export
rjabot <- function(dir = "results/",
                   filename = "output",
                   scientificName = NULL,
                   collectionCode = NULL,
                   stateProvince = NULL,
                   county = NULL
) {
  # jabot url
  my_url <- "https://model-r.jbrj.gov.br/modelr-web/execjabot.php?"
  # creting dir
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  # helper function
  url_query <-  function(vector, name) {
    char <- paste(paste0(vector), collapse = ",")
    url <- paste0(name, "=", char)
    return(url)
  }
  # Species name
  if (is.null(scientificName)) {
    my_url
  }
  else  {
    if (is.character(scientificName)) {
      scientificName <- tolower(gsub(" ", "%20", scientificName))
      sp <- url_query(scientificName, "especie")
      my_url <- paste0(my_url, '&', sp)
    }
    else
      stop("scientificName must be a character")
  }
  # Collection code
  if (is.null(collectionCode)) {
    my_url
  } else {
    if (is.character(collectionCode)) {
      cc <- url_query(collectionCode, "herbario")
      my_url <- paste0(my_url, '&', cc)
    }
  }
  # country
  # if (is.null(country)) {
  #   my_url
  # } else {
  #   if (is.character(country)) {
  #     country <- gsub(" ", "%20", country)
  #     ct <- url_query(country, "country")
  #     my_url <- paste0(my_url, ct)
  #   }
  # }
  # stateProvince
  if (is.null(stateProvince)) {
    my_url
  } else {
    if (is.character(stateProvince)) {
      stateProvince <- gsub(" ", "%20", stateProvince)
      st <- url_query(stateProvince, "stateProvince")
      my_url <- paste0(my_url, '&', st)
    }
  }
  # county
  if (is.null(county)) {
    my_url
  } else {
    if (is.character(county)) {
      county <- gsub(" ", "%20", county)
      co <- url_query(county, "county")
      my_url <- paste0(my_url, '&', co)
    }
  }
  # making request
  message("Making request to jabot...")
  # requesting JSON format
  rrr <- jsonlite::stream_in(url(my_url))
  fullname <- paste0(dir, filename, ".csv")
  message(paste0("Writing ", fullname, " on disk."))
  # renaming col names in data.frame
  names(rrr) <-
  write.table(
    rrr,
    fullname,
    sep = ",",
    row.names = FALSE,
    col.names = TRUE
  )
  # # if output is empty, return message
  if (is.null(dim(rrr))) {
    message("Output is empty. Check your request.")
  }
  return(list(data = rrr, url = my_url))
}
