#' Gets occurrence data from jabot
#'
#' This function gets occurrence data in Jabot's API (http://servicos.jbrj.gov.br/jabot/). Providing a scientific name for a species is mandatory.
#'
#' @inheritParams rspeciesLink
#'
#' @return A list of two elements. The first element is a character string containing the url search and the second element is a data.frame with the search result. It also saves the output on disk
#' @author Sara Mortara
#'
#'
#' @importFrom jsonlite fromJSON
#' @importFrom utils write.table
#'
#' @export

rjabot <- function(dir = "results/",
                   filename = "output",
                   species = NULL
                   #collectionCode = NULL,
                   #stateProvince = NULL,
                   #county = NULL,
                   #genus = NULL,
                   #family = NULL
) {

  # my_url <- "http://servicos.jbrj.gov.br/jabot/occurrence"
  # my_url <- "http://servicos.jbrj.gov.br/jabot/occurrence/splendens/Myrcia/Myrtaceae"
  # rrr <- jsonlite::fromJSON(my_url)
  # rrr

}
