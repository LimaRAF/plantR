#' @title Prepare For Duplicate Specimen Search
#'
#' @description This function ...
#'
#' @param x a data frame with the occurrence data.
#'
#' @details The input data frame \code{x} must contain at least the columns ...
#'
#' @importFrom
#'
#' @export prepDup
#'
prepDup <- function(x) {

  ##Removing unwanted columns
  cols = c("collectionCode","catalogNumber",
           "family","family.new","scientificName","scientificName.new","suggestedName",
           "recordedBy","recordedBy.new","last.name",
           "recordNumber","recordNumber.new",
           "loc.correct","municipality","municipality.new","locality","locality.new",
           "year","year.new",
           "dateIdentified","yearIdentified","yearIdentified.new")
  cls <-  unique(cols[cols %in% names(x)])
  x1 <- x[,cls]

}
