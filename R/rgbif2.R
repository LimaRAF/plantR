#' Gets occurrence data from GBIF
#'
#' This function uses the rgbif package to get occurrence data of a species.
#'
#' @param dir Path to directory where the file will be saved. Default is to create a "results/" directory
#' @param filename Name of the output file
# @param basisofrecord Character. Any in 'PreservedSpecimen', 'LivingSpecimen', 'FossilSpecimen',
# 'HumanObservation', 'MachineObservation' or 'MaterialSample'. Default is 'PreservedSpecimen' for museum and herbarium search
# @param country
# @param stateProvince
# @param collectionCode
# @param basisOfRecord
# @param hasCoordinate
#' @param scientificName Genus and epithet separated by space
#' @param ... any arguments from occ_search in rgbif package
#' @return A data.frame with the search result. Also saves the output on disk.
#'
#' @author Sara Mortara
#'
#' @examples
#'
#' ex_rgbif <- rgbif2(filename = "ex-gbif",
#'                    scientificName =  c("Asplenium truncorum"))
#' @importFrom rgbif name_backbone
#' @importFrom rgbif occ_search
#' @importFrom utils write.table
#' @export
#'
rgbif2 <- function(dir = "results/",
                   filename = "output",
                   scientificName,
                   ...
) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  key <- rgbif::name_backbone(name = scientificName)$speciesKey
  if (!is.null(key)) {
    message("Making request to GBIF...")
    gbif_data <- rgbif::occ_search(
      hasCoordinate = TRUE,
      hasGeospatialIssue = F,
      taxonKey = key,
      return = "data", ...
    )
    gbif_data <- gbif_data[!is.na(gbif_data$decimalLongitude) & !is.na(gbif_data$decimalLatitude),]
    fullname <- paste0(dir, filename, ".csv")
    message(paste0("Writing ", fullname, " on disk."))
    write.table(gbif_data,
                fullname,
                sep = ",",
                row.names = FALSE,
                col.names = TRUE)
    return(gbif_data)
  } else {
    message(paste0("Please insert a valid scientific name."))
  }
}
