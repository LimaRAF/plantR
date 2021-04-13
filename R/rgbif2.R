#' Gets occurrence data from GBIF
#'
#' This function uses the rgbif package to get occurrence data of a species.
#'
#' @param dir Path to directory where the file will be saved. Default is to
#'   create a "results/" directory
#' @param filename Name of the output file
#' @param species Name of the family, genus or species (i.e. genus and specific
#'   epithet separated by space). Accepts author if inserted correctly. Either a
#'   single character or a vector.
#' @param n.records Number of records to return. Default to 5000.
#' @param force Logical. Force downloading data for more than 10 species in a
#'   loop. Default `force = FALSE`
#' @param remove_na Logical. Defalt `TRUE` removes NA in columns decimalLatitude
#'   and decimalLongitude
#' @param save Logical. Save output to filename? Defaults to FALSE
#' @param file.format Character. The file extension to be used for saving ('csv'
#'   or 'rds'). Default to 'csv'.
#' @param compress Logical. Should the file be compressed? Default to FALSE.
#' @param ... Any argument from occ_search in rgbif package
#'
#' @return A data.frame with the search result. Columns species_id and download
#'   are added by the function. It can save the search output on disk
#'
#' @author Sara Mortara & Andrea Sánchez-Tapia
#'
#' @examples
#' \dontrun{
#' ex_rgbif <- rgbif2(filename = "ex-gbif",
#'                    species =  "Asplenium truncorum")
#' }
#'
#' @import data.table
#' @importFrom rgbif name_backbone
#' @importFrom rgbif occ_search
#' @importFrom dplyr bind_rows
#' @export
rgbif2 <- function(dir = "results/",
                   filename = "output",
                   species,
                   n.records = 5000,
                   force = FALSE,
                   remove_na = TRUE,
                   save = FALSE,
                   file.format = "csv",
                   compress = FALSE,
                   ...) {
  if (length(species) > 9) {
    if (!force) {
      stop("Use force = TRUE if you still want to download data for more than 10 species")
    }
  }
  #Getting the key
  key <- sapply(species, function(x) rgbif::name_backbone(name = x)$usageKey)
  # key <- sapply(species, function(x) rgbif::name_backbone(name = x)$speciesKey)
  key_pointer <- sapply(key, function(x) !is.null(x))
  # Adding message if any species not found
  if (!all(key_pointer)) {
    message("\nFamily, genus or species not found: ", species[!key_pointer],
            "\nReturning data only for: ",
            paste(species[key_pointer], collapse = ", "), "\n")
  }
  # Loop for each valid species
  gbif_data <- list()
  for (i in 1:length(key_pointer)) {
    if (key_pointer[i]) {
      message(paste0("Making request to GBIF for ", names(key)[i],"..."))
      tryCatch(gbif_data[[i]] <- rgbif::occ_search(taxonKey = key[i], limit = n.records, ...)$data,
               error = function(e) { gbif_data[[i]] <- data.frame(key = as.vector(key[i]),
                                                                  download = "failed") } )
    } else {gbif_data[[i]] <- data.frame(key = NA,
                                         download = "not_available")}
  }

  # Binding all data
  names(gbif_data) <- species
  all_data <- as.data.frame(dplyr::bind_rows(gbif_data, .id = "species_id"))

  # Filling w/ succeded when NA in download column (species not caught in tryCatch error)
  if ("download" %in% names(all_data)) {
    all_data[is.na(all_data$download)] <- "succeded"
  } else {
    all_data$download <- "succeded"
  }

  if ("failed" %in% all_data$download) {
    sp_failed <- unique(all_data$species_id[all_data$download %in% "failed"])
    warning(paste("Download of species", sp_failed, "failed!"))
  }

  if (remove_na) {
    all_data <- all_data[!is.na(all_data$decimalLongitude)
                         & !is.na(all_data$decimalLatitude), ]
  }

  if (save) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    # fullname <- paste0(dir, filename, ".csv")
    # message(paste0("Writing ", fullname, " on disk."))
    # write.table(all_data,
    #             fullname,
    #             sep = ",",
    #             row.names = FALSE,
    #             col.names = TRUE)

    if (file.format == "csv") {

      if (compress) {
        fullname <- paste0(dir, filename, ".csv.zip")
        message(paste0("Writing ", fullname, " on disk."))
        data.table::fwrite(all_data, file = fullname, compress = "gzip")

      } else {
        fullname <- paste0(dir, filename, ".csv")
        message(paste0("Writing ", fullname, " on disk."))
        data.table::fwrite(all_data, file = fullname)
      }
    }

    if (file.format == "rds") {

      fullname <- paste0(dir, filename, ".rds")
      message(paste0("Writing ", fullname, " on disk."))

      if (compress) {
        saveRDS(all_data, file = fullname, compress = "gzip")

      } else {
        saveRDS(all_data, file = fullname, compress = FALSE)
      }
    }
  }

  return(all_data)
}
