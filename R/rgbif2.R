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
#' @param remove_na Logical. Should records with NA in the columns
#'   decimalLatitude and decimalLongitude be removed? Default to FALSE.
#' @param save Logical. Save output to filename? Defaults to FALSE
#' @param file.format Character. The file extension to be used for saving ('csv'
#'   or 'rds'). Default to 'csv'.
#' @param compress Logical. Should the file be compressed? Default to FALSE.
#' @param ... Any argument from occ_search in rgbif package
#'
#' @return A data.frame with the search result. Columns species_id and download
#'   are added by the function. It can save the search output on disk
#'
#' @author Sara R. Mortara, Andrea Sánchez-Tapia & Renato A. F. de Lima
#'
#' @details Despite its name, the argument `species` can be used to declare
#' names at any taxonomic level (i.e. species, genus, family, order, etc.). But
#' not that for some names with homonyms, you may need to declare the authorship
#' as well so that GBIF can return the records for the right taxon key.
#'
#' By default, the function returns a maximum of 5000 records for each query.
#' This limit can be changed using the argument `n.records`. But note that the
#' maximum request imposed by the function `rgbif::occ_search()` is 100,000
#' records per taxon. This may be the case for the download of records for
#' entire families or orders and in this case only the first 100,000 records are
#' returned with a warning. So, please consider downloading records separately
#' for different regions, basis of records and/or higher taxonomic levels (i.e.
#' genera or species).
#'
#' The time of download will depend on the number of taxa selected and the
#' maximum number of records requested for download, as well as on the speed of
#' the internet connection. If you want to download large dataset, consider doing
#' your request using the GBIF web interface and load the DwC-A zip file using
#' the __plantR__ function `readData()`.
#'
#' __Important note__: __plantR__ currently does not provide tools to cite data
#' following the [GBIF citation guidelines]{https://www.gbif.org/citation-guidelines}.
#' So, it is necessary that the user identify the dataset publishers and
#' properly acknowledge each of them when citing the data. We recommend the
#' package __occCite__ (Owens et al. 2021) to help citing data. Another option
#' is to manually create a derived dataset and assign a unique DOI that can be
#' used to cite the dataset (check more instructions
#' [here]{https://www.gbif.org/citation-guidelines#derivedDatasets}).
#'
#' @references
#' Hannah L. Owens, Cory Merow, Brian Maitner, Jamie M. Kass, Vijay Barve and
#' Robert P. Guralnick (2021). occCite: Querying and Managing Large Biodiversity
#' Occurrence Datasets. R package version 0.4.6.
#' https://CRAN.R-project.org/package=occCite
#'
#' @examples
#' \dontrun{
#' ex_rgbif <- rgbif2(species =  "Paubrasilia echinata")
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
                   remove_na = FALSE,
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
  key_pointer <- sapply(key, function(x) !is.null(x))

  # Adding message if any species not found
  if (!all(key_pointer)) {
    message("\nTaxon not found: ", species[!key_pointer],
            "\nReturning data only for: ",
            paste(species[key_pointer], collapse = ", "), "\n")
  }
  # Loop for each valid species
  gbif_data <- list()
  for (i in 1:length(key_pointer)) {
    if (key_pointer[i]) {
      message(paste0("Making request to GBIF for ", names(key)[i],"..."))
      # tryCatch(gbif_data[[i]] <- rgbif::occ_search(taxonKey = key[i], limit = n.records, ...)$data,
      #          error = function(e) { gbif_data[[i]] <- data.frame(key = as.vector(key[i]),
      #                                                             download = "failed") } )
      gbif_data.i <- try(rgbif::occ_search(taxonKey = key[i],
                        limit = n.records, ...), TRUE)

      if (class(gbif_data.i) == "try-error") {
        if (grepl("Max offset of 100001 exceeded", gbif_data.i)) {
          gbif_data.i <- try(rgbif::occ_search(taxonKey = key[i],
                                               limit = 100000, ...), TRUE)
          aviso <- paste0("Max offset of 100001 exceeded. Downloading only the first 100000 records for ",
                         species[key_pointer])
          warning(aviso, call. = FALSE)
          gbif_data[[i]] <- gbif_data.i$data
        } else {
          gbif_data[[i]] <- data.frame(key = as.vector(key[i]),
                                       download = "failed")
        }
      } else {
        gbif_data[[i]] <- gbif_data.i$data
      }
    } else {
      gbif_data[[i]] <- data.frame(key = NA,
                                         download = "not_available")
    }
  }

  # Forcing numeric columns to be characters
  if (!is.null(gbif_data)) {
    for (i in 1:length(gbif_data)) {
      ids <- which(sapply(gbif_data[[i]], class) %in% c("numeric", "integer"))
      if (length(ids) > 0)
        for (j in ids) gbif_data[[i]][[j]] <- as.character(gbif_data[[i]][[j]])
    }
  }
  # Binding all data
  names(gbif_data) <- species
  all_data <- as.data.frame(dplyr::bind_rows(gbif_data, .id = "species_id"))

  # Filling w/ succeded when NA in download column (species not caught in tryCatch error)
  if ("download" %in% names(all_data)) {
    all_data$download[is.na(all_data$download)] <- "succeded"
  } else {
    all_data$download <- "succeded"
  }

  if ("failed" %in% all_data$download) {
    sp_failed <- unique(all_data$species_id[all_data$download %in% "failed"])
    warning(paste("Download of", sp_failed, "failed!"),
            call. = FALSE)
  }

  if (remove_na) {
    all_data <- all_data[!is.na(all_data$decimalLongitude)
                         & !is.na(all_data$decimalLatitude), ]
  }

  if (save) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)

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

  message("Please make sure that the restrictions and citation indicated by
  each GBIF data provider are observed and respected.")

  return(all_data)
}
