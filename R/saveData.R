#' @title Save Species Occurrence Data
#'
#' @description This function saves the occurrence data in any given directory,
#'   separated by species taxonomy, collections, years and countries, as well as
#'   by the confidence level of species identifications and coordinates.
#'
#' @param df a data frame with the occurrence data, generally as the output of
#'   the __plantR__ validation functions.
#' @param file.name character. Name of the file in which the data will be saved,
#'   without the extension. Default to "output".
#' @param dir.name character. Name of the folder where the data should be saved.
#'   Default to "plantR_output".
#' @param path character. The path to the directory of the output folder.
#'   Default to the user working directory.
#' @param by character. The variable used for separating the data into different
#'   files before saving.
#' @param file.format character. The file extension to be used for saving.
#'   Default to 'csv'.
#' @param compress logical. Should the files be compressed? Default to TRUE.
#' @param rm.dup logical. Should duplicated specimens be removed prior to
#'   saving? Default to FALSE.
#'
#' @details This function provides different option to save occurrence data. It
#'   provides options for saving data as 'csv' (function `fwrite` from package
#'   `data.table`) or 'rds' (base function `saveRDS`). The function tries to
#'   save data as fast as possible but processing time greatly depends or the
#'   size of the dataset. Options to compress data are available, but please be
#'   sure to have enough memory space for saving large datasets.
#'
#'   Currently, saving can be performed by grouping occurrence data by the
#'   following types of information:
#'   \itemize{
#'   \item code of the biological collection ('collection')
#'   \item year of collection ('year')
#'   \item taxonomy ('family', 'genus' or 'species')
#'   \item country of collection ('country')
#'   \item the confidence level of species identifications ('tax')
#'   \item the validation categories of the geographical coordinates ('geo')
#'   }
#'
#'   Note that if there are NAs in the grouping variable, they will be all saved
#'   under a file called 'NA.csv' or 'NA.rds'.
#'
#' @import data.table
#' @importFrom utils head
#'
#' @export saveData
#'
saveData <- function(df, file.name = "output", dir.name = "", path = "",
                     by = NULL, file.format = "csv",
                     compress = TRUE, rm.dup = FALSE) {

  # check input
  if (!inherits(df, "data.frame"))
    stop("Input object needs to be a data frame!")

  # Defining the general path for saving
  if (is.null(dir.name) | dir.name == "") {
    if (is.null(path) | path == "")
      path <- getwd()
    path <- file.path(path, "plantR_output")
  } else {
    if (is.null(path) | path == "")
      path <- getwd()
    path <- file.path(path, dir.name)
  }

  if (!file.exists(path))
    dir.create(path)

  ## PREPARING THE DATA ##
  # Select which co-variables will be used to separate the data
  if (!is.null(by)) {
    covs <- list(collection = c("collectionCode.new", "collectionCode"),
               year = c("year.new", "year"),
               family = c("family.new", "family"),
               genus = c("genus.new", "genus"),
               species = c("scientificName.new", "scientificName"),
               country = c("country.new", "country"),
               tax = c("tax.check1", "tax.check"),
               geo = c("geo.check1", "geo.check"))

    #Get only the columns of interest
    covs.present <- lapply(covs, function(z) utils::head(z[which(z %in% names(df))], n = 1))
    # covs.present <- lapply(covs, function(z) my.head(z[which(z %in% names(df))]))
    if (all(sapply(covs.present, nchar)==0))
      stop("The input data frame does not contain at least one of the required columns")

    group <- covs.present[[by]]
    if (is.null(group))
      stop("Please provide one of the following groups:\n 'collection', 'year', 'family', 'genus', 'species', 'country', 'tax' or 'geo'")
  }

  # Should the duplicates be removed?
  if (rm.dup) {
    if ("numTombo" %in% names(df)) {
      dt <- data.table::data.table(rmDup(df))
    } else {
      dt <- data.table::data.table(df)
      warning("Duplicated specimens cannot be removed; using the all occurrences instead")
    }
  } else {
    dt <- data.table::data.table(df)
  }

  ## SAVING THE DATA ##
  if (is.null(by)) {

    if (file.format == "csv") {

      if (compress) {
        file.name1 <- paste0(file.name, ".csv.zip")
        cat("Saving compressed occurrence data... ", sep = "")
        data.table::fwrite(data.frame(dt), file = file.path(path, file.name1),
                           compress = "gzip")
        cat("saved!", sep = "")
      } else {
        file.name1 <- paste0(file.name, ".csv")
        cat("Saving occurrence data... ", sep = "")
        data.table::fwrite(data.frame(dt), file = file.path(path, file.name1))
        cat("saved!", sep = "")
      }
    }

    if (file.format == "rds") {

      file.name1 <- paste0(file.name, ".rds")

      if (compress) {
        cat("Saving compressed occurrence data... ", sep = "")
        saveRDS(data.frame(dt), file = file.path(path, file.name1), compress = "gzip")
        cat("saved!\n", sep = "")
      } else {
        cat("Saving occurrence data... ", sep = "")
        saveRDS(data.frame(dt), file = file.path(path, file.name1), compress = FALSE)
        cat("saved!\n", sep = "")
      }
    }
  } else {

    groups <- sort(dt[, as.character(unique(.SD[[1L]])),
                      .SDcols = c(group)], na.last = TRUE)
    data.table::setkeyv(dt, group)

    if (file.format == "csv") {
      for (group.i in groups) {

        if (compress) {
          cat("Saving compressed data for group '",
              group.i, "'... ", sep = "")
          dt[group.i,
             data.table::fwrite(.SD, file.path(path, paste0(group.i, ".csv.zip")), compress = "gzip"), ]
          cat("saved!\n\n", sep = "")
        } else {
          cat("Saving data for group '",
              group.i, "'... ", sep = "")
          dt[group.i,
             data.table::fwrite(.SD, file.path(path, paste0(group.i, ".csv"))), ]
          cat("saved!\n\n", sep = "")
        }
      }
    }

    if (file.format == "rds") {
      for (group.i in groups) {

        if (compress) {
          cat("Saving compressed occurrence data for group '",
              group.i, "'... ", sep = "")
          saveRDS(data.frame(dt[group.i,]), file = file.path(path, paste0(group.i, ".rds")),
                  compress = "gzip")
          cat("saved!\n\n", sep = "")
        } else {
          cat("Saving occurrence data for group '",
              group.i, "'... ", sep = "")
          saveRDS(data.frame(dt[group.i,]), file = file.path(path, paste0(group.i, ".rds")),
                  compress = FALSE)
          cat("saved!\n\n", sep = "")
        }
      }
    }
  }
}
