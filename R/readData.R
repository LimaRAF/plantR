#' @title Read Data From DwC-A Files
#'
#' @description This function reads species records from Darwin Core Archive
#'   (DwC-A) files, typically obtained from GBIF as a '.zip' file. It returns
#'   different information from inside the DwC-A files so it can enter the
#'   __plantR__ workflow. Optionally, it can be used to save the required
#'   information into a local directory.
#'
#' @param file character. Name of the DwC-A file (often a '.zip') containing the
#'   species records.
#' @param path character. The path to the directory where the file was saved or
#'   of the web Uniform Resource Locator (URL) from which the file can be
#'   downloaded from. Default to the user working directory or to gbif download
#'   path.
#' @param dir.name character. Name of the folder where the processed data should
#'  be saved. Default the directory defined by `path`.
#' @param dir.tmp character. Name of the sub-folder where the temporary files
#'  should be saved within `dir.name`. Default to "plantR_input".
#' @param method the method to be passed to function `download.file()` for
#'   downloading files. Default to 'auto'.
#' @param bind.data logical. Should the occurrence and verbatim information be
#'   combined into a single table? Default to TRUE.
#' @param output character. Which information from the Darwin-Core file should
#'   be returned/saved? Default to 'occurrence', 'verbatim' and 'citations'.
#' @param save logical. Should the information be saved to file? Default to
#'   FALSE.
#' @param file.format character. The file extension to be used for saving.
#'   Default to 'csv'.
#' @param compress logical. Should the files be compressed? Default to TRUE.
#'
#' @details This function provides different options to read DwC-A files,
#'   typically the ones obtained from GBIF. Currently, this zip file can be read
#'   from a local directory or directly from GBIF API address. If the path is an
#'   URL address (e.g. https://api.gbif.org/v1/occurrence/download/request),
#'   then the function will download the zip file directly from the GBIF API.
#'
#'   The argument `output` defines which of the information within GBIF DwC-A
#'   files should be returned. Currently, the outputs available are:
#'   'occurrence', 'verbatim', 'multimedia', 'citations' and 'rights'. If more
#'   than one output is selected, the function returns a list in which each
#'   element represent the selected outputs. Currently, no data and database
#'   metadata are returned (i.e. '.xml' files). See package __finch__ for the
#'   complete parsing of DwC-A files and metadata.
#'
#'   All temporary files and folders are deleted after the extraction of the
#'   information, except if `save` is TRUE. In this case, only the unzipped
#'   files within the DwC-A file are removed.
#'
#'   Downloading large files (more than 2GB) may be an issue for some R
#'   versions. The `method` 'wget' may be more appropriate for users with proxy
#'   firewalls (see help of function `download.file()`).
#'
#' @import data.table
#' @importFrom utils download.file unzip
#'
#' @examples
#' \dontrun{
#'   occs <- readData(file = "0227351-200613084148143.zip",
#'                    path <- "https://api.gbif.org/v1/occurrence/download/request/")
#' }
#'
#' @export readData
#'
readData <- function(file = NULL, path = "", dir.name = "",
                     dir.tmp = "plantR_input",
                     method = "auto", bind.data = TRUE,
                     output = c("occurrence", "verbatim", "citations"), save = FALSE,
                     file.format = "csv", compress = TRUE) {

  # Check input
  if (is.null(file))
    stop("Please provide the file name")

  file_sep <- .Platform$file.sep

  if (is.null(dir.name) | dir.name == "") {
    if (is.null(path) | path == "") {
      dir.name <- paste0(file.path(getwd(), dir.tmp), file_sep)
      if (!file.exists(dir.name))
        dir.create(dir.name)
    } else {
      if (grepl("http", path)) {
        dir.name <- paste0(file.path(getwd(), dir.tmp), file_sep)
        if (!file.exists(dir.name))
          dir.create(dir.name)
      } else {
        # dir.name <- path
        dir.name <- paste0(file.path(path, dir.tmp), file_sep)
        if (!file.exists(dir.name))
          dir.create(dir.name)
      }
    }
  }

  # Defining the general path for reading
  if (is.null(path) | path == "") {
    path <- file.path(getwd(), file)
  } else {
    if (grepl("http", path)) {
      if (grepl("/$", path)) {
        path <- paste0(path, file)
      } else {
        path <- paste(path, file, sep = "/")
      }
    } else {
      path <- file.path(path, file)
      if (!file.exists(path))
        stop("The file name provided could not be found in the directory provided")
    }
  }

  ## DOWNLOADING THE DATA DIRECTLY FROM GBIF
  if (grepl("http", path)) {
    destfile.dwca <- file.path(dir.name, file)
    if (file.exists(destfile.dwca)) {
      warning("A zip file with the same name already exists in 'dir.name' and was not downloaded")
    } else {
      dwca <- utils::download.file(path, destfile = destfile.dwca,
                                   method = method, cacheOK = TRUE)
    }
  } else {
    destfile.dwca <- path
  }

  ## UNZIPPING THE DATA IN THE LOCAL DIRECTORY ##
  dir.name.unzip <- file.path(dir.name, gsub(".zip", "", file, fixed = TRUE))
  if (file.exists(dir.name.unzip)) {
    if (length(list.files(dir.name.unzip)) == 0) {
      dir.create(dir.name.unzip)
      zip.files <- utils::unzip(zipfile = destfile.dwca, list = TRUE)$Name
      zip.files <- zip.files[grepl(paste0(output,".txt", collapse = "|"), zip.files)]
      cat("Unzipping the DwC-A file... ", sep = "")
      dwca.unzip <- utils::unzip(zipfile = destfile.dwca,
                                 exdir = dir.name.unzip,
                                 files = zip.files)
      cat("done!\n", sep = "")
    } else {
      warning("An unzipped, non-empty folder for this file already exists and was not overwritten")
    }
  } else {
    dir.create(dir.name.unzip)
    zip.files <- utils::unzip(zipfile = destfile.dwca, list = TRUE)$Name
    zip.files <- zip.files[grepl(paste0(output,".txt", collapse = "|"), zip.files)]
    cat("Unzipping the DwC-A file... ", sep = "")
    dwca.unzip <- utils::unzip(zipfile = destfile.dwca,
                               exdir = dir.name.unzip,
                               files = zip.files)
    cat("done!\n", sep = "")
  }

  ## READING THE UNZIPPED FILES FROM LOCAL DIRECTORY ##
  all.files <- list.files(dir.name.unzip, full.names = TRUE)
  occurrence <- verbatim <- multimedia <- citations <- rights <- metadata <- NULL

  ## Reading the txt files
  if ("occurrence" %in% output) {
    occ.path <- all.files[grepl("occurrence.txt", all.files)]
    occ.data <- data.table::fread(occ.path, na.strings=c("NA"))

    verb.path <- all.files[grepl("verbatim.txt", all.files)]
    verb.data <- data.table::fread(verb.path, na.strings=c("NA"))
  }

  if ("multimedia" %in% output) {
    mult.path <- all.files[grepl("multimedia.txt", all.files)]
    mult.data <- data.table::fread(mult.path, na.strings=c("NA"))
  }

  if ("citations" %in% output) {
    cite.path <- all.files[grepl("citations.txt", all.files)]
    citations <- scan(cite.path, what = "character", sep = "\n",
                      fileEncoding = "UTF-8", quiet = TRUE)[-1]
    citations <- data.frame(citations = citations, stringsAsFactors = FALSE)
    cat(scan(cite.path, what = "character", sep = "\n",
                 fileEncoding = "UTF-8", quiet = TRUE)[1], "\n")
  }

  if ("rights" %in% output) {
    right.path <- all.files[grepl("rights.txt", all.files)]
    rights0 <- scan(right.path, what = "character", sep = "\n",
                    fileEncoding = "UTF-8", quiet = TRUE)
    rights <- cbind.data.frame(Dataset = rights0[grepl("Dataset", rights0, fixed = TRUE)],
                               Rights = rights0[grepl("Rights as supplied", rights0, fixed = TRUE)])
    rights$Dataset <- gsub("^Dataset\\: ", "", rights$Dataset, perl = TRUE)
    rights$Rights <- gsub("^Rights as supplied\\: ", "", rights$Rights, perl = TRUE)
  }

  ## Reading the xml files (currently not implemented)
  # if ("metadata" %in% output) {
  #   meta.path <- all.files[grepl("meta.xml", all.files)]
  #   meta <- EML:::read_eml(meta.path)
  #
  #   metadata.path <- all.files[grepl("metadata.xml", all.files)]
  #   metadata <- EML:::read_eml(metadata.path)
  #
  #   dataset.paths <- list.files(
  #     all.files[grepl("dataset$", all.files, perl = TRUE)], full.names = TRUE)
  #   datasets <- lapply(dataset.paths, EML::read_eml)
  # }

  ## MERGING OCCURRENCE AND VERBATIM DATA ##
  if ("occurrence" %in% output) {
    if (bind.data) {
    cols <- !names(verb.data) %in% names(occ.data)
    verb.data <- verb.data[, cols, with = FALSE]
    occurrence <- cbind(occ.data, verb.data)
    output <- output[!grepl("verbatim", output)]
    } else {
      occurrence <- occ.data
      verbatim <- verb.data
    }
  }

  ## PUTTING THE INFORMATION TOGETHER
  all.data <- list(occurrence = occurrence, verbatim = verbatim,
                   multimedia = multimedia, citations = citations, rights = rights)
  all.data <- all.data[!sapply(all.data, is.null)]

  ## SAVING THE SELECTED INFORMATION IN THE LOCAL FILE
  if (save) {
    if (file.format == "csv") {
      if (compress) {
        cat("Saving compressed occurrence data... ", sep = "")
        for (i in 1:length(all.data)) {
          df <- as.data.frame(all.data[[i]])
          nome <- names(all.data)[i]
          save.path <- file.path(dir.name.unzip, paste0(nome, ".csv.zip"))
          data.table::fwrite(df, file = save.path, compress = "gzip")
        }
        cat("saved!", sep = "")
      } else {
        cat("Saving occurrence data... ", sep = "")
        for (i in 1:length(all.data)) {
          df <- as.data.frame(all.data[[i]])
          nome <- names(all.data)[i]
          save.path <- file.path(dir.name.unzip, paste0(nome, ".csv"))
          data.table::fwrite(df, file = save.path)
        }
        cat("saved!", sep = "")
      }
    }

    if (file.format == "rds") {
      if (compress) {
        cat("Saving compressed occurrence data... ", sep = "")
        for (i in 1:length(all.data)) {
          df <- as.data.frame(all.data[[i]])
          nome <- names(all.data)[i]
          save.path <- file.path(dir.name.unzip, paste0(nome, ".rds"))
          saveRDS(df, file = save.path, compress = "gzip")
        }
        cat("saved!\n", sep = "")
      } else {
        cat("Saving occurrence data... ", sep = "")
        for (i in 1:length(all.data)) {
          df <- as.data.frame(all.data[[i]])
          nome <- names(all.data)[i]
          save.path <- file.path(dir.name.unzip, paste0(nome, ".rds"))
          saveRDS(df, file = save.path, compress = FALSE)
        }
        cat("saved!\n", sep = "")
      }
    }
  }

  ## REMOVING THE DOWNLOADED/UNZIPPED FILES
  if (grepl("http", path))
    file.remove(file.path(dir.name, file))

  file.remove(all.files[grepl("txt$|xml$", all.files)])

  if (length(list.files(dir.name.unzip)) == 0)
    unlink(dir.name.unzip, recursive = TRUE)

  if (length(list.files(dir.name)) == 0) {
    dir.name0 <- gsub(paste0(file_sep,"$"),"", dir.name)
    unlink(dir.name0, recursive = TRUE)
  }

  ## RETURNING THE SELECTED OUTPUT
  selected.data <- all.data[output]
  ids.dt <- sapply(selected.data,
                   function (x) identical(class(x), c("data.table","data.frame")))
  if (any(ids.dt))
    for(i in which(ids.dt == TRUE))
      selected.data[[i]] <- as.data.frame(selected.data[[i]])

  if (length(output) > 1)
    return(selected.data)
  if (length(output) == 1)
    return(selected.data[[output]])
}
