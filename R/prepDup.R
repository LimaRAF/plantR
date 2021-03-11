#' @title Prepare For Duplicate Specimen Search
#'
#' @description This function creates the duplicate search strings by
#'   concatenating the information on the taxonomy, collection and locality of
#'   the records.
#'
#' @param x a data frame with the species records.
#' @param col.names vector. A named vector containing the names of columns in the
#'   input data frame for each of the information that should be used to create
#'   the duplicate search string(s). Default to the __plantR__ output column
#'   names.
#' @param comb.fields list. A list containing one or more vectors with the
#'   information that should be used to create the duplicate search strings.
#'   Default to four vectors of information to be combined.
#' @param rec.ID character. The name of the columns containing the unique record
#' identifier (see function `getTombo()`). Default to 'numTombo'.
#' @param noYear character. Standard for missing data in Year. Default to
#'   "n.d.".
#' @param noName character. Standard for missing data in collector name. Default
#'   to "s.n.".
#' @param noNumb character. Standard for missing data in collector number. Default
#'   to "s.n.".
#' @param ignore.miss logical. Should the duplicate search strings with
#'   missing/unknown information (e.g. 'n.d.', 's.n.', NA) be excluded from the
#'   duplicate search. Default to TRUE.
#'
#' @author Renato A. F. de Lima
#'
#' @details Three groups of fields are available to produce the duplicate search
#'   string, and they are related to taxonomy, collection and locality of the
#'   specimen. These fields should be provided to the argument `col.names` and they are:
#'   - 'family': the botanical family (default: 'family.new')
#'   - 'species': the scientific name (default: 'scientificName.new')
#'   - 'col.name': the collector name (default: 'recordedBy.new')
#'   - 'col.last.name': the collector last name (default: 'last.name')
#'   - 'col.number': the collector serial number (default: 'recordNumber.new')
#'   - 'col.year': the collection year (default: 'year.new')
#'   - 'col.loc': the collection locality (default: 'municipality.new')
#'
#'   The corresponding columns that should be used to retrieve these fields in
#'   the input data frame must be provided as a named vector in the argument
#'   `col.names`, in which the fields listed above are the names and
#'   each element is the corresponding column name in the input data frame.
#'
#'   If an element named 'loc.str' containing the column name of the __plantR__
#'   locality string (i.e. 'loc.correct') is also provided, it can be used to
#'   complement any missing locality information in the locality of the
#'   collection (i.e 'col.loc') tha may have been retrieved in the data processing
#'   within the __plantR__ workflow.
#'
#'   The duplicate search strings are created by combining the fields listed
#'   above. Each combination of those fields (e.g. 'col.name' and 'col.number')
#'   should be provided to the argument `comb.fields` as a vector within a list.
#'   The number of strings to be generated will correspond to the number of vectors
#'   in this list. The order of the fields within vectors does not change the duplicate
#'   search process.
#'
#'   The argument `rec.ID` should indicate the column name in the input data
#'   containing the unique record identifier, which in the __plantR__ workflow
#'   is obtained using the function `getTombo()`. If only GBIF data is used,
#'   this column could be the field 'gbifID'. This identifier is used to
#'   indicate the groups of duplicated records, which is one of the outputs of
#'   function `getDup()` and is used to homogenize information within the groups
#'   of duplicates (function `mergeDup()`).
#'
#'   Please note that the retrieval of duplicates greatly depends on the
#'   completeness of the input information and in the amount of differences of
#'   notation standards among collections. In addition, the smaller the vectors
#'   of fields to be combined to create the duplicate strings, the higher the number
#'   of (true and false) duplicates will be retrieved.
#'
#' @seealso
#'  \link[plantR]{getTombo}, \link[plantR]{getDup} and \link[plantR]{mergeDup}.
#'
#' @importFrom stringr str_trim str_count
#'
#' @export prepDup
#'
prepDup <- function(x, col.names = c(family = "family.new",
                                     species = "scientificName.new",
                                     col.name = "recordedBy.new",
                                     col.last.name = "last.name",
                                     col.number = "recordNumber.new",
                                     col.year = "year.new",
                                     col.loc = "municipality.new",
                                     loc.str = "loc.correct"),
                    comb.fields = list(c("family","col.last.name","col.number","col.loc"),
                                       c("family","col.year","col.number","col.loc"),
                                       c("species","col.last.name","col.number","col.year"),
                                       c("col.year","col.last.name","col.number","col.loc")),
                    rec.ID = "numTombo", noYear = "s.d.", noName = "s.n.", noNumb = "s.n.", ignore.miss = TRUE) {

  ## check input
  if (!class(x)[1] == "data.frame")
    stop("Input object needs to be a data frame!")

  ## Checking the input columns
  #unique record identifier
  if (rec.ID %in% names(x)) {
    numTombo <- x[,rec.ID]
  } else {
    stop("Please provide a valid column name containing the unique record identifier")
  }

  #list of column names in the data
  id.cols <- col.names %in% names(x)

  #second try without the '.new' suffix
  if (any(!id.cols)) {
    col.names.sub <- gsub("\\.new", "", col.names)
    id.cols.sub <- col.names.sub %in% names(x)
    col.names[!id.cols & id.cols.sub] <-
      col.names.sub[!id.cols & id.cols.sub]
    id.cols[!id.cols & id.cols.sub] <- TRUE
  }

  if (all(!id.cols))
    stop("Please provide at least one of the necessary columns to build the search strings")

  #filtering only the columns and fields in the data
  cols <- col.names[id.cols]
  cols.miss <- col.names[!id.cols]

  flds <- unique(unlist(comb.fields))
  flds.miss <- flds[flds %in% names(cols.miss)]

  #warning and removing missing columns/fields
  if (length(cols.miss) > 0 & length(flds.miss) > 0) {
    warning(paste0("The columns name(s) of field(s) ", paste(flds.miss, collapse = " + "),
                   " was(were) not found in the input data frame and were removed"))
    comb.fields <- sapply(comb.fields,
                          function (x) x[!x %in% flds.miss])
  }

  ## Removing unwanted columns in the data
  x1 <- x[, match(cols, names(x))]
  names(x1) <- names(cols)[match(cols, names(x1))]

  ## Editing the filtered columns to avoid misleading duplicates
  if ("col.year" %in% names(x1)) { # collection year
    ids <- nchar(x1$col.year) > 4
    if (any(ids))
      x1$col.year[ids] <- as.character(sapply(strsplit(x1$col.year[ids], " |-|\\/", perl = TRUE),
                                              function(x) paste0(unique(x[nchar(x) >= 4]), collapse = "-")))
    x1$col.year <- suppressWarnings(as.double(x1$col.year))
    x1$col.year[is.na(x1$col.year)] <- noYear
  }

  if ("col.loc" %in% names(x1)) { # collection locality
    x1$col.loc <-
      tolower(rmLatin(x1$col.loc))
    x1$col.loc <- prepLoc(x1$col.loc)
  }

  if ("loc.str" %in% names(x1) & "col.loc" %in% names(x1)) { # locality string
    if (grepl("municipality", cols[names(cols) == "col.loc"]))
      pos <- 3
    if (grepl("locality", cols[names(cols) == "col.loc"]))
      pos <- 4
    tmp <- strsplit(x1$loc.str, "_", fixed = TRUE)
    ids <- suppressWarnings(sapply(tmp, length) >= pos)
    n2 <- rep(NA, dim(x1)[1])
    if (any(ids))
      n2[ids] <- sapply(tmp[ids], function(x) x[pos])
    x1$col.loc[is.na(x1$col.loc) & !is.na(n2)] <-
      as.character(n2[is.na(x1$col.loc) & !is.na(n2)])
  }

  if ("col.number" %in% names(x1)) { # Collector number
    x1$col.number[x1$col.number %in% c("", " ", NA)] <- noNumb
    x1$col.number[!grepl("\\d", x1$col.number, perl = TRUE)] <- noNumb
  }

  if ("col.name" %in% names(x1)) { # Collector name
    x1$col.name[x1$col.name %in% c("", " ", NA)] <- noName
    x1$col.name[nchar(x1$col.name) <= 3] <- noName
  }

  ## Creating the duplicate search strings
  srch.str <- vector("list", length(comb.fields))
  for (i in 1:length(srch.str)) {
    srch.str[[i]] <-
      as.character(apply(x1[,comb.fields[[i]]], 1, paste0, collapse = "_"))
  }

  #Should strings with missing/NA information be ignored?
  if (ignore.miss) {
    miss.patt <- paste0(c(noYear, noNumb, noName), collapse = '|')
    miss.patt <- gsub('\\.','\\\\.', miss.patt)
    for (i in 1:length(srch.str)) {
      srch.str[[i]][grepl(miss.patt,
                          srch.str[[i]], perl = TRUE, ignore.case=TRUE)] <- NA
      srch.str[[i]][grepl("^NA_|_NA_|_NA$",
                          srch.str[[i]], perl = TRUE, ignore.case=TRUE)] <- NA
    }
  }

  #Transforming the duplicate search strings into a data frame
  dup.srch.str <- do.call(cbind.data.frame, srch.str)
  names(dup.srch.str) <- paste0("dup.srch.str", 1:length(dup.srch.str))
  for(i in 1:length(srch.str))
    dup.srch.str[,i] <- as.character(dup.srch.str[,i])

  ##Saving the new info
  result <- cbind.data.frame(numTombo, dup.srch.str,
                         stringsAsFactors = FALSE)
  return(result)
}
