#' @title Create Record Identifiers
#'
#' @description This function creates a unique identifier for species records by
#'   concatenating the collection code and accession number of each record
#'
#' @param collection a vector with the collection codes.
#' @param accession a vector with the accession numbers.
#' @param by.coll logical. Should the removal of collection codes at the
#'   beginning of the accession numbers be removed based on the corresponding
#'   collection code provided in `collection`? Defaults to TRUE.
#' @param to.lower logical. Should the final unique identifier be converted to
#'   lower cases? Default to FALSE.
#'
#' @return the collection codes and the edited accession numbers concatenated
#'   using an underline mark (i.e '_').
#'
#' @details The function performs small edits in the accession number (the
#'   Darwin Core field 'catalogNumber'), such as the removal of spaces, hyphens,
#'   points and non-ascii characters. The function also remove all zeros at the
#'   beginning of the number, to simplify the notation.
#'
#'  Accession numbers may contain the collection code at their beginning. The
#'   function can remove this codes in two ways. The default way (`by.coll` = TRUE)
#'   removes only the codes matching the corresponding code provided in
#'   `collection`. The second way removes all letters in the beginning of the number
#'   irrespectively of the collection code (`by.coll` = FALSE).
#'
#'  It only edits the accession number, assuming that the collection code (the
#'  Darwin Core field 'collectionCode') has already been standardized using
#'  another __plantR__ function, `getCode()`. The final identifier can be
#'  returned as is (the default) or in lower cases by setting the argument
#'  `to.lower` to TRUE.
#'
#' @seealso
#'  \link[plantR]{getCode}
#'
#' @author Renato A. F. de Lima
#'
#' @importFrom stringr str_count
#'
#' @examples
#' colls <- c("P", "P", "NY", "G", "P", "P", "NY", "G",
#' "P", "P", "NY", "G", NA, NA)
#' access <- c("1427", "P001427", "NY1427", "G-G-1427/1",
#' "1427A", "1427 a", "1427-A", "U.1427", "P1427a", ""," ", NA, "1427", NA)
#'
#' getTombo(colls, access)
#'
#' @export getTombo
#'
getTombo <- function(collection = NULL, accession = NULL, by.coll = TRUE, to.lower = FALSE) {

  ## check input
  if (is.null(collection) | is.null(accession))
    stop("Please provide at least one collection and one accession")

  if (length(collection) != length(accession))
    stop("Collection codes and accession numbers must have the same size")

  ## Detecting NAs
  collection[collection %in% c("", " ", NA)] <- NA_character_
  accession[accession %in% c("", " ", NA)] <- NA_character_
  nas.col <- is.na(collection)
  nas.acc <- is.na(accession)

  ## Cleanning non-ascii, no NAs
  col <- rmLatin(collection[!nas.acc])
  tmb <- rmLatin(accession[!nas.acc])

  #Editing accessions number with characters other than numbers
  ids <- grepl('\\D', tmb, perl = TRUE)
  tmb1 <- tmb[ids]

  if (length(tmb1) > 0) {

    #list of collection codes for the subset
    col1 <- col[ids]

    #no numbers? then NA
    tmb1[!grepl('\\d', tmb1)] <- NA

    #removing separators and exclamation points
    tmb1 <- gsub('-|\\!', '', tmb1, perl = TRUE)

    #removing spaces
    tmb1 <- gsub(' ', '', tmb1, fixed = TRUE)

    #removing points between collections and accession
    tmb1 <- gsub('([a-z])\\.([0-9])', '\\1\\2', tmb1, perl = TRUE)

    #removing letters from the beggining of the accession number
    #sometimes accessions have a collection code in front of the accession
    if (!by.coll) {
      #Removing all non-numbers from the beggining of the accession
      pos <- as.double(regexpr('\\d', tmb1, perl = TRUE))
      pos[is.na(pos)] <- 0
      if (any(pos > 1))
        tmb1[pos > 1] <- substring(tmb1[pos > 1], pos[pos > 1])

      #Removing zeros from the beggining of the number
      pos <- as.double(regexpr('[1-9]', tmb1, perl = TRUE))
      pos[is.na(pos)] <- 0
      if (any(pos > 1))
        tmb1[pos > 1] <- substring(tmb1[pos > 1], pos[pos > 1])

      #Writing the result
      tmb[ids] <- tmb1

    } else {
      ## Removing only the collection codes from the beggining of the accession
      #Detecting the letters before numbers
      pos <- as.double(regexpr('\\d', tmb1, perl = TRUE))
      pos[is.na(pos)] <- 0
      tmb2 <- tmb1[pos > 1]
      #List of collections within the data
      col2 <- paste0("^", col1[pos > 1],"+")

      #Removing the collections codes
      tmb2 <- mapply(function(x, y) { gsub(y, "", x, perl = TRUE) },
                     tmb2, col2)

      #Converting the all-number strings to numeric
      tmb2[grepl(pattern = '\\d', tmb2, perl = TRUE) &
             !grepl(pattern = '\\D', tmb2, perl = TRUE)] <- as.numeric(tmb2[grepl(pattern = '\\d', tmb2, perl = TRUE) &
                                                                              !grepl(pattern = '\\D', tmb2, perl = TRUE)])
      tmb1[pos > 1] <- tmb2

      ## Removing zeros from the begining of the numerical string
      #Case 1: with letter in the beggining
      pos <- as.double(regexpr('\\d', tmb1, perl = TRUE))
      pos[is.na(pos)] <- 0
      if (any(pos > 1))
        tmb1[pos > 1] <- gsub('([a-z])0+([1-9])', '\\1\\2', tmb1[pos > 1], perl = TRUE)

      #Case 2: with letter somewhere else and a zero in the beggining
      pos <- as.double(regexpr('^0', tmb1, perl = TRUE))
      pos[is.na(pos)] <- 0
      if (any(pos == 1))
        tmb1[pos == 1] <- gsub('^(0+)([1-9])', '\\2', tmb1[pos == 1], perl = TRUE)

      #Writing the result
      tmb[ids] <- tmb1
    }
  }

  #Editing acessions only with numbers
  tmb[!grepl('\\D', tmb, perl = TRUE)] <-
    as.numeric(tmb[!grepl('\\D', tmb, perl = TRUE)])

  #Remove GBIF data without accession and only with a datasetKey
  tmb[!is.na(tmb) & stringr::str_count(tmb, "-") >= 3] <- NA

  #Creating the final/edited string of the collection+accession number
  numTombo <- paste(col, tmb, sep = "_")
  numTombo1 <- paste(collection[nas.acc & !nas.col],
                     accession[nas.acc & !nas.col], sep = "_")

  if (to.lower) {
    ids <- grepl("^NA_", numTombo, perl = TRUE)
    numTombo[!ids] <- tolower(numTombo[!ids])
    numTombo[ids] <-
      paste0("NA_", tolower(gsub("NA_", "", numTombo[ids], fixed = TRUE)))
    numTombo1 <-
      paste0(tolower(gsub("_NA", "", numTombo1, fixed = TRUE)), "_NA")
  }

  #Preparing to return
  result <- rep(NA_character_, length(collection))
  result[!nas.acc] <- numTombo
  result[nas.acc & !nas.col] <- numTombo1
  return(result)
}
