#' @title Prepare For Duplicate Specimen Search
#'
#' @description This function ...
#'
#' @param x a data frame with the occurrence data.
#' @param noYear Character. Standard for missing data in Year. Default to
#'   "n.d.".
#' @param noName character. Standard for missing data in collector name. Default
#'   to "s.n.".
#' @param noNumb character. Standard for missing data in collector number. Default
#'   to "s.n.".
#' @param combo.fields list. A list containing a vector with the occurrence
#'   information that should be used to construct the duplicate search strings.
#'   Default to TRUE. See Details.
#' @param ignore.miss logical. Should the duplicate search strings with
#'   missing data (e.g. 'n.d.', 's.n.') be excluded from the duplicate search.
#'   Default to TRUE.
#' @param ignore.na character. Should the duplicate search strings with
#'   missing data (e.g. 'n.d.', 's.n.') be excluded from the duplicate search.
#'   Default to TRUE.
#'
#' @author Renato A. F. de Lima
#'
#' @details The input data frame \code{x} must contain at least the columns ...
#'
#' Three groups of fields are available to produce the duplicate search string,
#' and they are related to taxonomy, collection and locality of the specimen.
#' These fields are: 'family', 'species', 'col.name', 'col.last.name',
#' 'col.number', 'col.year', 'municipality' or 'locality'. Each combination of
#' those fields (e.g. 'col.name' and 'col.number') should be provided as a vector and
#' as a separate element of this list. See examples.
#'
#' @importFrom stringr str_trim str_count
#'
#' @export prepDup
#'
prepDup <- function(x,
                    noYear = "s.d.",
                    noName = "s.n.",
                    noNumb = "s.n.",
                    comb.fields = list(c("family","col.last.name","col.number","municipality"),
                                       c("family","col.year","col.number","municipality"),
                                       c("species","col.last.name","col.number","col.year"),
                                       c("col.year","col.last.name","col.number","municipality")),
                    ignore.miss = TRUE,
                    ignore.na = TRUE) {

  ##Removing unwanted columns
  #List of colum names
  cols <- c("collectionCode","catalogNumber",
            "family","family.new","scientificName","scientificName.new",
            "recordedBy","recordedBy.new","last.name",
            "recordNumber","recordNumber.new",
            "loc.correct","municipality","municipality.new","locality","locality.new",
            "year","year.new",
            "dateIdentified","yearIdentified","yearIdentified.new")
  cls <-  unique(cols[cols %in% names(x)])
  #Priotirizing the .new columns, if original columns are also present
  cls1 <- cls[!duplicated(gsub(".new", "", cls), fromLast = TRUE)]
  x1 <- x[, cls1]

  ##Renaming columns for processing
  names(x1)[1:2] <- c("colecao", "tombo")
  names(x1)[(grep("loc.correct", names(x1)) + 1)] <- "localidade"
  if (any(grepl("dateIdentified|yearIdentified", names(x1)))) {
    if (names(x1)[dim(x1)[2]] == "dateIdentified") {
      names(x1)[(dim(x1)[2] - 1)] <- "ano"
      names(x1)[(dim(x1)[2])] <- "ano.det"
    } else {
      names(x1)[(dim(x1)[2] - 1)] <- "ano"
      names(x1)[(dim(x1)[2])] <- "ano.det"
    }
  } else {
    names(x1)[(dim(x1)[2])] <- "ano"
    x1$ano.det <- NA
  }
  #Adding the string to put the data back on its original order
  x1$order <- 1:dim(x1)[1]

  ##Editing the county column
  city <- tolower(textclean::replace_non_ascii(x1$localidade))
  city <- prepLoc(city)

  ##Defining the new county column
  tmp <- strsplit(x1$loc.correct, "_")
  id <- sapply(tmp, length)
  n2 <- rep(NA, dim(x1)[1])
  if (any(id >= 3))
    n2[id >= 3] <-
    sapply(tmp[id >= 3], function(x) x[3])
  x1$municipio <- n2
  x1$municipio[is.na(x1$municipio) & !is.na(city)] <-
    as.character(city[is.na(x1$municipio) & !is.na(city)])

  ##Creating the collection unique identifier
  col <- tolower(textclean::replace_non_ascii(x1$colecao))
  tmb <- tolower(textclean::replace_non_ascii(x1$tombo))
  #Editing acessions with characters other than numbers
  tmp1 <- tmb[grepl('\\D', tmb)]
  if (length(tmp1) > 0) {
    #no number? then NA
    tmp1[!grepl('\\d', tmp1)] <- NA
    #removing separators and exclamation points
    tmp1 <- gsub('-|\\!', '', tmp1)
    #removing the name of the collection and any other letters from the beggining of the accession number
    #sometimes accessions have a letter in front of the number

    ### ADD USER PROVIDED LIST O COLLECTION NAMES TO REMOVE? ###
    ### OR A HARD, REMOVE ALL LETTERS ARGUMENT? ###
    pos <- sapply(tmp1, function (x) gregexpr(pattern = '\\d', x)[[1]][1])
    if (any(!is.na(pos))) {
      pos1 <- pos[!is.na(pos) & pos > 1]
      tmp2 <- tmp1[!is.na(pos) & pos > 1]
      #List of collections within the data
      clc <- paste(paste("^", sort(unique(col)), sep = ""), collapse = "|")
      tmp2[grepl(clc, tmp2)] <- stringr::str_trim(gsub(clc, '', tmp2[grepl(clc, tmp2)]))
      tmp2[grepl(pattern = '\\d', tmp2) &
             !grepl(pattern = '\\D', tmp2)] = as.numeric(tmp2[grepl(pattern = '\\d', tmp2) &
                                                                !grepl(pattern = '\\D', tmp2)])
      tmp2[grepl('[a-z]',tmp2,ignore.case=TRUE)]
      tmp1[!is.na(pos) & pos > 1] = tmp2
    }

    #Removing zeros from the begining of the numerical string
    pos <- sapply(tmp1, function (x)
      gregexpr(pattern = '\\D', x)[[1]][1])
    pos[is.na(pos)] <- 0
    if (any(!is.na(pos)) & any(pos >= 1)) {
      pos1 <- pos[!is.na(pos) & pos >= 1]
      tmp2 <- tmp1[!is.na(pos) & pos >= 1]
      tmp3 <- sapply(1:length(tmp2), function(x) {
        inicio1 <- inicio2 <- pos1[x]
        if (pos1[x] == 1) inicio1 <- 1 else inicio1 <- pos1[x] - 1
        if (pos1[x] == 1) inicio2 <- pos1[x] + 1 else inicio2 <- pos1[x]
        parte1 <- substring(tmp2[x], 1, inicio1)
        if (!grepl(pattern = '\\D', parte1))
          parte1 <- as.numeric(parte1)
        parte2 <- substring(tmp2[x], inicio2)
        if (!grepl(pattern = '\\D', parte2))
          parte2 <- as.numeric(parte2)
        result <- paste(stringr::str_trim(parte1), stringr::str_trim(parte2), sep = "")
        return(result)
      })
      tmp1[!is.na(pos) & pos >= 1] <- tmp3
    }
    tmb[grepl('\\D', tmb)] <- tmp1
  }

  #Edting acessions only with numbers
  tmb[!grepl('\\D', tmb)] <- as.numeric(tmb[!grepl('\\D', tmb)])

  #Remove GBIF data without accession only with a gbifID?? Yes!
  tmb[!is.na(tmb) & stringr::str_count(tmb, "-") >= 3] <- NA
  #Creating the final/edited string of the collection+accession number
  x1$numTombo <- paste(col, tmb, sep = "_")

  ##Creating the collection year field
  if (any(grepl("ano.det", names(x1)))) {
    tmp <- x1$ano.det
    tmp[is.na(tmp) | tmp %in% ""] <- noYear
    tmp <- gsub("T00:00:00Z", noYear, tmp)
    x1$ano.det <- tmp
  }


  ##Final edits to avoid misleading duplicates
  #Collector number
  numb <- x1[,which(grepl("recordNumber", names(x1)))]
  numb[grepl("c\\(\\\"", numb)] <-
    gsub("c\\(\\\"", "", numb[grepl("c\\(\\\"", numb)]) # check if this is still necessary
  numb[grepl("\\\",-\\\"", numb)] <-
    gsub("\\\",-\\\"", "-", numb[grepl("\\\",-\\\"", numb)]) # check if this is still necessary
  numb[grepl("\\\"\\)", numb)] <-
    gsub("\\\"\\)", "", numb[grepl("\\\"\\)", numb)]) # check if this is still necessary
  numb[is.na(numb) | numb %in% ""] <- noNumb
  x1$numb <- numb

  #Collector last name
  nome <- x1[,which(grepl("last.name", names(x1)))]
  nome[!is.na(nome) & nchar(nome) < 2] <-
    x1[,tail(which(grepl("recordedBy", names(x1))),1)][!is.na(nome) & nchar(nome) < 2]
  nome[is.na(nome) | nome %in% ""] <- noName
  x1$nome <- nome

  #Collection year
  tmp <- x1$ano
  tmp <- as.character(sapply(strsplit(tmp, " "), function(x)
    unique(x[nchar(x) >= 4])))
  tmp[is.na(tmp) | tmp %in% ""] <- noYear
  x1$ano <- suppressWarnings(as.double(tmp))

  ## Creating the specimens identifier for each of the options defined in 'combos'
  #Converting the fields chosen to column names
  comb.fields <- lapply(comb.fields, function(x)
    gsub("family", names(x1)[grepl("family",names(x1))], x))
  comb.fields <- lapply(comb.fields, function(x)
    gsub("species", names(x1)[grepl("scientificName",names(x1))], x))
  comb.fields <- lapply(comb.fields, function(x)
    gsub("col.name", names(x1)[grepl("recordedBy",names(x1))], x))
  comb.fields <- lapply(comb.fields, function(x)
    gsub("col.last.name", "nome", x))
  comb.fields <- lapply(comb.fields, function(x)
    gsub("col.number", "numb", x))
  comb.fields <- lapply(comb.fields, function(x)
    gsub("col.year", "ano", x))
  comb.fields <- lapply(comb.fields, function(x)
    gsub("municipality", "municipio", x))
  comb.fields <- lapply(comb.fields, function(x)
    gsub("locality", "localidade", x))
  #Making sure that all columns are indeed in the data
  comb.fields <- lapply(comb.fields, function(x)
    x[x %in% names(x1)])

  #Creating the duplicate search strings
  srch.str <- vector("list", length(comb.fields))
  for (i in 1:length(srch.str)) {
    srch.str[[i]] <-
      as.character(apply(x1[,comb.fields[[i]]], 1, paste0, collapse = "_"))
  }

  #Should strings with missing/NA information be ignored?
  miss.vec <- paste0(c(noYear, noNumb, noName), collapse = '|')
  miss.vec <- gsub('\\.','\\\\.',miss.vec)

  if (ignore.miss) {
    for (i in 1:length(srch.str)) {
      srch.str[[i]][grepl(miss.vec, srch.str[[i]], ignore.case=TRUE)] <- NA
    }
  }
  if (ignore.na) {
    for (i in 1:length(srch.str)) {
      srch.str[[i]][grepl("^NA_|_NA_|_NANA_|_NA$", srch.str[[i]], ignore.case=TRUE)] <- NA
    }
  }

  #Transforming the duplicate search strings into a data frame
  dup.srch.str <- do.call(cbind.data.frame, srch.str)
  names(dup.srch.str) <- paste0("dup.srch.str", 1:length(dup.srch.str))
  for(i in 1:length(srch.str))
    dup.srch.str[,i] <- as.character(dup.srch.str[,i])

  ##Saving the new info
  x2 <- cbind.data.frame(x1, dup.srch.str,
                         stringsAsFactors = FALSE)
  x2 <- x2[order(x2$order),]
  # out.cols <- c(c("numTombo", "ano", "ano.det", "municipio"), names(dup.srch.str))
  out.cols <- c(c("numTombo"), names(dup.srch.str))
  result <- x2[, out.cols]

  return(result)
}
