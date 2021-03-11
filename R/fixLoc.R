#' @title Format Locality Information
#'
#' @description Standardize the notation of the locality fields country,
#'   stateProvince, municipality and locality, and search from some missing
#'   information within the available locality information.
#'
#' @param x a data frame containing typical locality fields from species records.
#' @param loc.levels a vector containing the names of the locality fields to be
#'   formatted.
#' @param scrap logical. Should the search of missing locality information be
#'   performed? Default to TRUE.
#' @param to.lower logical. Should the output locality names be return in
#'   lower cases? Default to TRUE.
#'
#' @return The input data frame \code{x}, plus the '.new' columns with the formatted
#'   fields and the resolution of the locality information available.
#'
#' @details The function performs several edits and replacements. Country names
#'   are formated into the international format, letters are lower-cased, and
#'   special characters and common abbreviations are removed.
#'
#'   By default, this function formats all four locality fields simultaneously
#'   (i.e. country, stateProvince, municipality, locality), but the user can
#'   choose among these fields through the argument `loc.levels`. However, the
#'   process of searching for missing information is more complete if all the
#'   four locality fields mentioned above are available.
#'
#'   If present, other Darwin Core fields are used internally to obtain missing
#'   information on the locality fields declared above, namely: 'countryCode',
#'   'county', and 'verbatimLocality'.
#'
#'   The argument `scrap` controls the search for missing municipality
#'   information from the field 'locality'. It also performs some extra editing
#'   and cropping of the field 'locality' in order to obtain more standardized
#'   locality descriptions. This argument uses different ways of splitting and
#'   cropping the locality description in order to find missing information.
#'   Although it does not always result in an accurate extraction of the
#'   information, it provides an extra tool to organize locality information
#'   which are not provided in the appropriate columns.
#'
#'   The function automatically returns the original resolution of the
#'   locality information provided. For instance, if only country information is
#'   provided (i.e. field is not empty), then the resolution is flagged as
#'   'country'; if country and stateProvince are given, then the resolution is
#'   flagged as 'stateProvince', and so on.
#'
#'
#' @author Renato A. F. de Lima
#'
#' @importFrom stringr str_trim str_replace_all
#' @importFrom countrycode countrycode
#'
#' @export fixLoc
#'
#' @examples
#' # Creating a data frame with locality information
#' (df <- data.frame(country = c("BR", "Brasil", "BRA", "Brazil", NA),
#' stateProvince = c("MG", "estado de Minas Gerais", "Minas Geraes",
#' "Minas Gerais", "Minas Gerais"),
#' municipality = c("Lavras", "lavras", NA, NA, "Lavras"),
#' locality = c(NA, "UFLA", "municipio de Lavras, campus UFLA",
#' "Minas Gerais, municipio Lavras", NA)))
#'
#' # Formating the locality information
#' fixLoc(df, scrap = FALSE)
#' fixLoc(df, scrap = FALSE, to.lower = FALSE)
#'
#' # Formating and scrapping the locality information
#' fixLoc(df, scrap = TRUE)
#'
#' # Formating the locality information only at country and state levels
#' fixLoc(df, loc.levels = c("country", "stateProvince"))[,-c(1:4)]
#'
fixLoc <- function(x,
                   loc.levels = c("country", "stateProvince", "municipality", "locality"),
                   scrap = TRUE, to.lower = TRUE) {

  ## checking input:
  if (!class(x) == "data.frame")
    stop("input object needs to be a data frame!")

  # Missing country that may be stored in the field 'countryCode'
  if ("countryCode" %in% names(x) & "country" %in% names(x)) {
    ids <- !is.na(x$countryCode) & is.na(x$country)
    x$country[ids] <- x$countryCode[ids]
  }

  # Replacing the names of possibly missing columns
  if ("countryCode" %in% names(x) & !"country" %in% names(x))
    colnames(x)[which(colnames(x) == "countryCode")] <- "country"

  # Missing municipality that may be stored in the field 'county'
  if ("municipality" %in% names(x) & "county" %in% names(x)) {
    ids <- !is.na(x$county) & is.na(x$municipality)
    x$municipality[ids] <- x$county[ids]
  }

  if (!"municipality" %in% names(x) & "county" %in% names(x))
    colnames(x)[which(colnames(x) == "county")] <- "municipality"

  # Missing locality that may be stored in the field 'verbatimLocality'
  if ("locality" %in% names(x) & "verbatimLocality" %in% names(x)) {
    ids <- !is.na(x$verbatimLocality) & is.na(x$locality)
    x$locality[ids] <- x$verbatimLocality[ids]
  }

  if (!"locality" %in% names(x) & "verbatimLocality" %in% names(x))
    colnames(x)[which(colnames(x) == "verbatimLocality")] <- "locality"


  if (!any(c("country", "stateProvince", "municipality", "locality") %in% colnames(x)))
    stop("input object needs to have at least one of the following fields: country/countryCode, stateProvince, municipality/county and locality/verbatimLocality")

  ## Obtaining the intermediary data frame for editing
  x1 <- x[, match(loc.levels, colnames(x)), drop = FALSE]

  ## Loading the dictionary of names, terms, abbreviations and encodign problems to be replaced
  enc <- unwantedEncoding
  dic <- replaceNames
  missLocs <- missLocs
  wordsForSearch <- wordsForSearch

  ## Solving common encoding problems
  for (i in 1:length(enc))
    x1[] <- lapply(x1, gsub, pattern = names(enc)[i], replacement = enc[i],
                   ignore.case = TRUE, perl = TRUE)
  x1[] <- lapply(x1, gsub, pattern = "\u00AD", replacement = "", perl = TRUE) # soft hyphen
  x1[] <- lapply(x1, gsub, pattern = "\u00AO", replacement = "", perl = TRUE) # hidden breaking space

  ## ADM0: Country level
  if (any(c("country","countryCode") %in% loc.levels)) {

    # Standardizing country name notation
    x1[ ,"country"] <- prepCountry(x1[ ,"country"])

    # # Converting any country codes into country names
    # x1[nchar(x1[ ,"country"]) == 2 & !is.na(x1[ ,"country"]) ,"country"] <-
    #   countrycode::countrycode(as.character(x1[nchar(x1[ ,"country"]) == 2 & !is.na(x1[ ,"country"]) ,"country"]), 'iso2c', 'country.name')
    # x1[nchar(x1[ ,"country"]) == 3 & !is.na(x1[ ,"country"]) ,"country"] <-
    #   countrycode::countrycode(as.character(x1[nchar(x1[ ,"country"]) == 3 & !is.na(x1[ ,"country"]) ,"country"]), 'iso3c', 'country.name')
    #
    # # Removing unwanted characters
    # x1[, "country"] <- tolower(rmLatin(x1[, "country"]))
    # x1[, "country"] <- gsub("^\\[|\\]$", "", x1[, "country"], perl = TRUE)
    #
    # # Replacing '&' by 'and' in compound country names
    # x1[, "country"] <- stringr::str_replace_all(x1[, "country"], " & ", " and ")
    #
    # # Replacing abbreviated 'Saint' names
    # x1[, "country"] <- gsub("^st. ", "saint ", x1[, "country"], perl = TRUE)
    #
    # # Removing some prepositions from country names
    # x1[, "country"] <- gsub(" of the ", " ", x1[, "country"], fixed = TRUE)
    # x1[, "country"] <- gsub(" of ", " ", x1[, "country"], fixed = TRUE)

    # Replacing missing info by NA
    pattern <- paste(missLocs, collapse = "|")
    x1[, "country"] <- gsub(pattern, NA, x1[, "country"], perl = TRUE)
    x1[, "country"][grepl("desconhecid|unknown", x1[, "country"], perl = TRUE)] <- NA

    # Replacing variants, abbreviations, typos, and non-standard names
    tmp1 <- dic[dic$class %in% "country" & apply(is.na(dic[, 2:4]), 1, all), ]
    tmp2 <- tmp1$replace
    names(tmp2) <- tmp1$pattern
    names(tmp2) <- gsub("\\\\", "", names(tmp2), perl = TRUE)
    names(tmp2) <- gsub('\\.', "\\\\.", names(tmp2), perl = TRUE)
    names(tmp2) <- gsub('\\(', "\\\\(", names(tmp2), perl = TRUE)
    names(tmp2) <- gsub('\\)', "\\\\)", names(tmp2), perl = TRUE)
    x1[, "country"] <- stringr::str_replace_all(x1[, "country"], tmp2)

    # Missing country for non missing states and counties (only for uniquivocal states)
    tmp1 <- dic[dic$class %in% "country" & dic$condition2 %in% "not_is.na", ]
    reps <- unique(tmp1$replace)
    if (all(c("stateProvince", "municipality") %in% names(x1)) & any(reps %in% unique(x1[ ,"country"]))) {
      reps1 <- reps[reps %in% unique(x1[, "country"])]
      for (i in 1:length(reps)) {
        x1[is.na(x1[ ,"country"]) & !is.na(x1[, "municipality"]) &
             x1[ ,"stateProvince"] %in% tmp1$condition1[tmp1$replace %in% reps1[i]], "country"] <- reps1[i]
      }
    }
  }

  ## ADM1: State/Province level ##
  if ("stateProvince" %in% loc.levels) {
    # Removing unwanted characters
    x1[, "stateProvince"] <- tolower(rmLatin(x1[, "stateProvince"]))

    # Replacing missing info by NA
    pattern <- paste(missLocs, collapse = "|")
    x1[ ,"stateProvince"] <- gsub(pattern, NA, x1[ ,"stateProvince"], perl = TRUE)
    x1[ ,"stateProvince"][grepl("desconhecid|unknown", x1[, "stateProvince"], perl = TRUE)] <- NA

    # Removing unwanted prefixes and abbreviations
    pattern <- paste(wordsForSearch, collapse = "|")
    x1[ ,"stateProvince"] <- gsub(pattern, "", x1[ ,"stateProvince"], perl = TRUE)

    # Replacing variants, abbreviations, typos, and non-standard names
    tmp1 <- dic[dic$class %in% "stateProvince" & !is.na(dic[ ,2]),]
    tmp2 <- tmp1$replace
    names(tmp2) <- tmp1$pattern
    names(tmp2) <- gsub('\\.', "\\\\.", names(tmp2), perl = TRUE)
    cond0 <- unique(tmp1$condition0)
    if ("country" %in% names(x1)) {
      if (any(cond0 %in% unique(x1[, "country"]))) {
          cond1 <- cond0[cond0 %in% unique(x1[, "country"])]
          for (i in 1:length(cond1))  x1[x1[, "country"] %in% cond1[i] ,"stateProvince"] <-
              stringr::str_replace_all(x1[x1[,"country"] %in% cond1[i] ,"stateProvince"], tmp2)
      }
    }
  }

  ## ADM2: County, Departament, Commune
    if ("municipality" %in% loc.levels) {
      # Removing unwanted characters and replacing missing info by NA
      x1[, "municipality"] <- tolower(rmLatin(x1[, "municipality"]))

      # Replacing missing info by NA
      pattern <- paste(missLocs, collapse = "|")
      x1[ ,"municipality"] <- gsub(pattern, NA, x1[ ,"municipality"], perl = TRUE)
      x1[ ,"municipality"][grepl("desconhecid|unknown", x1[, "municipality"], perl = TRUE)] <- NA

      # Removing unwanted prefixes and abbreviations
      tmp1 <- dic[dic$class %in% "county" & apply(is.na(dic[,2:4]), 1, all),]
      tmp2 <- tmp1$replace
      names(tmp2) <- tmp1$pattern
      names(tmp2) <- gsub('\\.', "\\\\.", names(tmp2), perl = TRUE)
      x1[ ,"municipality"] <- stringr::str_replace_all(x1[ ,"municipality"], tmp2)

      # Some extra removals
      tmp1 <- dic[dic$class %in% "locality1" & apply(is.na(dic[,2:4]), 1, all),]
      tmp2 <- rep("", dim(tmp1)[1])
      names(tmp2) <- tmp1$pattern
      names(tmp2) <- gsub('\\.', "\\\\.", names(tmp2), perl = TRUE)
      names(tmp2) <- gsub('\\(', "\\\\(", names(tmp2), perl = TRUE)
      x1[ ,"municipality"] <- stringr::str_replace_all(x1[ ,"municipality"], tmp2)
    }

  ## ADM3: locality (park, farm, etc.)
    if (any(c("locality") %in% loc.levels)) {
      # Removing unwanted characters
      x1[, "locality"] <- tolower(rmLatin(x1[, "locality"]))

      # Removing unwanted prefixes and abbreviations (1st round)
      tmp1 <- dic[dic$class %in% "locality1" & apply(is.na(dic[,2:4]), 1, all),]
      tmp2 <- tmp1$replace
      names(tmp2) <- tmp1$pattern
      names(tmp2) <- gsub('\\.', "\\\\.", names(tmp2), perl = TRUE)
      names(tmp2) <- gsub('\\(', "\\\\(", names(tmp2), perl = TRUE)
      names(tmp2) <- gsub('\\)', "\\\\)", names(tmp2), perl = TRUE)
      x1[ ,"locality"] <- stringr::str_replace_all(x1[ ,"locality"], tmp2)

      # solving some substitution problems
      x1[ ,"locality"] <- gsub(" de de | de of ", " de ", x1[ ,"locality"], perl = TRUE)
      x1[ ,"locality"] <- gsub(" de do ",	" do ", x1[ ,"locality"], perl = TRUE)

      # Removing unwanted prefixes and abbreviations (2nd round)
      tmp1 <- dic[dic$class %in% "locality2" & apply(is.na(dic[,2:4]), 1, all),]
      tmp2 <- tmp1$replace
      names(tmp2) <- tmp1$pattern
      names(tmp2) <- gsub('\\.', "\\\\.", names(tmp2), perl = TRUE)
      names(tmp2) <- gsub('\\(', "\\\\(", names(tmp2), perl = TRUE)
      names(tmp2) <- gsub('\\)', "\\\\)", names(tmp2), perl = TRUE)
      x1[ ,"locality"] = stringr::str_replace_all(x1[ ,"locality"], tmp2)
    }

    if (c("locality") %in% loc.levels & scrap) {
      # Spliting the locality vector to find missing information
      n4 <- strsplit(x1[ ,"locality"], ",|\\.|:|\\(|\\)|;", perl = TRUE)
      n4 <- sapply(n4, stringr::str_trim)

      # trying to get missing counties from the locality description (e.g. "Pico das Almas, municipio de Rio de Contas")
      n4.2 <- as.character(sapply(n4, function(x)
        paste(unique(stringr::str_trim(
          x[grepl("municipio|municipality|county|provincia|village", x, perl = TRUE)]
        )), collapse = "|")))
      n4.2 <- stringr::str_trim(n4.2)
      n4.2 <- gsub("municipio de |municipality of |municipio do |^county of |^provincia de|^provincia of|village of",
                  "",
                  n4.2, perl = TRUE)
      n4.2 <- gsub("municipio |municipality |^county |^provincia |village ",
                  "",
                  n4.2, perl = TRUE)
      n4.2[n4.2 %in% ""] <- NA

      # getting missing counties that may be the first part of the locality description (e.g. "Rio de Contas, Pico das Almas")
      n4.2.1 <- as.character(sapply(n4, function(x) x[1]))
      n4.2.1 <- stringr::str_trim(n4.2.1)
      n4.2.1[n4.2.1 %in% ""] <- NA

      # isolating localities possibily in the gazetter (e.g. parks, serras, farms)
      locais <- "parque|reserva|reserve|fazenda|nacional|estadual|parna|flona|rebio|rppn|e\\.e\\.|biologica|ecologica|extrativista|park|farm|estrada|rodovia|road|^sitio|^mata|^horto|^jardim|campus|^pico|^serra|^sierra|^morro|^chapada"
      n4.3 <- as.character(sapply(n4, function(x)
          paste(unique(stringr::str_trim(
            x[grepl(locais, x, perl = TRUE)]
          )), collapse = ", ")))
      n4.3 <- stringr::str_trim(n4.3)
      n4.3[n4.3 %in% ""] <- NA

      #other localities, than the ones in 'locais' and counties (first sentence, prior to the first comma when n4.3 is empty)
      n4.3[is.na(n4.3) & !is.na(x1[ ,"locality"])] <- as.character(sapply(n4[is.na(n4.3) & !is.na(x1[ ,"locality"])],
                                                                         function(x) my.head(x[!grepl("municipio|municipality|county|provincia|village", x, perl = TRUE)])))

      # Replacing missing counties
      if (any(c("municipality", "county") %in% loc.levels)) {
        # priority 1: localities specifying a county name
        x1[, "municipality"][is.na(x1[, "municipality"]) & !is.na(n4.2)] <-
          stringr::str_trim(n4.2[is.na(x1[, "municipality"]) & !is.na(n4.2)])
        # priority 2: first part of the locality description
        x1[, "municipality"][is.na(x1[, "municipality"]) & is.na(n4.2)] <-
          stringr::str_trim(n4.2.1[is.na(x1[, "municipality"]) & is.na(n4.2)])
      }

      # Replacing edited/missing localities
      x1[ ,"locality"][!is.na(n4.3)] <- n4.3[!is.na(n4.3)]
      x1[ ,"locality"][is.na(n4.3)] <-
        as.character(sapply(n4[is.na(n4.3)], function(x) x[1]))
    }

  ## Trimming and editing the edited columns
  for (i in 1:length(loc.levels))
    x1[, i] <- as.character(stringr::str_trim(x1[, i]))
  for (i in 1:length(loc.levels))
    x1[, i] <- gsub("^-$", NA, x1[, i], perl = TRUE)

  ## Resolution of the locality information provided
  tmp <- apply(x1[, loc.levels, drop = FALSE], 1, function(x)
      which(is.na(x))[1] - 1)
  tmp[tmp %in% 0] <- length(loc.levels) + 1
  tmp[is.na(tmp)] <- length(loc.levels)
  resol.orig <- c(loc.levels, "no_info")[tmp]

  ## Preparing the output
  if (length(loc.levels) == 1) {
    res <- as.vector(x1[,1])

    if (!to.lower)
      res <- stringr::str_to_title(res)

  } else {
    names(x1) <- paste0(names(x1), ".new")
    res <- as.data.frame(x1)
    if (c("locality.new") %in% names(x1) & scrap)
      res$locality.scrap <- n4.2.1

    if (!to.lower) {
      names.res <- names(res)
      for(i in 1:length(names.res))
        res[, names.res[i]] <- stringr::str_to_title(res[, names.res[i]])
    }
    res$resol.orig <- resol.orig
  }

  ## Merging the results and returning
  result <- cbind.data.frame(x, res, stringsAsFactors = FALSE)
  return(result)
}
