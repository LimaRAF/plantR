#' @title Format Locality Information
#'
#' @description Standardize the notation of the locality fields country,
#'   stateProvince, municipality and locality.
#'
#' @param x a data frame
#' @param admin.levels a vector containing the fields (i.e. administrative
#'   levels) to be formatted.
#' @param scrap logical. Should the search of missing locality information be
#'   performed?
#'
#' @return The input data frame \code{x}, plus the new columns with the formatted
#'   fields.
#'
#' @details The function performs several edits and replacements. Country names
#'   are formated in the international format, letters are lower-cased, and
#'   special characters and common abbreviations are removed.
#'
#'   By default, this function formats all four locality fields simultaneously
#'   (i.e. country, stateProvince, municipality, locality), but the user can
#'   chose among these fields through the argument `admin.levels`. However, the
#'   editing process is more complete if all the information is available for
#'   the four fields administrative levels mentioned above.
#'
#'   The argument `scrap` controls the search for missing municipality
#'   information from the field 'locality'. It also performs some extra editing
#'   and croping of the field 'locality' in order to obtain more standardized
#'   locality descriptions. This argument uses different ways of splitting and
#'   cropping the locality descripton in order to find missing information.
#'   Although it does not always result in an accurate extraction of the
#'   information, it provides an extra tool to organize locality information
#'   which are not provided in the appropriate columns.
#'
#'   The function automatically returns the original resolution of the
#'   information provided. For instance, if only country information is provided
#'   (i.e. field is not empty), then the resolution is flagged as 'country'; if
#'   country and stateProvince are given, then the resolution is flagged as
#'   'stateProvince', and so on.
#'
#' @author Renato A. F. de Lima
#'
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_trim
#' @importFrom countrycode countrycode
#'
#' @export fixLoc
#'
#' @examples
#' # Creating a data frame with locality information
#' (occs = data.frame(country = c("BR", "Brasil", "BRA", "Brazil", NA),
#'                    stateProvince = c("MG", "Minas Gerais", "Minas Geraes", "Minas Gerais", "Minas Gerais"),
#'                    municipality = c("Lavras", "lavras", NA, NA, "Lavras"),
#'                    locality = c(NA, "UFLA", "municipio de Lavras, campus UFLA", "Minas Gerais, municipio Lavras", NA)
#'                    ))
#'
#' # Formating the locality information
#' fixLoc(occs, scrap = FALSE)
#'
#' # Formating and scrapping the locality information
#' fixLoc(occs, scrap = TRUE)
#'
#' # Formating the locality information only at country and state levels
#' fixLoc(occs, admin.levels = c("country", "stateProvince"))
#'
fixLoc <- function(x,
                   admin.levels = c("country", "stateProvince", "municipality", "locality"),
                   scrap = TRUE) {

  ##To decide: Include extra ADM level between country and states??? Regions or Departments?? see the case of Peru

  ## checking input:
  if (!class(x) == "data.frame")
    stop("input object needs to be a data frame!")

  if (any(names(x) %in% c("countryCode")))
    colnames(x)[which(colnames(x) == "countryCode")] <- "country"

  if (any(names(x) %in% c("county")))
    colnames(x)[which(colnames(x) == "county")] <- "municipality"

  if (!any(c("country", "stateProvince", "municipality", "locality") %in% colnames(x)))
    stop("input object needs to have at least one of the following fields: country/countryCode, stateProvince, county/municipality and locality")

  ## Obtaining the intermediary data frame for editing
  if (length(admin.levels) == 1) {

    x1 <- x[which(colnames(x) %in% admin.levels)]

  } else {

    x1 <- x[, which(colnames(x) %in% admin.levels)]

  }

  ## Solving some common encoding problems
  x1[] <- lapply(x1, gsub, pattern = "ã¡|ã¢|ã£", replacement = "a", ignore.case = TRUE, perl = TRUE)
  x1[] <- lapply(x1, gsub, pattern = "ã©|ãª", replacement = "e", ignore.case = TRUE, perl = TRUE)
  x1[] <- lapply(x1, gsub, pattern = "ã§", replacement = "c", ignore.case = TRUE, perl = TRUE)
  x1[] <- lapply(x1, gsub, pattern = "ãº", replacement = "u", ignore.case = TRUE, perl = TRUE)
  x1[] <- lapply(x1, gsub, pattern = "ã´", replacement = "o", ignore.case = TRUE, perl = TRUE)
  x1[] <- lapply(x1, gsub, pattern = "ã\u008d", replacement = "i", ignore.case = TRUE, perl = TRUE)
  x1[] <- lapply(x1, gsub, pattern = "\u00AD", replacement = "", perl = TRUE) # soft hyphen
  x1[] <- lapply(x1, gsub, pattern = "\u00AO", replacement = "", perl = TRUE) # hidden breaking space
  x1[] <- lapply(x1, gsub, pattern = "&#225;", replacement = "a", perl = TRUE)

  ## Loading the dictionary of names, terms and abbreviations to be replaced
  dic <- replaceNames
  unwanted_array <- unwanted_array
  missLocs <- missLocs
  wordsForSearch <- wordsForSearch

  ### TRY TO GET FROM FILED LOCALITY INFOS FOM MISSING STATE AND COUNTY CONTAINING THE FOLLOWING TERMS:
  #departamento del
  #department of
  #departamiento de
  #province of
  #departemento del
  ### in addition, check why thereare edits like 'departamento detolima' in the data


  ## ADM0: Country level
  if(any(c("country", "countryCode") %in% admin.levels)) {
    # Converting any country codes into country names
    x1[nchar(x1[ ,"country"]) == 2 & !is.na(x1[ ,"country"]) ,"country"] <-
      countrycode::countrycode(as.character(x1[nchar(x1[ ,"country"]) == 2 & !is.na(x1[ ,"country"]) ,"country"]), 'iso2c', 'country.name')
    x1[nchar(x1[ ,"country"]) == 3 & !is.na(x1[ ,"country"]) ,"country"] <-
      countrycode::countrycode(as.character(x1[nchar(x1[ ,"country"]) == 3 & !is.na(x1[ ,"country"]) ,"country"]), 'iso3c', 'country.name')

    # Removing unwanted characters
    x1[, "country"] <-
      tolower(chartr(
        paste(names(unwanted_array), collapse = ''),
        paste(unwanted_array, collapse = ''),
        x1[, "country"]))

    # Replacing '&' by 'and' in compound country names
    x1[, "country"] <- stringr::str_replace_all(x1[, "country"], " & ", " and ")

    # Replacing abbreviated 'Saint' names
    x1[, "country"] <- gsub("^st. ", "saint ", x1[, "country"])

    # Removing some prepositions from country names
    x1[, "country"] <- gsub(" of the ", " ", x1[, "country"])
    x1[, "country"] <- gsub(" of ", " ", x1[, "country"])

    # Replacing missing info by NA
    pattern = paste(missLocs, collapse = "|")
    x1[, "country"] <- gsub(pattern, NA, x1[, "country"], perl = TRUE)
    x1[, "country"][grepl("desconhecid|unknown", x1[, "country"])] <- NA

    # Replacing variants, abbreviations, typos, and non-standard names
    tmp1 <- dic[dic$class %in% "country" & apply(is.na(dic[, 2:4]), 1, all), ]
    tmp2 <- tmp1$replace
    names(tmp2) = tmp1$pattern
    names(tmp2) <- gsub("\\\\", "", names(tmp2))
    x1[, "country"] <- stringr::str_replace_all(x1[, "country"], tmp2)

    # Missing country for non missing states and counties (only for uniquivocal states)
    tmp1 <- dic[dic$class %in% "country" & dic$condition2 %in% "not_is.na", ]
    reps <- unique(tmp1$replace)
    if(all(c("stateProvince", "municipality") %in% names(x1)) & any(reps %in% unique(x1[ ,"country"]))) {
      reps1 = reps[reps %in% unique(x1[, "country"])]
      for(i in 1:length(reps)) x1[is.na(x1[ ,"country"]) & !is.na(x1[, "municipality"]) &
                                    x1[ ,"stateProvince"] %in% tmp1$condition1[tmp1$replace %in% reps1[i]], "country"] <-
          reps1[i]
    }
  }

  ## ADM1: State/Province level ##
  if("stateProvince" %in% admin.levels) {
    # Removing unwanted characters
    x1[, "stateProvince"] <-
      tolower(chartr(
        paste(names(unwanted_array), collapse = ''),
        paste(unwanted_array, collapse = ''),
        x1[, "stateProvince"]))

    # Replacing missing info by NA
    pattern <- paste(missLocs, collapse = "|")
    x1[ ,"stateProvince"] <- gsub(pattern, NA, x1[ ,"stateProvince"], perl = TRUE)
    x1[ ,"stateProvince"][grepl("desconhecid|unknown", x1[, "stateProvince"])] <- NA

    # Removing unwanted prefixes and abbreviations
    pattern <- paste(wordsForSearch, collapse = "|")
    x1[ ,"stateProvince"] <- gsub(pattern, "", x1[ ,"stateProvince"], perl = TRUE)

    # Replacing variants, abbreviations, typos, and non-standard names
    tmp1 <- dic[dic$class %in% "stateProvince" & !is.na(dic[ ,2]),]
    tmp2 <- tmp1$replace
    names(tmp2) <- tmp1$pattern
    names(tmp2) <- gsub('\\.', "\\\\.", names(tmp2))
    cond0 <- unique(tmp1$condition0)
    if("country" %in% names(x1)) {
      if(any(cond0 %in% unique(x1[, "country"]))) {
          cond1 <- cond0[cond0 %in% unique(x1[, "country"])]
          for(i in 1:length(cond1))  x1[x1[, "country"] %in% cond1[i] ,"stateProvince"] =
              stringr::str_replace_all(x1[x1[,"country"] %in% cond1[i] ,"stateProvince"], tmp2)
        }
      }
    }

  ## ADM2: County, Departament, Commune
    if(any(c("municipality","county") %in% admin.levels)) {
      # Removing unwanted characters and replacing missing info by NA
      x1[, "municipality"] <-
        tolower(chartr(
          paste(names(unwanted_array), collapse = ''),
          paste(unwanted_array, collapse = ''),
          x1[, "municipality"]))

      # Replacing missing info by NA
      pattern <- paste(missLocs, collapse = "|")
      x1[ ,"municipality"] <- gsub(pattern, NA, x1[ ,"municipality"], perl = TRUE)
      x1[ ,"municipality"][grepl("desconhecid|unknown", x1[, "municipality"])] <- NA

      # Removing unwanted prefixes and abbreviations
      tmp1 <- dic[dic$class %in% "county" & apply(is.na(dic[,2:4]), 1, all),]
      tmp2 <- tmp1$replace
      names(tmp2) <- tmp1$pattern
      names(tmp2) <- gsub('\\.', "\\\\.", names(tmp2))
      x1[ ,"municipality"] <- stringr::str_replace_all(x1[ ,"municipality"], tmp2)
    }

  ## ADM3: locality (park, farm, etc.)
    if(any(c("locality") %in% admin.levels)) {
      # Removing unwanted characters
      x1[, "locality"] <-
        tolower(chartr(
          paste(names(unwanted_array), collapse = ''),
          paste(unwanted_array, collapse = ''),
          x1[, "locality"]))

      # Removing unwanted prefixes and abbreviations (1st round)
      tmp1 <- dic[dic$class %in% "locality1" & apply(is.na(dic[,2:4]), 1, all),]
      tmp2 <- tmp1$replace
      names(tmp2) <- tmp1$pattern
      names(tmp2) <- gsub('\\.', "\\\\.", names(tmp2))
      names(tmp2) <- gsub('\\(', "\\\\(", names(tmp2))
      names(tmp2) <- gsub('\\)', "\\\\)", names(tmp2))
      x1[ ,"locality"] <- stringr::str_replace_all(x1[ ,"locality"], tmp2)

      # solving some substitution problems
      x1[ ,"locality"] <- gsub(" de de | de of ", " de ", x1[ ,"locality"], perl = TRUE)
      x1[ ,"locality"] <- gsub(" de do ",	" do ", x1[ ,"locality"], perl = TRUE)

      # Removing unwanted prefixes and abbreviations (2nd round)
      tmp1 <- dic[dic$class %in% "locality2" & apply(is.na(dic[,2:4]), 1, all),]
      tmp2 <- tmp1$replace
      names(tmp2) <- tmp1$pattern
      names(tmp2) <- gsub('\\.', "\\\\.", names(tmp2))
      names(tmp2) <- gsub('\\(', "\\\\(", names(tmp2))
      names(tmp2) <- gsub('\\)', "\\\\)", names(tmp2))
      x1[ ,"locality"] = stringr::str_replace_all(x1[ ,"locality"], tmp2)
    }

    if(c("locality") %in% admin.levels & scrap) {
      # Spliting the locality vector to find missing information
      n4 <- strsplit(x1[ ,"locality"], ",|\\.|:|\\(|)|;")
      n4 <- sapply(n4, stringr::str_trim)

      # trying to get missing counties from the locality description (e.g. "Pico das Almas, municipio de Rio de Contas")
      n4.2 <- as.character(sapply(n4, function(x)
        paste(unique(stringr::str_trim(
          x[grepl("municipio|municipality|county|provincia|village", x)]
        )), collapse = "|")))
      n4.2 <- stringr::str_trim(n4.2)
      n4.2 <- gsub("municipio de |municipality of |municipio do |^county of |^provincia de|^provincia of|village of",
                  "",
                  n4.2)
      n4.2 <- gsub("municipio |municipality |^county |^provincia |village ",
                  "",
                  n4.2)
      n4.2[n4.2 %in% ""] <- NA

      # getting missing counties that may be the first part of the locality description (e.g. "Rio de Contas, Pico das Almas")
      n4.2.1 <- as.character(sapply(n4, function(x) x[1]))
      n4.2.1 <- str_trim(n4.2.1)
      n4.2.1[n4.2.1 %in% ""] <- NA

      # isolating localities possibily in the gazetter (e.g. parks, serras, farms)
      locais <- "parque|reserva|reserve|fazenda|nacional|estadual|parna|flona|rebio|rppn|e\\.e\\.|biologica|ecologica|extrativista|park|farm|estrada|rodovia|road|^sitio|^mata|^horto|^jardim|campus|^pico|^serra|^sierra|^morro|^chapada"
      n4.3 <- as.character(sapply(n4, function(x)
          paste(unique(stringr::str_trim(
            x[grepl(locais, x)]
          )), collapse = ", ")))
      n4.3 <- stringr::str_trim(n4.3)
      n4.3[n4.3 %in% ""] <- NA

      #other localities, than the ones in 'locais' and counties (first sentence, prior to the first comma when n4.3 is empty)
      n4.3[is.na(n4.3) & !is.na(x1[ ,"locality"])] <- as.character(sapply(n4[is.na(n4.3) & !is.na(x1[ ,"locality"])],
                                                                         function(x) head(x[!grepl("municipio|municipality|county|provincia|village",x)],1)))

      # Replacing missing counties
      if (any(c("municipality", "county") %in% admin.levels)) {
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
  for (i in 1:length(admin.levels))
    x1[, i] <- as.character(stringr::str_trim(x1[, i]))
  for (i in 1:length(admin.levels))
    x1[, i] <- gsub("^-$", NA, x1[, i])

  ## Resolution of the locality information provided
  tmp <- apply(x1[, admin.levels], 1, function(x)
      which(is.na(x))[1] - 1)
  tmp[tmp %in% 0] <- length(admin.levels) + 1
  tmp[is.na(tmp)] <- length(admin.levels)
  resol.orig <- c(admin.levels, "no_info")[tmp]

  ## Preparing the output
  if (length(admin.levels) == 1) {
    res <- as.vector(x1)
  } else {
    names(x1) <- paste0(names(x1), ".new")
    res <- as.data.frame(x1)
    if (c("locality.new") %in% names(x1) &
        scrap == TRUE)
      res$locality.scrap <- n4.2.1
    res$resol.orig <- resol.orig
  }

  ## Merging the results and returning
  result <- cbind.data.frame(x, res)
  return(result)
}
