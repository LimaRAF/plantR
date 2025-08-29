#' @title Format Locality Information
#'
#' @description Standardize the notation of the locality fields
#'   country, stateProvince, municipality and locality, and search
#'   from some missing information within the available locality
#'   information.
#'
#' @param x a data frame containing typical locality fields from
#'   species records.
#' @param loc.levels a vector containing the names of the locality
#'   fields to be formatted.
#' @param fix.encoding logical. Should the function try to find and
#'   fix common encoding problems from text in 'latin1'? Defaults to
#'   TRUE
#' @param scrap logical. Should the search of missing locality
#'   information be performed? Default to TRUE.
#' @param to.lower logical. Should the output locality names be return
#'   in lower cases? Default to TRUE.
#'
#' @return The input data frame \code{x}, plus the '.new' columns with
#'   the formatted fields and the resolution of the locality
#'   information available.
#'
#' @details The function performs several edits and replacements.
#'   Country names are formatted into the international format,
#'   letters are lower-cased, and special characters and common
#'   abbreviations are removed.
#'
#'   By default, this function formats all four locality fields
#'   simultaneously (i.e. country, stateProvince, municipality,
#'   locality), but the user can choose among these fields through the
#'   argument `loc.levels`. However, the process of searching for
#'   missing information is more complete if all the four locality
#'   fields mentioned above are available.
#'
#'   If present, other Darwin Core fields are used internally to
#'   obtain missing information on the locality fields declared above,
#'   namely: 'countryCode', 'county', and 'verbatimLocality'.
#'
#'   The argument `fix.encoding` controls the attempt to find and
#'   replace common enconding problems from text with special
#'   characters. This replacement is performed using the internal
#'   object `unwantedEncoding`. Note that the replacement actually
#'   removes the special characters (i.e. encoding problem of 'ã'
#'   becomes 'a').
#'
#'   The argument `scrap` controls the search for missing municipality
#'   information from the field 'locality'. It also performs some
#'   extra editing and cropping of the field 'locality' in order to
#'   obtain more standardized locality descriptions. This argument
#'   uses different ways of splitting and cropping the locality
#'   description in order to find missing information. Although it
#'   does not always result in an accurate extraction of the
#'   information, it provides an extra tool to organize locality
#'   information which are not provided in the appropriate columns.
#'
#'   The function automatically returns the original resolution of the
#'   locality information provided. For instance, if only country
#'   information is provided (i.e. field is not empty), then the
#'   resolution is flagged as 'country'; if country and stateProvince
#'   are given, then the resolution is flagged as 'stateProvince', and
#'   so on.
#'
#' The output of this function contains columns which are reserved
#' within the __plantR__ workflow. These columns cannot be present in
#' the input data frame. The full list of reserved columns is stored
#' in the internal object `reservedColNames`.
#'
#' @author Renato A. Ferreira de Lima
#'
#' @importFrom stringr str_to_title str_replace_all
#' @importFrom countrycode countrycode
#' @importFrom utils head
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
#' fixLoc(df, loc.levels = c("country", "stateProvince"))
#'
fixLoc <- function(x,
                   loc.levels = c("country", "stateProvince", "municipality", "locality"),
                   fix.encoding = TRUE,
                   scrap = TRUE,
                   to.lower = TRUE) {

  ## checking input:
  if (!inherits(x, "data.frame"))
    stop("input object needs to be a data frame!")

  na.strings <- c("", " ", "NA", NA)
  # Missing country that may be stored in the field 'countryCode'
  if ("countryCode" %in% names(x) & "country" %in% names(x)) {
    ids <- !x$countryCode %in% na.strings & x$country %in% na.strings
    x$country[ids] <- x$countryCode[ids]
  }

  # Replacing the names of possibly missing columns
  if ("countryCode" %in% names(x) & !"country" %in% names(x))
    colnames(x)[which(colnames(x) == "countryCode")] <- "country"

  # Missing municipality that may be stored in the field 'county'
  if ("municipality" %in% names(x) & "county" %in% names(x)) {
    ids <- !x$county %in% na.strings & x$municipality %in% na.strings
    x$municipality[ids] <- x$county[ids]
  }

  if (!"municipality" %in% names(x) & "county" %in% names(x))
    colnames(x)[which(colnames(x) == "county")] <- "municipality"

  # Missing locality that may be stored in the field 'verbatimLocality'
  if ("locality" %in% names(x) & "verbatimLocality" %in% names(x)) {
    ids <- !x$verbatimLocality %in% na.strings &
              x$locality %in% na.strings
    x$locality[ids] <- x$verbatimLocality[ids]
  }

  if (!"locality" %in% names(x) & "verbatimLocality" %in% names(x))
    colnames(x)[which(colnames(x) == "verbatimLocality")] <- "locality"

  if (!any(c("country", "stateProvince", "municipality", "locality") %in% colnames(x)))
    stop("input object needs to have at least one of the following fields: country/countryCode, stateProvince, municipality/county and locality/verbatimLocality")

  ## Obtaining the intermediary data frame for editing
  loc.levels <- loc.levels[loc.levels %in% names(x)]
  x1 <- x[, match(loc.levels, colnames(x), nomatch = 0), drop = FALSE]

  ## Loading the dictionary of names, terms, abbreviations and encodign problems to be replaced
  dic <- replaceNames
  missLocs <- missLocs
  wordsForSearch <- wordsForSearch

  ## Solving common encoding problems
  if (fix.encoding) {
    enc <- unwantedEncoding
    for (i in 1:length(enc))
      x1[] <- lapply(x1, gsub, pattern = names(enc)[i], replacement = enc[i],
                     ignore.case = TRUE, perl = TRUE)
    x1[] <- lapply(x1, gsub, pattern = "\u00AD", replacement = "", perl = TRUE) # soft hyphen
    x1[] <- lapply(x1, gsub, pattern = "\u00AO", replacement = "", perl = TRUE) # hidden breaking space
  }

  ## ADM0: Country level
  if (any(c("country","countryCode") %in% loc.levels)) {

    # Standardizing country name notation
    x1[ ,"country"] <- prepCountry(x1[ ,"country"])

    # Replacing missing info by NA
    pattern <- paste(missLocs, collapse = "|")
    x1[, "country"] <- gsub(pattern, NA, x1[, "country"], perl = TRUE)
    x1[, "country"][grepl("desconhecid|unknown", x1[, "country"], perl = TRUE)] <- NA

    # Removing brackets from country names
    bracks <- grepl('^\\[', x1[, "country"], perl = TRUE) &
                grepl('\\]$', x1[, "country"], perl = TRUE)
    if (any(bracks))
      x1[bracks, "country"] <-
        gsub("^\\[|\\]$", "", x1[bracks, "country"], perl = TRUE)

    # Replacing variants, abbreviations, typos, and non-standard names
    tmp1 <- dic[dic$class %in% "country" &
                  apply(is.na(dic[, 2:4]), 1, all), ]
    tmp2 <- tmp1$replace
    names(tmp2) <- tmp1$pattern
    names(tmp2) <- gsub("\\\\", "", names(tmp2), perl = TRUE)
    names(tmp2) <- gsub('\\.', "\\\\.", names(tmp2), perl = TRUE)
    names(tmp2) <- gsub('\\(', "\\\\(", names(tmp2), perl = TRUE)
    names(tmp2) <- gsub('\\)', "\\\\)", names(tmp2), perl = TRUE)
    names(tmp2) <- gsub('\\[', "\\\\[", names(tmp2), perl = TRUE)
    names(tmp2) <- gsub('\\]', "\\\\]", names(tmp2), perl = TRUE)
    x1[, "country"] <- stringr::str_replace_all(x1[, "country"], tmp2)

    # Missing country for non missing states and counties (only for uniquivocal states)
    tmp1 <- dic[dic$class %in% "country" & dic$condition2 %in% "not_is.na", ]
    reps <- unique(tmp1$replace)
    if (all(c("stateProvince", "municipality") %in% names(x1)) &
          any(reps %in% unique(x1[ ,"country"]))) {
      reps1 <- reps[reps %in% unique(x1[, "country"])]
      for (i in 1:length(reps1)) {
        x1$country[is.na(x1[ ,"country"]) & !is.na(x1[, "municipality"]) &
             x1[ ,"stateProvince"] %in% tmp1$condition1[tmp1$replace %in% reps1[i]]] <- reps1[i]
      }
    }
  }

  ## ADM1: State/Province level ##
  if ("stateProvince" %in% loc.levels) {
    # Removing unwanted characters
    x1[, "stateProvince"] <- tolower(rmLatin(x1[, "stateProvince"]))

    # Replacing missing info by NA
    pattern <- paste(missLocs, collapse = "|")
    x1[ ,"stateProvince"] <- gsub(pattern, NA, x1[ ,"stateProvince"],
                                  perl = TRUE)
    x1[ ,"stateProvince"][grepl("desconhecid|unknown",
                                x1[, "stateProvince"], perl = TRUE)] <- NA
    x1[ ,"stateProvince"][x1[ ,"stateProvince"] %in% c("", " ")] <- NA

    # Replacing some general abbreviations non-standard names
    x1[ ,"stateProvince"] <- gsub("^st\\.\\s|^st\\s", "saint ", x1[ ,"stateProvince"],
                                  perl = TRUE)

    # Removing unwanted prefixes and abbreviations
    pattern <- paste(wordsForSearch, collapse = "|")
    x1[ ,"stateProvince"] <- gsub(pattern, "", x1[ ,"stateProvince"],
                                  perl = TRUE)

    # Replacing variants, abbreviations, typos, and non-standard names
    tmp1 <- dic[dic$class %in% "stateProvince" & !is.na(dic[ ,2]),]
    tmp2 <- tmp1$replace
    names(tmp2) <- tmp1$pattern
    names(tmp2) <- gsub('\\.', "\\\\.", names(tmp2), perl = TRUE)
    cond0 <- unique(tmp1$condition0)
    if ("country" %in% names(x1)) {
      if (any(cond0 %in% unique(x1[, "country"]))) {
          cond1 <- cond0[cond0 %in% unique(x1[, "country"])]
          for (i in 1:length(cond1)) {
            tmp2.i <- tmp2[tmp1$condition0 %in% cond1[i]]
            x1[x1[, "country"] %in% cond1[i] ,"stateProvince"] <-
              stringr::str_replace_all(x1[x1[,"country"] %in% cond1[i] ,"stateProvince"], tmp2.i)
          }
      }
    }
  }

  ## ADM2: County, Departament, Commune
  if ("municipality" %in% loc.levels) {
      # Removing unwanted characters and replacing missing info by NA
      x1[, "municipality"] <- tolower(rmLatin(x1[, "municipality"]))

      # Replacing missing info by NA
      pattern <- paste(missLocs, collapse = "|")
      x1[ ,"municipality"] <- gsub(pattern, NA, x1[ ,"municipality"],
                                   perl = TRUE)
      x1[ ,"municipality"][grepl("desconhecid|unknown", x1[, "municipality"],
                                 perl = TRUE)] <- NA
      x1[ ,"municipality"][x1[ ,"municipality"] %in% c("", " ")] <- NA

      # Replacing some general abbreviations non-standard names
      x1[ ,"municipality"] <-
        gsub("munic\\.|munic_pio", "municipio", x1[ ,"municipality"],
                                    perl = TRUE)

      # Replacing locality description at the municipality field
      if ("locality" %in% loc.levels) {
        rep_these <- nchar(x1[ ,"municipality"]) > 45 & is.na(x1[ ,"locality"])
        rep_these[is.na(rep_these)] <- FALSE
        if (any(rep_these)) {
          x1[["locality"]][rep_these] <- x1[["municipality"]][rep_these]
          x1[["municipality"]][rep_these] <- NA
        }
      }

      # Removing unwanted prefixes and abbreviations
      pattern <- paste(wordsForSearch, collapse = "|")
      x1[ ,"municipality"] <- gsub(pattern, "", x1[ ,"municipality"],
                                   perl = TRUE)

      # Removing unwanted prefixes and abbreviations
      tmp1 <- dic[dic$class %in% "county" &
                    apply(is.na(dic[,2:4]), 1, all),]
      tmp2 <- tmp1$replace
      names(tmp2) <- tmp1$pattern
      names(tmp2) <- gsub('\\.', "\\\\.", names(tmp2), perl = TRUE)
      x1[ ,"municipality"] <-
        stringr::str_replace_all(x1[ ,"municipality"], tmp2)

      # Some extra removals
      tmp1 <- dic[dic$class %in% "locality1" &
                    apply(is.na(dic[,2:4]), 1, all),]
      tmp2 <- rep("", dim(tmp1)[1])
      names(tmp2) <- tmp1$pattern
      names(tmp2) <- gsub('\\.', "\\\\.", names(tmp2), perl = TRUE)
      names(tmp2) <- gsub('\\(', "\\\\(", names(tmp2), perl = TRUE)
      x1[ ,"municipality"] <-
        stringr::str_replace_all(x1[ ,"municipality"], tmp2)
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
      x1[ ,"locality"] <-
        stringr::str_replace_all(x1[ ,"locality"], tmp2)

      # solving some substitution problems
      x1[ ,"locality"] <-
        gsub(" de de | de of ", " de ", x1[ ,"locality"], perl = TRUE)
      x1[ ,"locality"] <-
        gsub(" de do ",	" do ", x1[ ,"locality"], perl = TRUE)

      # Removing unwanted prefixes and abbreviations (2nd round)
      tmp1 <- dic[dic$class %in% "locality2" &
                    apply(is.na(dic[,2:4]), 1, all),]
      tmp2 <- tmp1$replace
      names(tmp2) <- tmp1$pattern
      names(tmp2) <- gsub('\\.', "\\\\.", names(tmp2), perl = TRUE)
      names(tmp2) <- gsub('\\(', "\\\\(", names(tmp2), perl = TRUE)
      names(tmp2) <- gsub('\\)', "\\\\)", names(tmp2), perl = TRUE)
      x1[ ,"locality"] <-
        stringr::str_replace_all(x1[ ,"locality"], tmp2)
  }

  ## scrapping ADM3 for additional locality information
  if (c("locality") %in% loc.levels & scrap) {
    empty_vec <- c("", " ", NA, "na", "<na>")
    w_locality <- !x1[["locality"]] %in% empty_vec

    # Some few fixes before splitting
    new_loc <-
      gsub("_x0003__x0001_", "a", x1[["locality"]][w_locality],
           fixed = TRUE)
    new_loc <-
      squish(gsub("_", ", ", new_loc, fixed = TRUE))
    new_loc <-
      gsub("mun\\.,|mun,\\.", "municipio,", new_loc, perl = TRUE)

    # Spliting the locality vector to find missing information
    n4 <- strsplit(new_loc, ",|\\.|:|\\(|\\)|;| - |\\/|&", ## Remove the " - "?
                   perl = TRUE)
    #n4 <- sapply(n4, squish) #### CHECK POSSIBLE OMISSION PROBLEMS ####

    ## scrapping ADM1 and ADM2 from the locality field (ADM3)
    if ("stateProvince" %in% loc.levels) {
      no_state <- x1[["stateProvince"]][w_locality] %in% empty_vec
    } else { no_state <- rep(FALSE, nrow(x1))}

    if (any(c("municipality", "county") %in% loc.levels)) {
      no_munic <- x1[["municipality"]][w_locality] %in% empty_vec
    } else { no_munic <- rep(FALSE, nrow(x1))}

    if (any(no_state | no_munic)) {

      # trying to get missing stateProvince (e.g. "Estado de Sao Paulo: Cunha")
      patt_yes0 <- "estado|state|provincia|departamento|province|canton"
      patt_no0 <- "estadual|provincial|departamental|provincias|provinces|cantones"
      if (any(no_state)) {
        n4.1 <- as.character(sapply(n4[no_state], function(x)
          paste(unique(gsub("^ | $", "",
            x[grepl(patt_yes0, x, perl = TRUE) & !grepl(patt_no0, x, perl = TRUE)], perl = TRUE
          )), collapse = "|")))
        n4.1 <- squish(n4.1)
        n4.1 <- gsub("^estado de|^state of|^provincia de|^provincia of|^province de|^departamento de",
                     "",
                     n4.1, perl = TRUE)
        n4.1 <- gsub("estado |state |provincia |departamento |province ",
                     "",
                     n4.1, perl = TRUE)
        n4.1 <- squish(gsub("state$| provincia$| province$",
                            "",
                            n4.1, perl = TRUE))
        n4.1[n4.1 %in% c(empty_vec, "estado", "state", "provincia",
                         "province", "departamento")] <- NA

        # Replacing missing stateProvince
        rep_these <- !is.na(n4.1) & is.na(x1[["stateProvince"]][w_locality][no_state])
        if (any(rep_these)) {
          x1[["stateProvince"]][w_locality][no_state][rep_these] <-
            n4.1[rep_these]
        }
      }

      patt_yes <- "municipio|municipality|county|village|district"
      patt_no <- "municipios|municipalities|districts"
      if (any(no_munic)) {
        # trying to get missing counties from the locality (e.g. "Pico das Almas, municipio de Rio de Contas")
        n4.2 <- as.character(sapply(n4[no_munic], function(x)
          paste(unique(gsub("^ | $", "",
            x[grepl(patt_yes, x, perl = TRUE) & !grepl(patt_no, x, perl = TRUE)], perl = TRUE
          )), collapse = "|")))
        n4.2 <- squish(n4.2)
        n4.2 <- gsub("municipio de |municipality of |municipio do |^county of |village of|district of",
                     "",
                     n4.2, perl = TRUE)
        n4.2 <- gsub("municipio |municipality |^county |village |district ",
                     "",
                     n4.2, perl = TRUE)
        n4.2 <- squish(gsub(" municipio$| municipality$| county$| village$| district$",
                            "",
                            n4.2, perl = TRUE))
        n4.2[n4.2 %in% c("", "municipio", "municipality", "county",
                         "village","district")] <- NA

        # Replacing missing counties
        if (any(c("municipality", "county") %in% loc.levels)) {
          # priority 1: localities specifying a county name
          rep_these <- !is.na(n4.2) &
                          is.na(x1[["municipality"]][w_locality][no_munic])
          if (any(rep_these)) {
            x1[["municipality"]][w_locality][no_munic][rep_these] <-
              squish(n4.2[rep_these])
          }
        }
        # if (any(c("municipality", "county") %in% loc.levels)) {
        #   # priority 1: localities specifying a county name
        #   x1[, "municipality"][is.na(x1[, "municipality"]) & !is.na(n4.2)] <-
        #     gsub("^ | $", "", n4.2[is.na(x1[, "municipality"]) & !is.na(n4.2)], perl = TRUE)
        #   # priority 2: first part of the locality description
        #   x1[, "municipality"][is.na(x1[, "municipality"]) & is.na(n4.2)] <-
        #     gsub("^ | $", "", n4.2.1[is.na(x1[, "municipality"]) & is.na(n4.2)], perl = TRUE)
        # }
      }
    }

    # getting missing info in the first or last part of the locality description
    n4.2.1 <- as.character(sapply(n4, function(x) x[1]))
    n4.2.1 <- squish(n4.2.1)
    n4.2.1[n4.2.1 %in% empty_vec] <- NA
    rep_these <- grepl(patt_yes0, n4.2.1, perl = TRUE) &
                  !grepl(patt_no0, n4.2.1, perl = TRUE)
    if (any(rep_these))
      n4.2.1[rep_these] <-
        as.character(sapply(n4[rep_these], function(x) x[length(x)]))

    rep_these <- grepl(patt_yes0, n4.2.1, perl = TRUE) &
                  !grepl(patt_no0, n4.2.1, perl = TRUE)
    if (any(rep_these))
      n4.2.1[rep_these] <- NA

    # priority 2: other parts of the locality description
    rep_these <- !is.na(n4.2.1) &
                    is.na(x1[["municipality"]][w_locality])
    if (any(rep_these)) {
      x1[["municipality"]][w_locality][rep_these] <-
        squish(n4.2.1[rep_these])
    }

    # isolating localities possibily in the gazetter (e.g. parks, serras, farms)
    locais <- "parque|reserva|reserve|fazenda|nacional|estadual|parna|flona|rebio|rppn|e\\.e\\.|biologica|ecologica|extrativista|park|farm|hacienda|estrada|rodovia|carretera|road|camino|^sitio|^mata|^horto|^jardim|campus|^pico|^serra|^sierra|^morro|^chapada|^colina|^monumento"
    n4.3 <- as.character(sapply(n4, function(x)
      paste(unique(gsub("^ | $", "",
        x[grepl(locais, x, perl = TRUE)], perl = TRUE,
      )), collapse = ", ")))
    n4.3 <- squish(n4.3)
    n4.3[n4.3 %in% empty_vec] <- NA

    #other localities, than the ones in 'locais' and counties (first sentence, prior to the first comma when n4.3 is empty)
    check_these <- is.na(n4.3)
    if (!all(check_these)) {
      n4.3[check_these] <-
        squish(as.character(sapply(n4[check_these],
                                   function(x) utils::head(x[!grepl(patt_yes, x, perl = TRUE)], n = 1))))
      # n4.3[is.na(n4.3) & !is.na(x1[ ,"locality"])] <-
      #   as.character(sapply(n4[is.na(n4.3) & !is.na(x1[ ,"locality"])],
      #                       function(x) utils::head(x[!grepl(patt_yes, x, perl = TRUE)], n = 1)))

      # Replacing edited/missing localities
      rep_these <- !is.na(n4.3)
      if (any(rep_these)) {
        x1[["locality"]][w_locality][rep_these] <-
          n4.3[rep_these]
      }
      # if (any(!rep_these)) {
      #   x1[["locality"]][w_locality][!rep_these] <-
      #     as.character(sapply(n4[!rep_these], function(x) x[1]))
      # }
      # # Replacing edited/missing localities
      # x1[ ,"locality"][!is.na(n4.3)] <- n4.3[!is.na(n4.3)]
      # x1[ ,"locality"][is.na(n4.3)] <-
      #   as.character(sapply(n4[is.na(n4.3)], function(x) x[1]))
    }
  }

  ## Trimming and editing the edited columns
  for (i in 1:length(loc.levels))
    x1[, i] <- as.character(squish(x1[, i]))

  for (i in 1:length(loc.levels))
    x1[, i] <- gsub("^-$", NA, x1[, i], perl = TRUE)

  ## Resolution of the locality information provided
  tmp <- apply(x1[, loc.levels, drop = FALSE], 1, function(x)
            which(is.na(x))[1] - 1)
  if (any(tmp %in% 0))
    tmp[tmp %in% 0] <- length(loc.levels) + 1

  if (any(is.na(tmp)))
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
    if (c("locality.new") %in% names(x1) & scrap) {
      res$locality.scrap <- NA
      if (exists("n4.2.1")) {
        res$locality.scrap[w_locality] <- squish(n4.2.1)
      }
    }

    if (!to.lower) {
      names.res <- names(res)
      for(i in 1:length(names.res))
        res[, names.res[i]] <- stringr::str_to_title(res[, names.res[i]])
    }
    res$resol.orig <- resol.orig
  }

  ## Final edits
  check_these <- !is.na(res$locality.new) & !is.na(res$locality.scrap) &
                    res$locality.new == res$locality.scrap
  if (any(check_these))
    res$locality.scrap[check_these] <- NA

  ## Merging the results and returning
  result <- cbind.data.frame(x, res)
  return(result)
}
