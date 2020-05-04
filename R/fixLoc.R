#' @title Edit and Format Locality Information
#'
#' @description
#'
#' @param x a data.frame
#'
#'
fixLoc = function(x, adm.levels = c("country", "stateProvince", "municipality", "locality"), scrap = TRUE) {
  ##To decide: Include extra ADM level between country and states??? Regions or Departments?? see the case of Peru
  require(stringr)
  require(countrycode)

  ## check input:
  if (!class(x) == "data.frame") { stop("input object needs to be a data frame!") }
  if (any(names(x) %in% c("countryCode"))) { colnames(x)[which(colnames(x) == "countryCode")] = "country" }
  if (any(names(x) %in% c("county"))) { colnames(x)[which(colnames(x) == "county")] = "municipality" }
  if (!any(c("country","stateProvince","municipality","locality") %in% colnames(x))) { stop("input object needs to have at least one of the following fields: country/countryCode, stateProvince, county/municipality and locality") }

  ## Obtaining the intermediary data frame for editing
  if(length(adm.levels)==1) { x1 = x[which(colnames(x) %in% adm.levels)] } else { x1 = x[, which(colnames(x) %in% adm.levels)] }

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
  dic <- replace_names

  ## Loading the objects that will be used in the formatting process
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
  if(any(c("country", "countryCode") %in% adm.levels)) {
    # Converting any country codes into country names
    # x1[nchar(x1[ ,"country"]) == 2 & !is.na(x1[ ,"country"]) ,"country"] =
    #   countrycode(as.character(x1[nchar(x1[ ,"country"]) == 2 & !is.na(x1[ ,"country"]) ,"country"]), 'iso2c', 'country.name')
    # x1[nchar(x1[ ,"country"]) == 3 & !is.na(x1[ ,"country"]) ,"country"] =
    #   countrycode(as.character(x1[nchar(x1[ ,"country"]) == 3 & !is.na(x1[ ,"country"]) ,"country"]), 'iso3c', 'country.name')
    # Removing unwanted characters
    x1[ ,"country"] = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), x1[ ,"country"]))
    # Replacing '&' by 'and' in compound country names
    x1[ ,"country"] = str_replace_all(x1[ ,"country"], " & ", " and ")
    # Replacing missing info by NA
    pattern = paste(missLocs, collapse = "|")
    x1[ ,"country"] = gsub(pattern, NA, x1[ ,"country"], perl = TRUE)
    x1[ ,"country"][grepl("desconhecid|unknown", x1[, "country"])] = NA
    # Replacing variants, abbreviations, typos, and non-standard names
    tmp1 = dic[dic$class %in% "country" & apply(is.na(dic[,2:4]), 1, all),]
    tmp2 = tmp1$replace; names(tmp2) = tmp1$pattern
    names(tmp2) = gsub("\\\\", "", names(tmp2))
    x1[ ,"country"] = str_replace_all(x1[ ,"country"], tmp2)
    # Missing country for non missing states and counties (only for uniquivocal states)
    tmp1 = dic[dic$class %in% "country" & dic$condition2 %in% "not_is.na",]
    reps = unique(tmp1$replace)
    if(all(c("stateProvince", "municipality") %in% names(x1)) & any(reps %in% unique(x1[ ,"country"]))) {
      reps1 = reps[reps %in% unique(x1[, "country"])]
      for(i in 1:length(reps)) x1[is.na(x1[ ,"country"]) & !is.na(x1[, "municipality"]) &
                                    x1[ ,"stateProvince"] %in% tmp1$condition1[tmp1$replace %in% reps1[i]], "country"] = reps1[i]
    }
  }

  ## ADM1: State/Province level ##
  if("stateProvince" %in% adm.levels) {
    # Removing unwanted characters
    x1[ ,"stateProvince"] = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), x1[ ,"stateProvince"]))
    # Replacing missing info by NA
    pattern = paste(missLocs, collapse = "|")
    x1[ ,"stateProvince"] = gsub(pattern, NA, x1[ ,"stateProvince"], perl = TRUE)
    x1[ ,"stateProvince"][grepl("desconhecid|unknown", x1[, "stateProvince"])] = NA
    # Removing unwanted prefixes and abbreviations
    pattern <- paste(wordsForSearch, collapse = "|")
    x1[ ,"stateProvince"] = gsub(pattern, "", x1[ ,"stateProvince"], perl = TRUE)
    # Replacing variants, abbreviations, typos, and non-standard names
    tmp1 = dic[dic$class %in% "stateProvince" & !is.na(dic[ ,2]),]
    tmp2 = tmp1$replace; names(tmp2) = tmp1$pattern
    names(tmp2) = gsub('\\.',"\\\\.",names(tmp2))
    cond0 = unique(tmp1$condition0)
    if("country" %in% names(x1)) {
      if(any(cond0 %in% unique(x1[, "country"]))) {
        cond1 = cond0[cond0 %in% unique(x1[, "country"])]
        for(i in 1:length(cond1))  x1[x1[, "country"] %in% cond1[i] ,"stateProvince"] =
            str_replace_all(x1[x1[,"country"] %in% cond1[i] ,"stateProvince"], tmp2)
      }
    }
  }

  ## ADM2: County, Departament, Commune
  if(any(c("municipality","county") %in% adm.levels)) {
    # Removing unwanted characters and replacing missing info by NA
    x1[ ,"municipality"] = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), x1[ ,"municipality"]))
    # Replacing missing info by NA
    pattern = paste(missLocs, collapse = "|")
    x1[ ,"municipality"] = gsub(pattern, NA, x1[ ,"municipality"], perl = TRUE)
    x1[ ,"municipality"][grepl("desconhecid|unknown", x1[, "municipality"])] = NA
    # Removing unwanted prefixes and abbreviations
    tmp1 = dic[dic$class %in% "county" & apply(is.na(dic[,2:4]), 1, all),]
    tmp2 = tmp1$replace; names(tmp2) = tmp1$pattern
    names(tmp2) = gsub('\\.',"\\\\.",names(tmp2))
    x1[ ,"municipality"] = str_replace_all(x1[ ,"municipality"], tmp2)
  }

  ## ADM3: locality (park, farm, etc.)
  if(any(c("locality") %in% adm.levels)) {
    # Removing unwanted characters
    x1[ ,"locality"] = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), x1[ ,"locality"]))
    # Removing unwanted prefixes and abbreviations (1st round)
    tmp1 = dic[dic$class %in% "locality1" & apply(is.na(dic[,2:4]), 1, all),]
    tmp2 = tmp1$replace; names(tmp2) = tmp1$pattern
    names(tmp2) = gsub('\\.',"\\\\.", names(tmp2))
    names(tmp2) = gsub('\\(',"\\\\(", names(tmp2))
    names(tmp2) = gsub('\\)',"\\\\)", names(tmp2))
    x1[ ,"locality"] = str_replace_all(x1[ ,"locality"], tmp2)
    # solving some substitution problems
    x1[ ,"locality"] = gsub(" de de | de of ", " de ", x1[ ,"locality"], perl = TRUE)
    x1[ ,"locality"] = gsub(" de do ",	" do ", x1[ ,"locality"], perl = TRUE)
    # Removing unwanted prefixes and abbreviations (2nd round)
    tmp1 = dic[dic$class %in% "locality2" & apply(is.na(dic[,2:4]), 1, all),]
    tmp2 = tmp1$replace; names(tmp2) = tmp1$pattern
    names(tmp2) = gsub('\\.',"\\\\.", names(tmp2))
    names(tmp2) = gsub('\\(',"\\\\(", names(tmp2))
    names(tmp2) = gsub('\\)',"\\\\)", names(tmp2))
    x1[ ,"locality"] = str_replace_all(x1[ ,"locality"], tmp2)
  }

  if(c("locality") %in% names(x1) & scrap == TRUE) {
    # Spliting the locality vector to find missing information
    n4 = strsplit(x1[ ,"locality"], ",|\\.|:|\\(|)|;")
    n4 = sapply(n4, str_trim)
    # trying to get missing counties from the locality description (e.g. "Pico das Almas, municipio de Rio de Contas")
    n4.2 = as.character(sapply(n4, function(x) paste(unique(str_trim(x[grepl("municipio|municipality|county|provincia|village",x)])), collapse = "|")))
    n4.2 = str_trim(n4.2)
    n4.2 = gsub("municipio de |municipality of |municipio do |^county of |^provincia de|^provincia of|village of", "", n4.2)
    n4.2 = gsub("municipio |municipality |^county |^provincia |village ", "", n4.2)
    n4.2[n4.2 %in% ""] = NA
    # getting missing counties that may be the first part of the locality description (e.g. "Rio de Contas, Pico das Almas")
    n4.2.1 = as.character(sapply(n4, function(x) x[1]))
    n4.2.1 = str_trim(n4.2.1)
    n4.2.1[n4.2.1 %in% ""] = NA
    # isolating localities possibily in the gazetter (e.g. parks, serras, farms)
    locais = "parque|reserva|reserve|fazenda|nacional|estadual|parna|flona|rebio|rppn|e\\.e\\.|biologica|ecologica|extrativista|park|farm|estrada|rodovia|road|^sitio|^mata|^horto|^jardim|campus|^pico|^serra|^sierra|^morro|^chapada"
    n4.3 = as.character(sapply(n4, function(x) paste(unique(str_trim(x[grepl(locais,x)])), collapse = ", ")))
    n4.3 = str_trim(n4.3)
    n4.3[n4.3 %in% ""] = NA
    #other localities, than the ones in 'locais' and counties (first sentence, prior to the first comma when n4.3 is empty)
    n4.3[is.na(n4.3) & !is.na(x1[ ,"locality"])] = as.character(sapply(n4[is.na(n4.3) & !is.na(x1[ ,"locality"])],
                                                                       function(x) head(x[!grepl("municipio|municipality|county|provincia|village",x)],1)))
    # Replacing missing counties
    if(any(c("municipality","county") %in% adm.levels)) {
      # priority 1: localities specifying a county name
      x1[ ,"municipality"][is.na(x1[ ,"municipality"]) & !is.na(n4.2)] =
        str_trim(n4.2[is.na(x1[ ,"municipality"]) & !is.na(n4.2)])
      # priority 2: first part of the locality description
      x1[ ,"municipality"][is.na(x1[ ,"municipality"]) & is.na(n4.2)] =
        str_trim(n4.2.1[is.na(x1[ ,"municipality"]) & is.na(n4.2)])
    }
    # Replacing edited/missing localities
    x1[ ,"locality"][!is.na(n4.3)] = n4.3[!is.na(n4.3)]
    x1[ ,"locality"][is.na(n4.3)] = as.character(sapply(n4[is.na(n4.3)], function(x) x[1]))
  }

  ## Trimming and editing the edited columns
  for (i in 1:length(adm.levels)) x1[ ,i] = as.character(str_trim(x1[ ,i]))
  for (i in 1:length(adm.levels)) x1[ ,i] = gsub("^-$", NA, x1[ ,i])

  # Resolution of the locality information provided
  tmp = apply(x1[,adm.levels], 1, function(x) which(is.na(x))[1]-1)
  tmp[tmp %in% 0] = length(adm.levels) + 1
  tmp[is.na(tmp)] = length(adm.levels)
  resol.orig = c(adm.levels,"no_info")[tmp]

  # Preparing the output
  if(length(adm.levels)==1) {
    res = as.vector(x1)
  } else {
    names(x1) = paste0(names(x1),".new")
    res = as.data.frame(x1)
    if(c("locality.new") %in% names(x1) & scrap == TRUE) res$locality.scrap = n4.2.1
    res$resol.orig = resol.orig
  }
  return(res)
}
