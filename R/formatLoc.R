#' @title Format and Find Specimen Locality Information
#'
#' @description
#'
#' @param x Character
#'
formatLoc = function(x) {
  ## check input:
   if (!class(x) == "data.frame") { stop("input object needs to be a data frame!") }
   if (any(names(x) %in% c("countryCode"))) { colnames(x)[which(colnames(x) == "countryCode")] = "country" }
   if (any(names(x) %in% c("county"))) { colnames(x)[which(colnames(x) == "county")] = "municipality" }
   if (!all(c("country","stateProvince","municipality","locality") %in% colnames(x))) { stop("input object needs to have the following fields: country/countryCode, stateProvince, county/municipality and locality") }

  ## Loading the gazetter, with missing counties and ortographical variants of some names (not exaustive)
   dic = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//gazetteer.csv",as.is=TRUE)

  ##Check: Include extra ADM level between country and states??? Regions or Departments??

  ## Obtaining the intermediary data frame for editing
   x1 = x[, which(colnames(x) %in% c("country", "stateProvince", "municipality", "locality"))]

  ## Solving some common encoding problems
   x1[] <- lapply(x1, gsub, pattern = "ã¡|ã¢|ã£", replacement = "a", ignore.case = TRUE, perl = TRUE)
   x1[] <- lapply(x1, gsub, pattern = "ã©|ãª", replacement = "e", ignore.case = TRUE, perl = TRUE)
   x1[] <- lapply(x1, gsub, pattern = "ã§", replacement = "c", ignore.case = TRUE, perl = TRUE)
   x1[] <- lapply(x1, gsub, pattern = "ãº", replacement = "u", ignore.case = TRUE, perl = TRUE)
   x1[] <- lapply(x1, gsub, pattern = "ã´", replacement = "o", ignore.case = TRUE, perl = TRUE)
   x1[] <- lapply(x1, gsub, pattern = "ã\u008d", replacement = "i", ignore.case = TRUE, perl = TRUE)
   x1[] <- lapply(x1, gsub, pattern = "\u00AD", replacement = "", perl = TRUE) # soft hyphen
   x1[] <- lapply(x1, gsub, pattern = "\u00AO", replacement = "", perl = TRUE) # hidden breaking space

  ## ADM0: Country level
   # Converting any country codes into country names
     x1[nchar(x1[ ,1]) == 2 & !is.na(x1[ ,1]) ,1] = countrycode(as.character(x1[nchar(x1[ ,1]) == 2 & !is.na(x1[ ,1]) ,1]), 'iso2c', 'country.name')
     x1[nchar(x1[ ,1]) == 3 & !is.na(x1[ ,1]) ,1] = countrycode(as.character(x1[nchar(x1[ ,1]) == 3 & !is.na(x1[ ,1]) ,1]), 'iso3c', 'country.name')
   # Removing unwanted characters
     unwanted_array = list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                           'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                           'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='S', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                           'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                           'ö'='o', 'ø'='o', 'ü'='u', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )
     x1[ ,1] = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), x1[ ,1]))
   # Replacing variants, abbreviations, typos, and non-standard names
     tmp = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//replace_names.csv",as.is=TRUE,fileEncoding="UTF-8-BOM")
     tmp1 = tmp[tmp$class %in% "country" & apply(is.na(tmp[,2:4]), 1, all),]
     tmp2 = tmp1$replace; names(tmp2) = tmp1$pattern
     x1[ ,1] = str_replace_all(x1[ ,1], tmp2)
    # Missing country for non missing states and counties (only for uniquivocal Brazilian states)
     x1[is.na(x1[,1]) & !is.na(x1[,3]) & x1[,2] %in%
          c("Acre","Alagoas","Amapá","Bahia","Ceará","Espírito Santo","Goiás","Maranhão","Minas Gerais","Mato Grosso do Sul","Pará","Paraíba",
            "Pernambuco","Piauí","Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","Sergipe","São Paulo","Tocantins"
          )] = "brazil"

  #### CONTINUAR DAQUI ####

  ## Replacing missing info by NA
   missLocs = c("^\\?$","^s\\/localidade","^indeterminada$","^indeterminado$","^s\\.d\\.$","^desconhecido$","^sin loc\\.$","^sin\\. loc\\.$",
                "^ignorado$","^sem informacao$","^n\\.i\\.","^nao especificado$","^nao informado$","^bloqueado$",
                "no locality information available","^protected due to name conservation status","^completar datos","^no disponible$","^not available$")
   pattern <- paste(missLocs, collapse = "|")
   x1[ ,1] <- gsub(pattern, NA, x1[ ,1], perl = TRUE)




  ## Extracting the locality fields
   n0 = x[, which(colnames(x) == "country")] #country
   n1 = x[, which(colnames(x) == "stateProvince")] #state, province, etc.
   n2 = x[, which(colnames(x) == "municipality")] #county, departamiento, commune, etc.
   n3 = x[, which(colnames(x) == "locality")] #locality: park, farm, etc.

  ## Nivel0: Country
  ## Converting any country codes into country names
   n0[nchar(n0)==2 & !is.na(n0)] = countrycode(as.character(n0[nchar(n0)==2 & !is.na(n0)]), 'iso2c', 'country.name')
   n0[nchar(n0)==3 & !is.na(n0)] = countrycode(as.character(n0[nchar(n0)==3 & !is.na(n0)]), 'iso3c', 'country.name')
   #Removing unwanted characters and replacing missing info by NA
   unwanted_array = list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                         'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                         'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='S', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                         'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                         'ö'='o', 'ø'='o', 'ü'='u', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )
   n0 = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), n0))
   missLocs = c("^\\?$","s\\/localidade","inderminada","inderminado","^s\\.d\\.","^desconhecido","sin loc\\.","sin\\. loc\\.",
                "^ignorado","^sem informacao","n\\.i\\.","nao especificado","^não informado",
                "no locality information available","protected due to name conservation status","completar datos","no disponible")
   pattern <- paste(missLocs, collapse = "|")
   ids = apply(x[,c("country","stateProvince","municipality","locality")], 2, function(i) grep(pattern, i))

   ids = lapply(c("country","stateProvince","municipality","locality"), function(i) {
     id.col = grep(i,colnames(x))
     id.row = grep(pattern, x[,id.col])
     as.matrix(data.frame(id.row,id.col))})
   n0 <- gsub(pattern, NA, n0, perl = TRUE)
   #Replacing some typos and non-english coutry names

  ## Nivel1: States, Provinces
   #Removing unwanted characters and replacing missing info by NA
   n1 = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), n1))
   pattern <- paste(c(missLocs, "estado nao informado!"), collapse = "|")
   n1 <- gsub(pattern, NA, n1, perl = TRUE)
   #Removing unwanted prefixes and abbreviations
   wordsForSearch = c("^prov\\. ","^dep\\. ","^depto\\. ","^prov\\.","^mun\\. ","^dept\\.","^dpto\\.","^depto\\.","^dept.",
                      "^departamento ","^departamento de ","^provincia de ","^província de ")
   pattern <- paste(wordsForSearch, collapse = "|")
   n1 <- gsub(pattern, "", n1, perl = TRUE)
   n1 = str_trim(gsub("\\.$","",n1))

  ## Nivel2: County, Departament, Commune
   #Removing unwanted characters and replacing missing info by NA
   n2 = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), n2))
   pattern <- paste0("\\b(?:", paste(missLocs, collapse = "|"), ")\\b ?")
   n2 <- gsub(pattern, NA, n2, perl = TRUE)
   #Replacing some abbreviations by the full locality names
   n2 = gsub("^gral\\.", "general",n2)
   n2 = gsub("^rod\\. ", "rodovia",n2)
   n2 = gsub("sma\\.", "santissima",n2)
   n2 = gsub("st\\. antonio", "santo antonio",n2)
   n2 = gsub("s\\.jose", "sao jose",n2)
   n2 = gsub("sta\\. elena", "santa elena",n2)
   n2 = gsub("sta\\. rita", "santa rita",n2)
   n2 = gsub("s\\. jose", "sao jose",n2)
   n2 = gsub("s\\. joao", "sao joao",n2)
   n2 = gsub("s\\. bento|s\\.bento", "sao bento",n2)
   n2 = gsub("s\\. luis", "sao luis",n2)
   n2 = gsub("s\\. miguel", "sao miguel",n2)
   n2 = gsub("s\\. f\\. paula", "sao francisco paula",n2)
   n2 = gsub("nª sª|nossa sra\\.", "nossa senhora",n2)
   n2 = gsub(" s\\. luiz", " sao luiz",n2)
   n2 = gsub(" s\\. mateus", " sao mateus",n2)
   n2 = gsub(" s\\. francisco", " sao francisco",n2)
   n2 = gsub(" dr\\. ", " doutor ",n2)
   n2 = gsub("sta\\. maria", "santa maria",n2)
   n2 = gsub('\\.$', '', n2)
   n2 = str_trim(n2)

  ## Nivel3: locality (park, farm, etc.)
   n3 = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), n3))
   n3 = gsub("\\?|s\\/localidade|inderminada|inderminado|^s\\.d\\.|^desconhecido|^ignorado|bloqueado|^sem informacao|n\\.i\\.|Sin loc\\.|Sin\\. loc\\.",NA,n3)
   n3 = gsub("nao especificado|^não informado|^no specific\\.$|^no specific$",NA,n3)

  ## Spliting the locality vector to find missing information
   n4 = strsplit(n3, ",|\\.|:|\\(|)|;")
   n4 = sapply(n4, str_trim)
   # trying to get missing counties from the locality description (e.g. "Pico das Almas, municipio de Rio de Contas")
     n4.2 = as.character(sapply(n4, function(x) paste(unique(str_trim(x[grepl("municipio|municipality|county|provincia|village",x)])),collapse = "|")))
     n4.2 = str_trim(n4.2)
     n4.2 = gsub("municipio de |municipality of |municipio do |^county of |^provincia de|^provincia of|village of","",n4.2)
     n4.2 = gsub("municipio |municipality |municipio |^county |^provincia |village ","",n4.2)
     n4.2[n4.2 %in% ""] = NA
   # getting missing counties that may be the first part of the locality description (e.g. "Rio de Contas, Pico das Almas")
     n4.2.1 = as.character(sapply(n4, function(x) x[1]))
     n4.2.1 = str_trim(n4.2.1)
     n4.2.1[n4.2.1 %in% ""] = NA
   # isolating localities possibily in the gazetter (e.g. parks, serras, farms)
     locais = "parque|reserva|reserve|fazenda|nacional|estadual|parna|flona|rebio|rppn|e\\.e\\.|biologica|ecologica|extrativista|park|farm|estrada|rodovia|road|^sitio|^mata|^horto|^jardim|campus|^pico|^serra|^sierra|^morro|^chapada"
     n4.3 = as.character(sapply(n4, function(x) paste(unique(str_trim(x[grepl(locais,x)])),collapse = ", ")))
     n4.3 = str_trim(n4.3)
     n4.3[n4.3 %in% ""] = NA
   #other localities, than the ones in 'locais' and counties (first sentence, prior to the first comma when n4.3 is empty)
     n4.3[is.na(n4.3)&!is.na(n3)] = as.character(sapply(n4[is.na(n4.3)&!is.na(n3)],function(x) head(x[!grepl("municipio|municipality|county|provincia|village",x)],1)))
   ## Replacing missing counties and localities
     n2[is.na(n2) & !is.na(n4.2)] = str_trim(n4.2[is.na(n2) & !is.na(n4.2)])
     n3[!is.na(n4.3)] = str_trim(n4.3[!is.na(n4.3)])
     n3[is.na(n4.3)] = str_trim(as.character(sapply(n4[is.na(n4.3)], function(x) x[1])))


  return(loc)
}
#
#### 1 - Finding and editing locality data - speciesLink, speciesLink_new and JABOT ####
#
#### PASTE HERE AFTER FINISHING TO CORRECT ONE OF THE STEPS OF THE FUNCTION:
## Updating the columns 'af.check' and 'AtlanticForest_Lei11428' for the municipalities extracted manually
#
#
