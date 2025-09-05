# Script to read from data-raw/dictionaries and generate sysdata

# loading packages
library(stringr)
library(readr)
library(dplyr)

# Reading processed files
dic_files <- list.files(path = "./data-raw/dictionaries",
                        pattern = "csv",
                        full.names = TRUE)

#nome das tabelas
data_names <- basename(dic_files) %>%
  stringr::str_split(., "[:punct:]", simplify = TRUE) %>%
  data.frame() %>%
  select(1) %>%
  pull()

#guess encoding
loc_list <- purrr::map(dic_files,
                       ~readr::guess_encoding(.))

loc_list <- c("UTF-8",
              "UTF-8",
              "UTF-8",
              "UTF-8", #ASCII?
              "UTF-8",
              "UTF-8",
              # "ASCII",
              # "ASCII",
              "UTF-8")
#Renato: código abaixo estava retornando um erro, por conta dos "ASCII"
#troquei por "UTF-8" no `loc_list` e o erro sumiu.
#Mas não pq estava "ASCII" então, checar...

dic <- purrr::map2(.x = dic_files,
                   .y = loc_list,
                   ~read_csv(file = .x,
                             guess_max = 30000,#this has to be large
                             locale = locale(encoding = .y)))

encoding <- "UTF-8"
dic <- lapply(dic_files,
              read_csv,
              trim_ws = FALSE,
              guess_max = 30000,#this has to be large
              locale = locale(encoding = encoding))

names(dic) <- data_names
lapply(dic, nrow)
#new dims!
# gazetteer from 34807 to 23436 (July 2020); 23789 (Apr 2025)
#taxonomists from 9297 to 8518 (July 2020); 8715 (Apr 2025)

# transforma em data.frame
dic <- lapply(dic, as.data.frame)

### create existing named objects
admin <- dic$admin
collectionCodes <- dic$collectionCodes
familiesSynonyms <- dic$familiesSynonyms
fieldNames <- dic$fieldNames
gazetteer <- dic$gazetteer
replaceNames <- dic$replaceNames
taxonomists <- dic$taxonomists

### Creating other objects necessary fot the data processing and validation
missLocs <- c("^\\?$",
              "^s\\/localidade",
              "^indeterminada$",
              "^indeterminado$",
              "^s\\.d\\.$",
              "^desconhecido$",
              "^sin loc\\.$",
              "^sin\\. loc\\.$",
              "^ignorado$",
              "^sem informacao$",
              "^n\\.i\\.",
              "^nao especificado$",
              "^nao informado$",
              "^bloqueado$",
              "no locality information available",
              "^protected due to name conservation status",
              "^completar datos",
              "^no disponible$",
              "^not available$",
              "^loc\\.ign$",
              "local ignorado",
              "^indisponivel$")

wordsForSearch <- c("^prov\\. ",
                    "^dep\\. ",
                    "^depto\\. ",
                    "^prov\\.",
                    "^dept\\.",
                    "^dpto\\.",
                    "^depto\\.",
                    "^dept\\.",
                    "^dist\\. de ",
                    "^dist\\. ",
                    " dist\\.$",
                    "^municipio de ",
                    "^municipio del ",
                    "^municipality of ",
                    "^municipio ",
                    "^municipio ",
                    "^municipality ",
                    "^mun\\. ",
                    # "^mun ",
                    " mun\\.$",
                    " municipio$",
                    " municipality$",
                    "^departamento de ",
                    "^departamento del ",
                    "^departament of ",
                    "^departamiento de ",
                    "^departemento del ",
                    "^departamento ",
                    "^provincia de ",
                    "^provincia del ",
                    "^província de ",
                    "^província: ",
                    "^província ",
                    "^province of ",
                    " province$",
                    "^estado do ",
                    "^estado de ",
                    "^estado: ",
                    "^estado ")

unwanted_latin <- c('À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Æ',
                    'Ç', 'È', 'É', 'Ê', 'Ë',
                    'Ì', 'Í', 'Î', 'Ï',
                    'Ñ', 'Ò', 'Ó', 'Ô', 'Õ', 'Ö', 'Ø',
                    'Ù', 'Ú', 'Û', 'Ü', 'Ý', 'Þ', 'ß',
                    'à', 'á', 'â', 'ã', 'ä', 'å', 'æ',
                    'ç', 'è', 'é', 'ê', 'ë',
                    'ì', 'í', 'î', 'ï',
                    'ð', 'ñ', 'ò', 'ó', 'ô', 'õ', 'ö', 'ø',
                    'ü', 'ù', 'ú', 'û', 'ý', 'þ', 'ÿ',
                    'Ŀ', 'ŀ', 'Ŋ', 'ŋ',
                    'Œ', 'œ', 'Š', 'š', 'Ÿ', 'Ž', 'ž')
unwantedLatin <- textclean::replace_non_ascii(unwanted_latin)
names(unwantedLatin) <- unwanted_latin

unwantedEncoding <- c('ã¡' = 'a',
                      'ã¢' = 'a',
                      'ã£' = 'a',
                      '&#225;' = 'a',
                      'ã§' = 'c',
                      'ã©' = 'e',
                      'ãª' = 'e',
                      'ã´' = 'o',
                      'ã\u008d' = 'i',
                      'ãº' = 'u')
Encoding(names(unwantedEncoding)) <- "UTF-8"
names(unwantedEncoding) <- iconv(names(unwantedEncoding), "UTF-8", "UTF-8")

cultivated <- c("cultivated", "cultivada", "cultivado", "cultivato", "cultivad",
                "under cultivation",
                "exotic", "exótica",
                "plantada", "plantado", "planted",
                "plantio",
                "arboreto", "arboretum", "pomar",
                #"canteiro",
                "área de visitação",
                "cult\\.",
                "cant\\. [a-z]", "cant [A-Z]", "cant\\. [0-9]",
                "cant \\. [0-9]", "cant [0-9]",
                "\\(cult\\)", "\\(cult \\)",
                "in cultivo", "in cultis",
                " quadra [a-z]", "quadra [a-z] do",
                "naturalised",
                "em experimento de")

notCultivated <- c("nativa",
                   "espontânea",
                   "pastagem cultivada",
                   "área do arboreto",
                   "presença de exóticas",
                   " área cultivada", " cultivated area")

missColls <- c("s/col.", "s/col", "s/c",
               "s/coletor", "s/colector", "s.coletor",
               " sem col.", "s.col.", "s.c.", "s.n.",
               "sem informação", "sem informacao",
               "collector unspecified", "collector unknown", "unknown",
               "disponible, n.", "disponivel, n.", "available, n.",
               "informação, s.", "informacao, s.",
               "unspecified, c.",
               "unknown, c.",
               "sin",
               "?")

missDets <- c("s/det.", "s/det", "s/d",
              "s/determinador", "s.determinador",
              " sem det.", "s.det.",
              "s.n.",
              "sem informação", "sem informacao",
              "determiner unspecified", "determiner unknown",
              "unknown",
              "disponible, n.", "disponivel, n.", "available, n.",
              "informação, s.", "informacao, s.",
              "unspecified, d.",
              "unknown, d.",
              "sin",
              "?")


treatPreps <- c("Dr.", "Dra.", "Pe.", "Prof.", "Profa.",
                "Sr.", "Sra.", "Mr.", "Mrs.", "Mme.")

namePreps <- c("De", "Dos", "Do", "Da", "Das",
               "Del", "Du", "Des",
               "Di", "Dalla", "Della", "Ter", "Von",
               "Van", "De La", "De Las", "De Lo", "De Los",
               "Van Der", "Van Den")

#### ver se todas as oções estão aqui: https://pt.stackoverflow.com/questions/242948/validar-nome-e-sobrenome-com-express%C3%A3o-regular ####
#Opções de preposições que não estão acima (incluir?): "e", "y", "bin", "le"

badEncoding <- c("Ã€", "Ã‚", "Ãƒ", "Ã„", "Ã…", "Ã†", "Ã‡", "Ãˆ", "Ã‰",
                 "ÃŠ", "Ã‹", "ÃŒ", "ÃŽ", "Ã‘", "Ã’", "Ã“", "Ã”",
                 "Ã•", "Ã–", "Ã—", "Ã˜", "Ã™", "Ãš", "Ã›", "Ãœ", "Ãž", "ÃŸ",
                 "Ã¡", "Ã¢", "Ã£", "Ã¤", "Ã¥", "Ã¦", "Ã§", "Ã¨", "Ã©", "Ãª",
                 "Ã«", "Ã¬", "Ã®", "Ã¯", "Ã°", "Ã±", "Ã²", "Ã³", "Ã´", "Ãµ",
                 "Ã¶", "Ã¸", "Ã¹", "Ãº", "Ã»", "Ã¼", "Ã½", "Ã¾", "Ã¿", "Ã­")
names(badEncoding) <- c("À", "Â", "Ã", "Ä", "Å", "Æ", "Ç", "È", "É", "Ê", "Ë",
                        "Ì", "Î", "Ñ", "Ò", "Ó", "Ô", "Õ", "Ö", "×", "Ø",
                        "Ù", "Ú", "Û", "Ü", "Þ", "ß", "á", "â", "ã", "ä", "å",
                        "æ", "ç", "è", "é", "ê", "ë", "ì", "î", "ï", "ð", "ñ", "ò",
                        "ó", "ô", "õ", "ö", "ø", "ù", "ú", "û", "ü", "ý", "þ", "ÿ", "í")
Encoding(names(badEncoding)) <- "UTF-8"
names(badEncoding) <- iconv(names(badEncoding), "UTF-8", "UTF-8")

## List of most common authors
library(plantRdata)
data("wfoNames")
toto <- wfoNames$tax.authorship
toto1 <- toto[!grepl("\\.|\\(|&", toto) &
                !is.na(toto) & !toto %in% ""]
corte <- 70 # aprox. autores com mais de 1000 spp descritas
toto2 <- names(tail(sort(table(toto1)), corte))
spp_or_lower <- c("genus", "species", "form", "subspecies", "variety",
                  "subvariety","subform")
epitetos <- wfoNames$tax.name[wfoNames$taxon.rank %in% spp_or_lower]
epitetos <-
  unique(sapply(strsplit(epitetos, " ", fixed = TRUE), tail, 1))
# wfoNames[grepl("Ching$", wfoNames$tax.name), ]
# wfoNames[grepl("Hayata$", wfoNames$tax.name), ]
commonAuthors <- tolower(toto2[!toto2 %in% epitetos])

toto <- bfoNames$tax.authorship
toto1 <- toto[!grepl("\\.|\\(|&", toto) &
                !is.na(toto) & !toto %in% ""]
corte <- 58 # aprox. autores com mais de 100 spp descritas
toto2 <- names(tail(sort(table(toto1)), corte))
commonAuthors_bfo <- tolower(toto2[!toto2 %in% epitetos])

commonAuthors <- sort(unique(c(commonAuthors, rmLatin(commonAuthors),
                               commonAuthors_bfo, rmLatin(commonAuthors_bfo))))
rm(wfoNames, epitetos, toto, toto1, toto2)

## Named vector with plantR reserved column names
reservedColNames <- c(
  # "format.occcs" - 10 columns
  "collectionCode.new", "collectionObs", "recordedBy.new",
  "recordNumber.new", "year.new", "identifiedBy.new",
  "yearIdentified.new", "recordedBy.aux", "identifiedBy.aux",
  "last.name",
  # "format.locs" - 11 columns
  "country.new", "stateProvince.new", "municipality.new",
  "locality.new", "locality.scrap", "resol.orig",
  "loc", "loc.correct", "latitude.gazetteer",
  "longitude.gazetteer","resolution.gazetteer",
  # "format.coords" - 5 columns
  "coord.check", "decimalLatitude.new", "decimalLongitude.new",
  "origin.coord", "precision.coord",
  # "format.tax" - 13 columns
  "scientificName.new", "scientificNameAuthorship.new",
  "scientificNameStatus", "suggestedFamily",
  "suggestedName", "suggestedAuthorship",
  "tax.name", "tax.authorship", "taxon.rank", "tax.notes",
  "id", "scientificNameFull", "family.new",
  # "validate.locs" - 1 column
  "loc.check",
  # "validate.coords" - 8 columns
  "geo.check", "cult.check", "out.check", "dist.check", "dist.check.obs",
  "NAME_0", "NAME_1", "NAME_2",
  # "validate.tax" - 1 columns
  "tax.check",
  # "validate.dups" - 22 columns
  "numTombo", "dup.ID", "dup.numb", "dup.prop",
  "family.new1", "scientificName.new1", "scientificNameAuthorship.new1",
  "identifiedBy.new1", "yearIdentified.new1", "tax.check1",
  "taxon.rank1", "scientificNameStatus1", "id1",
  "ref.spec.tax", "decimalLatitude.new1", "decimalLongitude.new1",
  "origin.coord1", "precision.coord1", "geo.check1", "ref.spec.geo",
  "loc.correct1", "resolution.gazetteer1", "loc.check1", "ref.spec.loc"
)
groupNames <- rep(c("format.occs", "format.locs", "format.coords",
                    "format.tax", "validate.locs", "validate.coords",
                    "validate.tax", "validate.dups"),
                  times = c(10, 11, 5, 13, 1, 8, 1, 24))
reservedColNames <- setNames(reservedColNames, groupNames)

## Conversion of coordinate validation categories
simpGeoCheck <- c(
  #common cases
  "ok_country/ok_state/ok_county" = "ok_county", # ok!
  "ok_country/ok_state/bad_county" = "ok_state", # ok!
  "ok_country/ok_state/no_county" = "ok_state", # ok?
  "ok_country/bad_state/ok_county" = "ok_county", # ok??? Discuss
  "ok_country/bad_state/bad_county" = "ok_country", # ok!
  "ok_country/bad_state/no_county" = "ok_country", # ok?
  "ok_country/no_state/no_county" = "ok_country", # ok?
  "bad_country/bad_state/bad_county" = "bad_country", # ok!
  "bad_country/bad_state/no_county" = "bad_country", # ok?
  "bad_country/no_state/no_county" = "bad_country", # ok?
  "no_country/no_state/no_county" = "check_gazetteer", # ok!
  #rare cases
  "ok_country/no_state/ok_county" = "ok_county", # ok?
  "ok_country/no_state/bad_county" = "ok_country", # ok?
  "bad_country/ok_state/ok_county" = "ok_county", # ok?
  "bad_country/ok_state/bad_county" = "ok_state", # or bad_country?
  "bad_country/ok_state/no_county" = "ok_state", # or bad_country?
  "bad_country/bad_state/ok_county" = "bad_country", # do not trust this one
  "bad_country/no_state/bad_county" = "bad_country", # ok?
  "bad_country/no_state/ok_county" = "bad_country", # do not trust this one
  "no_country/ok_state/ok_county" = "ok_county", # ok?
  "no_country/ok_state/bad_county" = "ok_state", # or bad_country?
  "no_country/ok_state/no_county" = "ok_state", # or bad_country?
  "no_country/bad_state/ok_county" = "bad_country", # do not trust this one. Or no_cannot_check?
  "no_country/bad_state/bad_county" = "bad_country", # or no_cannot_check?
  "no_country/no_state/ok_county" = "bad_country", # do not trust this one. Or no_cannot_check?,
  "no_country/no_state/bad_county" = "bad_country", # or no_cannot_check?,
  "no_country/bad_state/no_county" = "bad_country") # or bad_state or no_cannot_check?


## Conversion table for the TDWG Botanical Countries
# Download botanical countries (level3)
# url1 <- "https://github.com/tdwg/wgsrpd/refs/master/109-488-1-ED/2nd%20Edition/tblLevel3.txt"
url1 <- "https://raw.githubusercontent.com/tdwg/wgsrpd/refs/heads/master/109-488-1-ED/2nd%20Edition/tblLevel3.txt"
zip <- paste0("wcvp", ".zip")
path <- file.path(here::here(), "data-raw", zip)
path1 <- gsub("\\.zip", "_dist.txt", path)
utils::download.file(url = url1, destfile = path1, mode = "wb")
level3 <- read.table(path1, sep = "*", fileEncoding = "Latin1",
                     header = TRUE, stringsAsFactors = FALSE,
                     quote = "", fill = TRUE)
unlink(path1)
# Download subdivision of botanical countries (level4)
# url2 <- "https://github.com/tdwg/wgsrpd/raw/master/109-488-1-ED/2nd%20Edition/tblLevel4.txt"
url2 <- "https://github.com/tdwg/wgsrpd/raw/refs/heads/master/109-488-1-ED/2nd%20Edition/tblLevel4.txt"
path2 <- gsub("\\.zip", "_dist1.txt", path)
utils::download.file(url = url2, destfile = path2, mode = "wb")
level4 <- read.table(path2, sep = "*", fileEncoding = "Latin1",
                     header = TRUE, stringsAsFactors = FALSE,
                     quote = "", fill = TRUE)
unlink(path2)

# Merge both informations
level_all <- dplyr::left_join(level4, level3[, c("L3.code", "L3.area")],
                              by = c("L3.code"))
level_all1 <- aggregate(L4.code ~ L3.area + L4.country,
                        FUN = function(x) paste(unique(x), collapse = "|"),
                        data = level_all)
names(level_all1) <- c("taxon.distribution.bc",
                       "taxon.distribution.bru",
                       "taxon.distribution.bru.code")

# General edits
## Kirgizstan case
level_all1$taxon.distribution.bc <- gsub("Kirgizistan", "Kyrgyzstan",
                                         level_all1$taxon.distribution.bc)
level_all1$taxon.distribution.bru <- gsub("Kirgizistan", "Kyrgyzstan",
                                          level_all1$taxon.distribution.bru)
## Gambia case
level_all1$taxon.distribution.bc <- gsub("Gambia, The", "Gambia",
                                         level_all1$taxon.distribution.bc)
level_all1$taxon.distribution.bru <- gsub("Gambia, The", "Gambia",
                                          level_all1$taxon.distribution.bru)
## Brasília case
level_all1$taxon.distribution.bc <- gsub("^brazilia distrito federal$",
                                         "distrito federal",
                                         level_all1$taxon.distribution.bc)
level_all1$taxon.distribution.bru <- gsub("^brazilia distrito federal$",
                                          "distrito federal",
                                          level_all1$taxon.distribution.bru)
## Substitute 'i' by 'island' and 'is' by 'islands' to match plantR
level_all1$taxon.distribution.bc <-
  plantR::prepCountry(level_all1$taxon.distribution.bc)
level_all1$taxon.distribution.bru <-
  plantR::prepCountry(level_all1$taxon.distribution.bru)
level_all1$taxon.distribution.bc <-
  plantR::prepLoc(level_all1$taxon.distribution.bc)
level_all1$taxon.distribution.bru <-
  plantR::prepLoc(level_all1$taxon.distribution.bru)
# ## Edit the column to match wvcvp names exactly, that is, nchar max = 20
# level_all1$taxon.distribution.bc <-
#   substr(level_all1$taxon.distribution.bc, 1, 20)
botanicalCountries <- level_all1
row.names(botanicalCountries) <- NULL

# Brazilian states
statesBR <- c(
  "BR-AC", "BR-AL", "BR-AP", "BR-AM", "BR-BA", "BR-CE", "BR-DF",
  "BR-ES", "BR-GO", "BR-MA", "BR-MS", "BR-MT", "BR-MG",
  "BR-PB", "BR-PR", "BR-PA", "BR-PE", "BR-PI", "BR-RJ", "BR-RN", "BR-RS",
  "BR-RO", "BR-RR", "BR-SC", "BR-SP", "BR-SE", "BR-TO")
names(statesBR) <- c("acre", "alagoas", "amapa", "amazonas", "bahia", "ceara",
                      "distrito federal", "espirito santo", "goias", "maranhao",
                      "mato grosso sul", "mato grosso", "minas gerais",
                      "paraiba", "parana", "para", "pernambuco", "piaui", "rio janeiro",
                      "rio grande norte", "rio grande sul", "rondonia", "roraima",
                      "santa catarina", "sao paulo", "sergipe", "tocantins")

# Trying to decrease file sizes
rownames(admin) <- NULL
rownames(gazetteer) <- NULL
rownames(collectionCodes) <- NULL
rownames(taxonomists) <- NULL

# Saving data
usethis::use_data(admin, # salvar Admin no plantRdata ou como inst/extdata?
                  collectionCodes,
                  familiesSynonyms,
                  fieldNames,
                  gazetteer,
                  replaceNames,
                  taxonomists,
                  missLocs,
                  wordsForSearch,
                  unwantedLatin,
                  unwantedEncoding,
                  cultivated,
                  notCultivated,
                  missColls,
                  missDets,
                  treatPreps,
                  namePreps,
                  badEncoding,
                  reservedColNames,
                  simpGeoCheck,
                  botanicalCountries,
                  statesBR,
                  commonAuthors,
                  overwrite = TRUE,
                  internal = TRUE,
                  compress = "xz")
#Removing data
rm(admin, collectionCodes, familiesSynonyms, fieldNames, gazetteer,
   replaceNames, taxonomists, missLocs, wordsForSearch, unwantedLatin,
   unwantedEncoding, cultivated, notCultivated, missColls, missDets,
   treatPreps, namePreps, badEncoding, reservedColNames, simpGeoCheck,
   botanicalCountries, statesBR, commonAuthors)
