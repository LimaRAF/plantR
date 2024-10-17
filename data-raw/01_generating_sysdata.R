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
              guess_max = 30000,#this has to be large
              locale = locale(encoding = encoding))

names(dic) <- data_names
lapply(dic, nrow) #new dims! especially gazetteer from 34807 to 23436
#taxonomists from 9297 to 8518 (01/07/2020)

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
                    "^mun\\. ",
                    "^dept\\.",
                    "^dpto\\.",
                    "^depto\\.",
                    "^dept.",
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
url1 <- "https://github.com/tdwg/wgsrpd/raw/master/109-488-1-ED/2nd%20Edition/tblLevel3.txt"
zip <- paste0("wcvp", ".zip")
path <- file.path(here::here(), "data-raw", zip)
path1 <- gsub("\\.zip", "_dist.txt", path)
utils::download.file(url = url1, destfile = path1, mode = "wb")
level3 <- read.table(path1, sep = "*", fileEncoding = "Latin1",
                     header = TRUE, stringsAsFactors = FALSE,
                     quote = "", fill = TRUE)
unlink(path1)
# Download subdivision of botanical countries (level4)
url2 <- "https://github.com/tdwg/wgsrpd/raw/master/109-488-1-ED/2nd%20Edition/tblLevel4.txt"
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

# Saving data
usethis::use_data(admin,
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
                  simpGeoCheck,
                  botanicalCountries,
                  overwrite = TRUE,
                  internal = TRUE,
                  compress = "xz")
#Removing data
rm(admin, collectionCodes, familiesSynonyms, fieldNames, gazetteer,
   replaceNames, taxonomists, missLocs, wordsForSearch, unwantedLatin,
   unwantedEncoding, cultivated, notCultivated, missColls, missDets,
   treatPreps, namePreps, badEncoding, simpGeoCheck, botanicalCountries)
