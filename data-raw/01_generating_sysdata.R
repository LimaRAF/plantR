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
#troquei por "UTF-8" no `loc_list` e o erro sumiu. Mas não pq estava "ASCII" então, checar...

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

cultivated <- c("cultivated",
                "cultivada",
                "cultivado",
                "cultivato",
                "cultivad",
                "under cultivation",
                "plantada",
                "plantado",
                "planted",
                "plantio",
                "arboreto",
                "arboretum",
                "exotic",
                "exótica",
                #"canteiro",
                "pomar",
                "área de visitação",
                "cult\\.",
                "cant\\. [a-z]",
                "cant [A-Z]",
                "cant\\. [0-9]",
                "cant \\. [0-9]",
                "cant [0-9]",
                "\\(cult\\)",
                "\\(cult \\)",
                "in cultivo",
                "in cultis",
                " quadra [a-z]",
                "quadra [a-z] do",
                "naturalised",
                "em experimento de")

notCultivated <- c("nativa",
                   "espontânea",
                   "pastagem cultivada",
                   "área do arboreto",
                   "presença de exóticas",
                   " área cultivada",
                   " área cultivada",
                   " cultivated area")

missColls <- c("s/col.",
               "s/col",
               "s/c",
               "s/coletor",
               "s/colector",
               "s.coletor",
               " sem col.",
               "s.col.",
               "s.c.",
               "s.n.",
               "sem informação",
               "sem informacao",
               "collector unspecified",
               "collector unknown",
               "unknown",
               "disponible, n.",
               "disponivel, n.",
               "available, n.",
               "informação, s.",
               "informacao, s.",
               "sin",
               "?")

missDets <- c("s/det.",
              "s/det",
              "s/d",
              "s/determinador",
              "s.determinador",
              " sem det.",
              "s.det.",
              "s.n.",
              "sem informação",
              "sem informacao",
              "determiner unspecified",
              "determiner unknown",
              "unknown",
              "disponible, n.",
              "disponivel, n.",
              "available, n.",
              "informação, s.",
              "informacao, s.",
              "sin",
              "?")

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
                  simpGeoCheck,
                  overwrite = TRUE,
                  internal = TRUE,
                  compress = "xz")
