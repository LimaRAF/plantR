# Script to read dictionaries and generate the R/sysdata.rda

# loading packages
library(stringr)
library(readr)
library(dplyr)


#1. from raw files to processed csv in dictionaries----

#dic_files <- list.files(path = "./data-raw/raw", # old path: changing paths because I cannot subversion the package folders within a github local repository
dic_files <- list.files(path = "C:/Users/renato/Documents/raflima/Pos Doc/Manuscritos/Artigo AF checklist/data analysis/dictionaries",
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

encoding <- "UTF-8" #ast we are getting somewhere
dic <- lapply(dic_files,
              read_csv,
              guess_max = 30000,#this has to be large
              locale = locale(encoding = encoding)
              )

names(dic) <- data_names
lapply(dic, nrow)
# transforma em data.frame
dic <- lapply(dic, as.data.frame)


# taxonomists:
taxonomists <- dic$taxonomists[ ,c("order", "source", "family", "family.obs", "full.name1", "tdwg.name")]
taxonomists <- taxonomists[!is.na(taxonomists$tdwg.name), ]
taxonomists <- taxonomists[!is.na(taxonomists$family), ]
taxonomists <- taxonomists[!grepl('\\?|,',taxonomists$family), ]
taxonomists <- taxonomists[!grepl('Wood anatomist', taxonomists$family), ]


collectionCodes <- dic$collectionCodes[ ,c("order",
                                           "collection.string",
                                           "collectioncode.gbif",
                                           "institutioncode.gbif",
                                           "name",
                                           "index.herbariorum.or.working.code",
                                           "organization",
                                           "OBS")]
# dictionary of plant families and their synonyms
familiesSynonyms <- dic$familiesSynonyms
# names of the columns names form different data sources and their equivalencies:
fieldNames <- dic$fieldNames[ ,c("order",
                                  "standard_name",
                                  "gbif",
                                  "splink",
                                  "splink2gbif",
                                  "jabot",
                                  "jabot_old",
                                  "example",
                                  "plantR_status")]
# gazetteer
gazetteer <- dic$gazetteer[ ,c("order",
                               "status",
                               "source",
                               "loc",
                               "loc.correct",
                               "latitude.gazetteer",
                               "longitude.gazetteer",
                               "resolution.gazetteer")]

gazetteer <- gazetteer[gazetteer$status %in% "ok",]

### REVER FORMA DE REMOVER LOCALIDADES COM COORDENADAS DIFERENTES...

priorities <- data.frame(source = unique(gazetteer$source), priority =
                           c(2, 5, 4, 2, 5, 1, 4, 4, 3, 4, 1))
priorities
#ast ö checar que a ordem seja esta
gazetteer <- left_join(gazetteer, priorities)
gazetteer <- gazetteer[order(gazetteer$priority), ]
dplyr::count(gazetteer, source, priority) %>% arrange(priority)
gazetteer <- gazetteer[!duplicated(gazetteer$loc) & !is.na(gazetteer$loc.correct),]

# administrative descriptors

admin <- dic$gazetteer[ ,c("order",
                           "status",
                           "source",
                           "country_code",
                           "NAME_0",
                           "state_code",
                           "NAME_1",
                           "NAME_2",
                           "NAME_3",
                           "NAME_4",
                           "loc",
                           "loc.correct",
                           "resolution.gazetteer")]
admin <- admin[admin$status %in% "ok", ]
admin <- left_join(admin, priorities)
admin <- admin[order(admin$priority), ]
admin <- admin[order(admin$loc.correct),]
admin <- admin[!duplicated(admin$loc.correct),] # removing duplicated localities
admin <- admin[admin$resolution.gazetteer %in% c("country", "state","county", "localidade"),] # removing localities below locality level (i.e. sublocalities)

admin <- admin[, c("order",
                   "loc.correct",
                   "country_code",
                   "state_code",
                   "NAME_0",
                   "NAME_1",
                   "NAME_2",
                   "NAME_3",
                   "source")]

# names and abbreviation of localities to be replaced
replaceNames <- dic$replaceNames
for (i in 1:length(replaceNames))
  replaceNames[, i] <- textclean::replace_non_ascii(replaceNames[, i])

# other objects necessary fot the data processing and validation

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
              "local ignorado")

wordsForSearch <- c("^prov\\. ",
                    "^dep\\. ",
                    "^depto\\. ",
                    "^prov\\.",
                    "^mun\\. ",
                    "^dept\\.",
                    "^dpto\\.",
                    "^depto\\.",
                    "^dept.",
                   "^departamento ",
                   "^departamento de ",
                   "^departamento del ",# I would add this it probably shows up
                   "^provincia de ",
                   "^provincia del ",
                   "^província de ",
                   "^estado do ",
                   "^estado de ")


# só checando como estao os arquivos
head(taxonomists)
head(familiesSynonyms)
head(collectionCodes)
head(fieldNames)
head(gazetteer)
head(admin)
head(replaceNames)

#dir.create("./data-raw/dictionaries")
write_csv(taxonomists, "./data-raw/dictionaries/taxonomists.csv")
write_csv(familiesSynonyms, "./data-raw/dictionaries/familiesSynonyms.csv")
write_csv(collectionCodes, "./data-raw/dictionaries/collectionCodes.csv")
write_csv(fieldNames, "./data-raw/dictionaries/fieldNames.csv")
write_csv(gazetteer, "./data-raw/dictionaries/gazetteer.csv")
write_csv(admin, "./data-raw/dictionaries/admin.csv")
write_csv(replaceNames, "./data-raw/dictionaries/replaceNames.csv")

# 2. from processed csv files to sysdata.rda ----
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

loc_list <- c(
  "UTF-8",
  "UTF-8",
  "UTF-8",
  "UTF-8",
  "ASCII",
  "ASCII",
  "UTF-8")

dic <- purrr::map2(.x = dic_files,
                   .y = loc_list,
                   ~read_csv(file = .x,
                             guess_max = 30000,#this has to be large
                             locale = locale(encoding = .y)
))
encoding <- "UTF-8"
dic <- lapply(dic_files,
              read_csv,
              guess_max = 30000,#this has to be large
              locale = locale(encoding = encoding)
)

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

# Saving data
usethis::use_data(
                  admin,
                  collectionCodes,
                  familiesSynonyms,
                  fieldNames,
                  gazetteer,
                  replaceNames,
                  taxonomists,
                  missLocs,
                  wordsForSearch,
                  overwrite = TRUE,
                  internal = TRUE,
                  compress = "xz")

