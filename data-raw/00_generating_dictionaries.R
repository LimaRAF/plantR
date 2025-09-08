# Script to read from raw_dictionaries and save in data-raw/dictionaries

# loading packages
library(stringr)
library(readr)
library(dplyr)

## 0. Downloading new information from Google Drive -----------------

### Taxonomists
link <- "https://docs.google.com/spreadsheets/d/1N3DV-e_2bRKgHTl1VmKwkjEPt4yrMQ2Bb2-X4ZtkXHQ/edit?usp=sharing"
path <- 'data-raw/raw_dictionaries/taxonomists.xlsx'
dl <- googledrive::drive_download( googledrive::as_id(link),
                                   path = path,
                                   overwrite = TRUE)
dados <- as.data.frame(readxl::read_xlsx(path, guess_max = 10000))
# replacing "NA"s
empty.vec <- c("", " ", "NA")
for (i in seq_along(dados))
  dados[[i]][dados[[i]] %in% c("", " ", "NA")] <- NA
# saving and cleaning
write.csv(dados, gsub("xlsx$", "csv", path), fileEncoding = "UTF-8",
          row.names = FALSE)
file.remove(path)

### Gazetteer
link <- "https://docs.google.com/spreadsheets/d/1qOyD7qDzgqEz6Xdbf5GcrjDChjJbJApfWk_P4pgCOAg/edit?usp=sharing"
path <- 'data-raw/raw_dictionaries/gazetteer.xlsx'
dl <- googledrive::drive_download( googledrive::as_id(link),
                                   path = path,
                                   overwrite = TRUE)
dados <- as.data.frame(readxl::read_xlsx(path, guess_max = 10000))
# replacing "NA"s
empty.vec <- c("", " ", "NA")
for (i in seq_along(dados))
  dados[[i]][dados[[i]] %in% c("", " ", "NA")] <- NA
# character to numbers
cols2change <- c("latitude.gazetteer", "longitude.gazetteer")
for (i in seq_along(cols2change))
  dados[[cols2change[i]]] <- as.numeric(dados[[cols2change[i]]])
# saving and cleaning
write.csv(dados, gsub("xlsx$", "csv", path), fileEncoding = "UTF-8",
          row.names = FALSE)
file.remove(path)

### Replace Names
link <- "https://docs.google.com/spreadsheets/d/1ghaHza2waxxufx-mYZMq2a66r6xLVgqfKy4zckwEYj8/edit?usp=sharing"
path <- 'data-raw/raw_dictionaries/replaceNames.xlsx'
dl <- googledrive::drive_download( googledrive::as_id(link),
                                   path = path,
                                   overwrite = TRUE)
dados <- as.data.frame(readxl::read_xlsx(path, guess_max = 10000,
                                         trim_ws = FALSE))
# replacing "NA"s
empty.vec <- c("", " ", "NA")
for (i in seq_along(dados))
  dados[[i]][dados[[i]] %in% c("", " ", "NA")] <- NA

write.csv(dados, gsub("xlsx$", "csv", path), fileEncoding = "UTF-8",
          row.names = FALSE)
file.remove(path)

### Family Synonyms
link <- "https://docs.google.com/spreadsheets/d/1FHbkNlR_-BXACPMs5CQy0zC4ydwLZrgqabYpHDBJaXc/edit?usp=sharing"
path <- 'data-raw/raw_dictionaries/familiesSynonyms.xlsx'
dl <- googledrive::drive_download( googledrive::as_id(link),
                                   path = path,
                                   overwrite = TRUE)
dados <- as.data.frame(readxl::read_xlsx(path, guess_max = 10000))
# replacing "NA"s
empty.vec <- c("", " ", "NA")
for (i in seq_along(dados))
  dados[[i]][dados[[i]] %in% c("", " ", "NA")] <- NA

write.csv(dados, gsub("xlsx$", "csv", path), fileEncoding = "UTF-8",
          row.names = FALSE)
file.remove(path)


### Collection Codes
link <- "https://docs.google.com/spreadsheets/d/1T4aq9zEXG76wFnVJRTI4-atMK1IiJVW9J8NpJXW9Bn8/edit?usp=sharing"
path <- 'data-raw/raw_dictionaries/collectionCodes.xlsx'
dl <- googledrive::drive_download( googledrive::as_id(link),
                                   path = path,
                                   overwrite = TRUE)
dados <- as.data.frame(readxl::read_xlsx(path, guess_max = 10000))
# replacing "NA"s
empty.vec <- c("", " ", "NA")
for (i in seq_along(dados))
  dados[[i]][dados[[i]] %in% c("", " ", "NA")] <- NA
# character to numbers
cols2change <- c("latitude", "longitude")
for (i in seq_along(cols2change))
  dados[[cols2change[i]]] <- as.numeric(dados[[cols2change[i]]])
cols2change <- c("records", "total accessioned")
for (i in seq_along(cols2change))
  dados[[cols2change[i]]] <- as.integer(dados[[cols2change[i]]])
write.csv(dados, gsub("xlsx$", "csv", path), fileEncoding = "UTF-8",
          row.names = FALSE)
file.remove(path)


## 1. from raw files to processed csv in dictionaries -------------
dic_files <- list.files(path = "data-raw/raw_dictionaries",
                        pattern = "csv",
                        full.names = TRUE)

#filtering out unwanted files
dic_files <-
  dic_files[!grepl("CC_database|IndexXylariorum|splink_non_plants|gadmCheck",
                   dic_files)]

#nome das tabelas
data_names <- basename(dic_files) %>%
  stringr::str_split(., "[:punct:]", simplify = TRUE) %>%
  data.frame() %>%
  select(1) %>%
  pull()

#guess encoding
loc_list <- purrr::map(dic_files,
                       ~readr::guess_encoding(.))

encoding <- "UTF-8"

dic <- lapply(dic_files,
              read_csv,
              trim_ws = FALSE,
              guess_max = 30000,# this has to be large
              locale = locale(encoding = encoding),
              )

names(dic) <- data_names

# # transforma em data.frame
# dic <- lapply(dic, as.data.frame)

# taxonomists:
taxonomists <- dic$taxonomists[ ,
                                c("order", "source", "status",
                                  "family", "family.obs",
                                  "full.name1", "tdwg.name")]
taxonomists <- taxonomists[!is.na(taxonomists$tdwg.name), ]
taxonomists <- taxonomists[!is.na(taxonomists$family), ]
taxonomists <- taxonomists[!grepl('\\?|,',taxonomists$family), ]
taxonomists <- taxonomists[!grepl('Wood anatomist', taxonomists$family), ]
taxonomists <- taxonomists[taxonomists$status %in% "ok", ]

taxonomists$status <- NULL
taxonomists <- taxonomists[order(taxonomists$order),]
taxonomists$order <- NULL
taxonomists$family.obs <- NULL

# collection codes:
collectionCodes <- dic$collectionCodes[ ,c("ordem.colecao",
                                           "collection.string",
                                           "collectioncode.gbif",
                                           "institutioncode.gbif",
                                           #"name",
                                           #"citation",
                                           "index.herbariorum.or.working.code",
                                           "organization",
                                           #"latitude","longitude","physical country",
                                           "col.OBS")]
collectionCodes <-
  collectionCodes[!is.na(collectionCodes$index.herbariorum.or.working.code),]
collectionCodes$ordem.colecao <- NULL

# Plant families
familiesSynonyms <- dic$familiesSynonyms
familiesSynonyms$order <- NULL

# Field names form different data sources and their equivalencies:
# ATTENTION: fieldNames has its own script now data-raw/make_fieldNames.R
# fieldNames <- dic$fieldNames
# fieldNames <- dic$fieldNames[ ,c("order",
#                                   "standard_name",
#                                   "gbif",
#                                   "splink",
#                                   "splink2gbif",
#                                   "jabot",
#                                   "jabot_old",
#                                   "example",
#                                   "plantR_status")]

# gazetteer
gazetteer <- dic$gazetteer[ ,c("order",
                               "status",
                               "source",
                               "loc",
                               "loc.correct",
                               "latitude.gazetteer",
                               "longitude.gazetteer",
                               "resolution.gazetteer")]

gazetteer <- gazetteer[grepl("^ok", gazetteer$status, perl = TRUE),]

### REVER FORMA DE REMOVER LOCALIDADES COM COORDENADAS DIFERENTES...
priorities <- data.frame(source = c("gdam", "gdam_treeco", "treeco", "ibge",
                                    "google", "ibge_treeco", "splink_jabot",
                                    "gbif", "gbif_gsg", "cncflora", "ibge?", "types"),
                         priority = c(2, 3, 3, 1, 5, 0, 4, 4, 4, 3, 3, 0))
priorities[order(priorities$priority),]

gazetteer <- dplyr::left_join(gazetteer, priorities)
gazetteer <- gazetteer[order(gazetteer$priority), ]
dplyr::count(gazetteer, source, priority) %>% arrange(priority)
gazetteer <-
  gazetteer[!duplicated(gazetteer$loc) & !is.na(gazetteer$loc.correct),]

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
admin <- admin[grepl("^ok", admin$status, perl = TRUE), ]
admin <- left_join(admin, priorities)
admin <- admin[order(admin$priority), ]
admin <- admin[order(admin$loc.correct),]
admin <- admin[!duplicated(admin$loc.correct),] # removing duplicated localities
admin <- admin[!is.na(admin$loc.correct),]
admin <- admin[admin$resolution.gazetteer %in%
                 c("country", "state","county", "localidade"),] # removing localities below locality level (i.e. sublocalities)

admin <- admin[, c("source",
                   #"order",
                   "loc.correct",
                   "country_code",
                   #"state_code",
                   "NAME_0",
                   "NAME_1",
                   "NAME_2",
                   "NAME_3")]

gazetteer$order <- NULL
gazetteer$status <- NULL
gazetteer$priority <- NULL

check_these <- stringr::str_count(admin$loc.correct, "_") > 2
check_these[is.na(check_these)] <- FALSE
loc_parc <- admin$loc.correct
loc_parc[check_these] <- str_extract(loc_parc[check_these], ".*(?=_)")
probs <- admin$NAME_2[match(loc_parc, admin$loc.correct)] != admin$NAME_2
probs[is.na(probs)] <- FALSE
if(any(probs)) {
  as.data.frame(admin)[probs,c(1,4:6)]
  # stopifnot(dim(admin[probs, ])[1] == 0)
  # sort(admin$order[probs])
}


# names and abbreviation of localities to be replaced
replaceNames <- dic$replaceNames
tmp1 <- replaceNames[replaceNames$class %in% "locality1" & apply(is.na(replaceNames[,2:4]), 1, all),]
tmp1$replace

#Renato ö: o codigo abaixo estava dando problemas e tenho quase certeza que preciamos
#dos non_ascii para fazer a reposição dos nomes. Descomentar só após verificar
#se a reposição precisa ou não dos non_ascii. Se tiver alguma entrada dando pau em
#particular editar a entrada do dicionário raw na mão mesmo...
# for (i in 1:length(replaceNames))
#   replaceNames[, i] <- textclean::replace_non_ascii(replaceNames[, i])

# só checando como estao os arquivos
head(taxonomists); dim(taxonomists)
head(familiesSynonyms); dim(familiesSynonyms)
head(collectionCodes)
head(gazetteer)
head(admin)
head(replaceNames)

write_csv(taxonomists, "./data-raw/dictionaries/taxonomists.csv")
write_csv(familiesSynonyms, "./data-raw/dictionaries/familiesSynonyms.csv")
write_csv(collectionCodes, "./data-raw/dictionaries/collectionCodes.csv")
write_csv(gazetteer, "./data-raw/dictionaries/gazetteer.csv")
write_csv(admin, "./data-raw/dictionaries/admin.csv")
write_csv(replaceNames, "./data-raw/dictionaries/replaceNames.csv")

#Removing data
rm(taxonomists, familiesSynonyms, collectionCodes, gazetteer, admin, replaceNames)


