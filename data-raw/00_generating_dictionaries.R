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
dados <- readxl::read_xlsx(path, guess_max = 10000)
# replacing "NA"s
empty.vec <- c("", " ", "NA")
for (i in seq_along(dados))
  dados[[i]][dados[[i]] %in% c("", " ", "NA")] <- NA
# saving and cleaning
readr::write_csv(dados, gsub("xlsx$", "csv", path),)
# write.csv(dados, gsub("xlsx$", "csv", path), fileEncoding = "UTF-8",
#           row.names = FALSE)
file.remove(path)

### Gazetteer
link <- "https://docs.google.com/spreadsheets/d/1qOyD7qDzgqEz6Xdbf5GcrjDChjJbJApfWk_P4pgCOAg/edit?usp=sharing"
path <- 'data-raw/raw_dictionaries/gazetteer.xlsx'
dl <- googledrive::drive_download( googledrive::as_id(link),
                                   path = path,
                                   overwrite = TRUE)
dados <- readxl::read_xlsx(path, guess_max = 10000)
# replacing "NA"s
empty.vec <- c("", " ", "NA")
for (i in seq_along(dados))
  dados[[i]][dados[[i]] %in% c("", " ", "NA")] <- NA
# character to numbers
cols2change <- c("latitude.gazetteer", "longitude.gazetteer")
for (i in seq_along(cols2change))
  dados[[cols2change[i]]] <- as.numeric(dados[[cols2change[i]]])

# fixing enconding issues
# bad_enc <- enc2native(names(badEncoding))
# good_enc <- names(badEncoding)
# bad_enc <- c(bad_enc, "<U+00C1>")
# good_enc <- c(good_enc, "Á")
#
# col2check <- c("NAME_0", "NAME_1", "NAME_2", "NAME_3")
# for (i in seq_along(col2check)) {
#   vect.i <- dados[[col2check[i]]]
#   check_these <- !stringi::stri_enc_mark(vect.i) %in% "ASCII"
#   if (any(check_these)) {
#     vect.i[check_these] <- enc2native(vect.i[check_these])
#     # vect.i[check_these] <- iconv(vect.i[check_these], "latin1", "UTF-8")
#     # Encoding(vect.i[check_these]) <- "UTF-8"
#     for (j in seq_along(bad_enc))
#       vect.i[check_these] <- gsub(bad_enc[j], good_enc[j],
#                                   vect.i[check_these], fixed = TRUE)
#
#     # t <- stringi::stri_trans_general(vect.i[check_these],"any-latin")
#     # t <- stringi::stri_trans_general(vect.i[check_these],"ASCII")
#     # t <- stringi::stri_trans_general(t, "Latin")
#     # "Any-Accents"
#     #vect.i <- enc2utf8(vect.i)
#     vect.i[check_these] <-
#       stringi::stri_trans_general(vect.i[check_these],"ASCII")
#     dados[[col2check[i]]] <- iconv(vect.i, "", "UTF-8")
#   }
# }
head(dados)
head(dados$NAME_0)
stringi::stri_enc_mark(head(dados$NAME_0))

# saving and cleaning
readr::write_csv(dados, gsub("xlsx$", "csv", path))
# write.csv(dados, gsub("xlsx$", "csv", path), fileEncoding = "UTF-8",
#           row.names = FALSE)
file.remove(path)

### GADM checks
link <- "https://docs.google.com/spreadsheets/d/1ccJVYZMMtrL8mwZbPi-xW0nZK3-zQ3eidr7OMXFr-fE/edit?usp=sharing"
path <- 'data-raw/raw_dictionaries/gadmCheck.xlsx'
dl <- googledrive::drive_download( googledrive::as_id(link),
                                   path = path,
                                   overwrite = TRUE)
dados <- readxl::read_xlsx(path, guess_max = 10000, trim_ws = FALSE)
# replacing "NA"s
empty.vec <- c("", " ", "NA")
for (i in seq_along(dados))
  dados[[i]][dados[[i]] %in% c("", " ", "NA")] <- NA
# saving and cleaning
readr::write_csv(dados, gsub("xlsx$", "csv", path))
# write.csv(dados, gsub("xlsx$", "csv", path), fileEncoding = "UTF-8",
#           row.names = FALSE)
file.remove(path)

### Replace Names
link <- "https://docs.google.com/spreadsheets/d/1ghaHza2waxxufx-mYZMq2a66r6xLVgqfKy4zckwEYj8/edit?usp=sharing"
path <- 'data-raw/raw_dictionaries/replaceNames.xlsx'
dl <- googledrive::drive_download( googledrive::as_id(link),
                                   path = path,
                                   overwrite = TRUE)
dados <- readxl::read_xlsx(path, guess_max = 10000, trim_ws = FALSE)
# replacing "NA"s
empty.vec <- c("", " ", "NA")
for (i in seq_along(dados))
  dados[[i]][dados[[i]] %in% c("", " ", "NA")] <- NA

readr::write_csv(dados, gsub("xlsx$", "csv", path))
# write.csv(dados, gsub("xlsx$", "csv", path), fileEncoding = "UTF-8",
#           row.names = FALSE)
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

readr::write_csv(dados, gsub("xlsx$", "csv", path))
# write.csv(dados, gsub("xlsx$", "csv", path), fileEncoding = "UTF-8",
#           row.names = FALSE)
file.remove(path)


### Collection Codes
link <- "https://docs.google.com/spreadsheets/d/1T4aq9zEXG76wFnVJRTI4-atMK1IiJVW9J8NpJXW9Bn8/edit?usp=sharing"
path <- 'data-raw/raw_dictionaries/collectionCodes.xlsx'
dl <- googledrive::drive_download( googledrive::as_id(link),
                                   path = path,
                                   overwrite = TRUE)
dados <- as.data.frame(readxl::read_xlsx(path, guess_max = 10000))
# replacing "NA"s
empty.vec <- c("", " ")
for (i in seq_along(dados))
  dados[[i]][dados[[i]] %in% c("", " ")] <- NA
# character to numbers
cols2change <- c("latitude", "longitude")
for (i in seq_along(cols2change))
  dados[[cols2change[i]]] <- as.numeric(dados[[cols2change[i]]])
cols2change <- c("records", "specimenTotal")
for (i in seq_along(cols2change))
  dados[[cols2change[i]]] <- as.integer(dados[[cols2change[i]]])
readr::write_csv(dados, gsub("xlsx$", "csv", path))
# write.csv(dados, gsub("xlsx$", "csv", path), fileEncoding = "UTF-8",
#           row.names = FALSE)
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
  dplyr::pull()

#guess encoding
loc_list <- purrr::map(dic_files,
                       ~readr::guess_encoding(.))

encoding <- "UTF-8"

dic <- lapply(dic_files,
              readr::read_csv,
              trim_ws = FALSE,
              guess_max = 30000,# this has to be large
              locale = readr::locale(encoding = encoding),
              )
names(dic) <- data_names

# # transforma em data.frame
# dic <- lapply(dic, as.data.frame)

# taxonomists:
taxonomists <- dic$taxonomists[ ,
                                c("order", "source", "status",
                                  "tax", "tax.rank", "tax.obs",
                                  "full.name1", "tdwg.name",
                                  "tax.kingdom")]
taxonomists <- taxonomists[!is.na(taxonomists$tdwg.name), ]
taxonomists <- taxonomists[!is.na(taxonomists$tax), ]
taxonomists <- taxonomists[!grepl('\\?|,',taxonomists$tax), ]
taxonomists <- taxonomists[!grepl('Wood an|Citizen ', taxonomists$tax), ]
taxonomists <- taxonomists[!grepl('Naturalist', taxonomists$tax), ]
taxonomists <- taxonomists[taxonomists$status %in% "ok", ]

taxonomists$status <- NULL
taxonomists <- taxonomists[order(taxonomists$full.name1),]
taxonomists$order <- NULL
taxonomists$tax.obs <- NULL
taxonomists$tax.rank[is.na(taxonomists$tax.rank)] <- "family"
taxonomists$tax.rank[grepl("eneralist", taxonomists$tax)] <- NA
taxonomists$tax.rank[taxonomists$tax.rank %in% "family" &
                       !grepl("ceae$", taxonomists$tax) &
                       grepl("ales$", taxonomists$tax)] <- "order"
# Conifers
rep_these <- taxonomists$tax.rank %in% "family" & grepl("Conifers$|Gymnosperms$", taxonomists$tax)
if (any(rep_these)) { # Note: Gymnos also include Ginkgoopsida and Cycadopsida. Break in more lines?
  taxonomists$tax.rank[rep_these] <- "class"
  taxonomists$tax[rep_these] <- "Pinopsida"
}
# Monocots
rep_these <- taxonomists$tax.rank %in% "family" & grepl("Monocots$", taxonomists$tax)
if (any(rep_these)) {
  taxonomists$tax.rank[rep_these] <- "class"
  taxonomists$tax[rep_these] <- "Liliopsida"
}
# Pterydophyta
rep_these <- taxonomists$tax.rank %in% "family" & grepl("Pteridophyta$", taxonomists$tax)
if (any(rep_these)) { # Note: Ferns also include Lycopodiopsida. Break in more lines?
  taxonomists$tax.rank[rep_these] <- "class"
  taxonomists$tax[rep_these] <- "Polypodiopsida"
}
# Check for any changes or new taxa:
table(taxonomists$tax[taxonomists$tax.rank %in% "family" &
                       !grepl("ceae$", taxonomists$tax)])

# Generalists of multiple classes:
break_df <- taxonomists[grepl("\\|", taxonomists$tax, perl = TRUE), ]
split_tax <- strsplit(break_df$tax, "\\|")
res_df <- vector("list", length(split_tax))
for (i in 1:length(split_tax)) {
  split_df <- do.call("rbind", replicate(lengths(split_tax)[i],
                                         break_df[i, ], simplify = FALSE))
  split_df$tax <- unlist(split_tax[[i]])
  res_df[[i]] <- split_df
}
res_df1 <- as.data.frame(dplyr::bind_rows(res_df))

taxonomists <- taxonomists[!grepl("\\|", taxonomists$tax, perl = TRUE), ]
taxonomists <- rbind.data.frame(taxonomists, res_df1)
stopifnot(!any(grepl("\\|", taxonomists$tax, perl = TRUE)))

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

## false NA that is actually the collection code 'NA' (United States National Arboretum)
rep_these <- collectionCodes$index.herbariorum.or.working.code %in% "\"NA\"" &
                grepl("United States National Arboretum", collectionCodes$organization, perl = TRUE)
if (any(rep_these)) {
  repcols <- c("collectioncode.gbif", "institutioncode.gbif", "index.herbariorum.or.working.code")
  collectionCodes[rep_these, repcols] <- "NA"
}

collectionCodes <-
  collectionCodes[!is.na(collectionCodes$collection.string),]

# Remove duplicate colection strings of the type 'COL_NA' (now dealt internally in getCode)
rm_these <- duplicated(collectionCodes$index.herbariorum.or.working.code) &
              is.na(collectionCodes$institutioncode.gbif)
if (any(rm_these))
  collectionCodes <-collectionCodes[!rm_these, ]

collectionCodes$ordem.colecao <- NULL

# Plant families
familiesSynonyms <- dic$familiesSynonyms
familiesSynonyms$order <- NULL

## Removendo sinonimias conflituosas do COL com outras referencias (e.g. APG)
familiesSynonyms <- familiesSynonyms[!grepl("confli", familiesSynonyms$obs),]
familiesSynonyms$obs <- NULL

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
priorities <- data.frame(source = c("gadm", "gadm_new", "gadm_treeco", "treeco", "ibge",
                                    "google", "ibge_treeco", "splink_jabot",
                                    "gbif", "gbif_gsg", "cncflora", "ibge?", "types",
                                    "cnuc_ibge", "gadm?", "gbif_jabot",
                                    "gbif_jabot_reflora", "gbif_jabot_reflora_splink",
                                    "gbif_jabot_splink", "gbif_reflora",
                                    "gbif_reflora_splink", "gbif_splink",
                                    "jabot_splink", "reflora", "splink"),
                         priority = c(2, 3, 3, 3, 1, 5, 0, 4, 4, 4, 3, 3, 3,
                                      1, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4))
priorities[order(priorities$priority),]

gazetteer <- dplyr::left_join(gazetteer, priorities)
gazetteer <- gazetteer[order(gazetteer$priority), ]
dplyr::count(gazetteer, source, priority) %>% dplyr::arrange(priority)
gazetteer$loc <- plantR:::squish(gazetteer$loc)
gazetteer <-
  gazetteer[!duplicated(gazetteer$loc) & !is.na(gazetteer$loc.correct),]

gazetteer$source[grepl("ibge$", gazetteer$source)] <- "ibge"
gazetteer$source[grepl("^gadm", gazetteer$source)] <- "gadm"
gazetteer$source[grepl("gbif", gazetteer$source)] <- "herbaria"
gazetteer$source[grepl("splink", gazetteer$source)] <- "herbaria"

res.gaz <- c("localidade", "localidade|sublocalidade", "sublocalidade",
             "distrito|vila", "distrito", "distrito|bairro", "bairro",
             "cachoeira", "mina", "vila", "serra", "locality")
res.string <- stringr::str_count(gazetteer$loc.correct, "_")
rep_these <- gazetteer$resolution.gazetteer %in% res.gaz &
              res.string > 3
if(any(rep_these))
  gazetteer$resolution.gazetteer[rep_these] <- "sublocality"

rep_these <- gazetteer$resolution.gazetteer %in% res.gaz &
              res.string > 2
if(any(rep_these))
  gazetteer$resolution.gazetteer[rep_these] <- "locality"
all_res <- names(table(gazetteer$resolution.gazetteer))
stopifnot(all(all_res %in% c("country","state","county","locality","sublocality")))

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
                 c("country", "state", "county", "locality"),] # removing localities below locality level (i.e. sublocalities)

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
loc_parc[check_these] <- stringr::str_extract(loc_parc[check_these], ".*(?=_)")
probs <- admin$NAME_2[match(loc_parc, admin$loc.correct)] != admin$NAME_2
probs[is.na(probs)] <- FALSE
if(any(probs)) {
  as.data.frame(admin)[probs,c(1,4:6)]
  # stopifnot(dim(admin[probs, ])[1] == 0)
  # sort(admin$order[probs])
}


# names and abbreviation of localities to be replaced
replaceNames <- dic$replaceNames
tmp1 <- replaceNames[replaceNames$class %in% "locality1" &
                       apply(is.na(replaceNames[,2:4]), 1, all),]
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

readr::write_csv(taxonomists, "./data-raw/dictionaries/taxonomists.csv")
readr::write_csv(familiesSynonyms, "./data-raw/dictionaries/familiesSynonyms.csv")
readr::write_csv(collectionCodes, "./data-raw/dictionaries/collectionCodes.csv")
readr::write_csv(gazetteer, "./data-raw/dictionaries/gazetteer.csv")
readr::write_csv(admin, "./data-raw/dictionaries/admin.csv")
readr::write_csv(replaceNames, "./data-raw/dictionaries/replaceNames.csv")

#Removing data
rm(taxonomists, familiesSynonyms, collectionCodes, gazetteer, admin,
   replaceNames)


