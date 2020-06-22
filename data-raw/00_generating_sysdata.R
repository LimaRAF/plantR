# Script to read dictionaries and generate the R/sysdata.rda

# loading packages
library(stringr)
library(readr)
library(dplyr)
# all_incov <- iconvlist()
# nenhum dos ANSI ou WINDOWS funcionam
# ansi <- all_incov[str_detect(all_incov, "ANSI")]
# ruwin <- all_incov[str_detect(all_incov, "WINDOWS")]

#ast: this should be in data-raw/dictionaries
dic_files <- list.files(path = "./data-raw",
                        pattern = "csv",
                        full.names = TRUE)

#ast: usando guess_encoding para entender.
loc_list <- purrr::map(dic_files,
                       ~readr::guess_encoding(.))
# hmmm
# dic <- purrr::map2(.x = dic_files,
#                    .y = loc_list,
#                    .f = read_csv,
#                    locale = locale(encoding = encoding))
# but the ideal way is having correct enconding from the source

encoding <- "ISO-8859-15" # substituir aqui pelo encoding correto #ast no such thing for now
dic <- lapply(dic_files, read_csv, locale = locale(encoding = encoding))
lapply(dic, head)
# transforma em data.frame
dic <- lapply(dic, as.data.frame)

# dai imagino que usaria o iconv para transformar em UTF-8
# ast: não precisa transformar - a leitura vai permitir usar o arquivo tranquilo e suas opções predeterminadas de encoding para salvar deveriam ser UTF-8 anyway - no R e Rsatudio mesmo.

#encoding_to <- "UTF-8"
#dic <- lapply(dic, iconv, from = encoding, to = encoding_to)

# sara: aqui to fazendo na mao para manter o nome dos objetos
# renato: acrescentei uma filtragem para tirar colunas/linhas desnecessárias e diminuir o tamanho dos arquivos
# taxonomists:
autores <- dic[[1]][ ,c("order","source","family","family.obs","full.name1","tdwg.name")]

autores <- autores[!is.na(autores$tdwg.name), ]
autores <- autores[!is.na(autores$family), ]
autores <- autores[!grepl('\\?|,',autores$family), ]
autores <- autores[!grepl('Floristics/Generalist \\(all families\\)|Wood anatomist', autores$family), ]
# dictionary of herbarium codes:
collectionCodes <- dic[[2]][ ,c("order","collection.string","collectioncode.gbif",
                                "institutioncode.gbif","name","index.herbariorum.or.working.code",
                                "organization","OBS")]
# dictionary of plant families and their synonyms
families_synonyms <- dic[[3]]
# names of the columns names form differnt data sources and their equivalencies:
field_names <- dic[[4]][ ,c("order","standard_name","gbif","splink","splink2gbif","jabot","jabot_old",
                            "example","plantR_status")]

# gazetteer
gazetteer <- dic[[5]][ ,c("order","status","source","loc","loc.correct",
                          "latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")]
gazetteer <- gazetteer[gazetteer$status %in% "ok",]
### REVER FORMA DE REMOVER LOCALIDADES COM COORDENADAS DIFERENTES...
gazetteer$priority <- as.double(as.character(factor(gazetteer$source, levels = unique(gazetteer$source),
                                                     labels = c(2, 5, 4, 2, 5, 1, 4, 4, 3, 4, 1))))
gazetteer <- gazetteer[order(gazetteer$priority), ]
gazetteer <- gazetteer[!duplicated(gazetteer$loc) & !is.na(gazetteer$loc.correct),]

# administrative descriptors
admin <- dic[[5]][ ,c("order","status","source","country_code",
                           "NAME_0","state_code","NAME_1","NAME_2","NAME_3","NAME_4",
                           "loc","loc.correct","resolution.gazetteer")]
admin <- admin[admin$status %in% "ok", ]
admin$priority <- as.double(as.character(factor(admin$source, levels = unique(admin$source),
                                                     labels = c(2, 5, 4, 2, 5, 1, 4, 4, 3, 4, 1))))
admin <- admin[order(admin$priority), ]
admin <- admin[order(admin$loc.correct),]
admin <- admin[!duplicated(admin$loc.correct),] # removing duplicated localities
admin <- admin[admin$resolution.gazetteer %in% c("country","state","county","localidade"),] # removing localities below locality level (i.e. sublocalities)
admin <- admin[, c(
  "order",
  "loc.correct",
  "country_code",
  "state_code",
  "NAME_0",
  "NAME_1",
  "NAME_2",
  "NAME_3",
  "source"
)]

# names and abbreviation of localities to be replaced
replace_names <- dic[[6]]
replace_names[] <- lapply(replace_names, gsub, pattern = "Ã¡", replacement = "á", ignore.case = TRUE, perl = TRUE)
replace_names[] <- lapply(replace_names, gsub, pattern = "Ã©", replacement = "é", ignore.case = TRUE, perl = TRUE)
replace_names[] <- lapply(replace_names, gsub, pattern = "Ã£", replacement = "ã", ignore.case = TRUE, perl = TRUE)
replace_names[] <- lapply(replace_names, gsub, pattern = "ÃŽ", replacement = "ô", ignore.case = TRUE, perl = TRUE)
replace_names[] <- lapply(replace_names, gsub, pattern = "\u00AD", replacement = "??", perl = TRUE) # hidden breaking space
replace_names[] <- lapply(replace_names, gsub, pattern = "Ã\\?\\?", replacement = "í", ignore.case = TRUE, perl = TRUE)
replace_names[] <- lapply(replace_names, gsub, pattern = "ÂŠ", replacement = "ª", ignore.case = TRUE, perl = TRUE)


# other objects necessary fot the data processing and validation
unwanted_array = list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                      'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                      'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='S', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                      'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                      'ö'='o', 'ø'='o', 'ü'='u', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )
missLocs = c("^\\?$","^s\\/localidade","^indeterminada$","^indeterminado$","^s\\.d\\.$","^desconhecido$","^sin loc\\.$","^sin\\. loc\\.$",
             "^ignorado$","^sem informacao$","^n\\.i\\.","^nao especificado$","^nao informado$","^bloqueado$",
             "no locality information available","^protected due to name conservation status","^completar datos",
             "^no disponible$","^not available$","^loc\\.ign$","local ignorado")
wordsForSearch = c("^prov\\. ","^dep\\. ","^depto\\. ","^prov\\.","^mun\\. ","^dept\\.","^dpto\\.","^depto\\.","^dept.",
                   "^departamento ","^departamento de ","^provincia de ","^província de ","^estado do ","^estado de ")


# só checando como estao os arquivos
head(autores)
head(collectionCodes)
head(families_synonyms)
head(field_names)
head(gazetteer)
head(admin)
head(replace_names)

#ast: se os arquivos ficaram bons vou salvar como csv e ler o encoding de novo :shrugs:
dir.create("./data-raw/dictionaries")
write_csv(autores, "./data-raw/dictionaries/autores.csv")
write_csv(collectionCodes, "./data-raw/dictionaries/families_synonyms.csv")
write_csv(families_synonyms, "./data-raw/dictionaries/collectionCodes.csv")
write_csv(field_names, "./data-raw/dictionaries/field_names.csv")
write_csv(gazetteer, "./data-raw/dictionaries/gazetteer.csv")
write_csv(admin, "./data-raw/dictionaries/admin.csv")

###tentar ver se o encoding se resolveu mesmo:
dic_files <- list.files(path = "./data-raw/dictionaries",
                        pattern = "csv",
                        full.names = TRUE)

#ast: usando guess_encoding para entender.
loc_list <- purrr::map(dic_files,
                       ~readr::guess_encoding(.))

loc_list
#nelhorou mas ainda dá erros -UTF-8 agora


usethis::use_data(autores,
                  collectionCodes,
                  families_synonyms,
                  field_names,
                  gazetteer,
                  admin,
                  replace_names,
                  unwanted_array,
                  missLocs,
                  wordsForSearch,
                  overwrite = TRUE,
                  internal = TRUE,
                  compress = "xz")

