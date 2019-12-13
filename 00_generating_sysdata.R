# Script to read dictionaries and generate the R/sysdata.rda

# loading packages
library(stringr)
library(readr)

# testei com todos as opções de ANSI disponível em iconvlist
all_incov <- iconvlist()
# nenhum dos ANSI ou WINDOWS funcionam
ansi <- all_incov[str_detect(all_incov, "ANSI")]
ruwin <- all_incov[str_detect(all_incov, "WINDOWS")]

dic_files <- list.files(path = "dictionaries",
                        pattern = "csv",
                        full.names = TRUE)

encoding <- "ISO-8859-15" # substituir aqui pelo encoding correto
dic <- lapply(dic_files, read_csv, locale = locale(encoding = encoding))
lapply(dic, head)

# transforma em data.frame
dic <- lapply(dic, as.data.frame)

# dai imagino que usaria o iconv para transformar em UTF-8
#encoding_to <- "UTF-8"
#dic <- lapply(dic, iconv, from = encoding, to = encoding_to)

# sara: aqui to fazendo na mao para manter o nome dos objetos
# renato: acrescentei uma filtragem para tirar colunas/linhas desnecessárias e diminuir o tamanho dos arquivos
autores <- dic[[1]][,c("order","source","family","family.obs","full.name1","tdwg.name")]
autores <- autores[!is.na(autores$tdwg.name),]
autores <- autores[!is.na(autores$family),]
autores <- autores[!grepl('\\?|,',autores$family),]
autores <- autores[!grepl('Floristics/Generalist \\(all families\\)|Wood anatomist', autores$family),]
collectionCodes <- dic[[2]][,c("order","collection.string","collectioncode.gbif",
                               "institutioncode.gbif","name","index.herbariorum.or.working.code",
                               "organization","OBS")]
families_synonyms <- dic[[3]]
field_names <- dic[[4]]
gazetteer <- dic[[5]][,c("order","status","source","country_code","NAME_0","NAME_1","NAME_2","NAME_3","NAME_4",
                         "loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")]
gazetteer <- gazetteer[gazetteer$status %in% "ok",]

head(autores)
head(collectionCodes)
head(families_synonyms)
head(field_names)
head(gazetteer)

#Saving the sysdata
save(autores,
     collectionCodes,
     families_synonyms,
     field_names,
     gazetteer,
     file = "R/sysdata.rda",
     compress = "xz")
