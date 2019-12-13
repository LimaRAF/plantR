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

# aqui to fazendo na mao para manter o nome dos objetos
autores <- dic[[1]]
collectionCodes <- dic[[2]]
families_synonyms <- dic[[3]]
field_names <- dic[[4]]
gazetteer <- dic[[5]]

head(autores)
head(collectionCodes)
head(families_synonyms)
head(field_names)
head(gazetteer)
collectionCodes$section

save(autores,
     collectionCodes,
     families_synonyms,
     field_names,
     gazetteer,
     file = "R/sysdata.rda",
     compress = "xz")
