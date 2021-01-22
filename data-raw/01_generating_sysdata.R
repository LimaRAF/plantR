# Script to read from data-raw/dictionaries and generate sysdata


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
              "UTF-8",
              "ASCII",
              "ASCII",
              "UTF-8")

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
                  overwrite = TRUE,
                  internal = TRUE,
                  compress = "xz")

