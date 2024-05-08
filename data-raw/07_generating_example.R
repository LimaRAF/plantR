# Script to generate the example datasets stored in the /data folder

# dataset example.rda ------------------------------------------------
## Loading the data using plantR function
# spp <- c("Trema micrantha", "Casearia sylvestris")
# chave <- readLines("data-raw/api_key.txt")
# df <- rspeciesLink(species = spp, key = chave,
#                    MaxRecords = 5000,
#                    basisOfRecord = "PreservedSpecimen",
#                    Scope = "p", Synonyms = "flora2020")
## Loading the data downloaded from their web interface
df <- as.data.frame(data.table::fread("data-raw/results/speciesLink-20240506233830-0032294.txt"))

## Filtering
df <- df[df$basisofrecord %in% c("PreservedSpecimen"), ]
field_names <- read.csv("data-raw/dictionaries/fieldNames.csv")
col.names <- field_names$speciesLink[!is.na(field_names$type)]
col.names <- col.names[!is.na(col.names)]
col.names <- col.names[!duplicated(col.names)]
df.names <- names(df)[names(df) %in% col.names]
example <- df[, df.names]

## Fixing non-ASCII
col_to_check <- c("country","stateprovince","county","locality",
                  "notes", "collector", "collectornumber",
                  "identifiedby",
                  "scientificnameauthor",
                  "verbatimlatitude", "verbatimlongitude")
for(i in seq_along(col_to_check)) {
  Encoding(example[[col_to_check[i]]]) <- "latin1"
  example[[col_to_check[i]]] <- iconv(example[[col_to_check[i]]],
    "latin1","UTF-8")
}

## Saving
usethis::use_data(example,
                  overwrite = TRUE,
                  internal = FALSE,
                  compress = "xz")

# dataset example.rda ------------------------------------------------
## Loading the data using plantR function
# spp <- c("Euterpe edulis")
# df <- rspeciesLink(species = spp, key = chave,
#                    MaxRecords = 5000,
#                    save = TRUE, dir = "data-raw/results/",
#                    filename = "speciesLink-20240507090342-0004989")
df <- read.csv("data-raw/results/speciesLink-20240507090342-0004989.csv")
## Loading the data downloaded from their web interface
# df <- as.data.frame(data.table::fread("data-raw/results/speciesLink-20240507090342-0004989.txt"))

## Filtering
# field_names <- read.csv("data-raw/dictionaries/fieldNames.csv")
# col.names <- field_names$speciesLink[!is.na(field_names$type)]
# col.names <- col.names[!is.na(col.names)]
# col.names <- col.names[!duplicated(col.names)]
# df.names <- names(df)[names(df) %in% col.names]
example_intro <- df

## Fixing non-ASCII
col_to_check <- c("country","stateprovince","county","locality",
                  "occurrenceremarks", "recordedby",
                  "identifiedby", "recordnumber",
                  "scientificnameauthorship",
                  "verbatimlatitude", "verbatimlongitude",
                  "continentocean", "phylum", "preparationtype")
for(i in seq_along(col_to_check)) {
  Encoding(example_intro[[col_to_check[i]]]) <- "latin1"
  example_intro[[col_to_check[i]]] <- iconv(example_intro[[col_to_check[i]]],
                                      "latin1","UTF-8")
}

## Saving
usethis::use_data(example_intro,
                  overwrite = TRUE,
                  internal = FALSE,
                  compress = "xz")
