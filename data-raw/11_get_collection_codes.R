
# Getting collectionCode from multiple aggregators ------------------------


#### Get collection codes from GBIF's GRSciColl ####
# Reference: https://scientific-collections.gbif.org/
# file_path <- here::here("data", "raw-data", "GBIF")
file_path <- tempdir()
# if(!dir.exists(file_path))
#  dir.create(file_path)

file_name <- "GBIF_SciCol.tsv"
url <- paste0("https://api.gbif.org/v1/grscicoll/collection/export?format=TSV&displayOnNHCPortal=true")
options(timeout = 1000)
download.file(url, file.path(file_path, file_name), mode = "wb")
gbif_col <- readr::read_tsv(file.path(file_path, file_name))

# Get only potential herbaria
to_search <- c("plants", "plantas", "herbário", "herbarium", "herbaria", "herbario",
               "museu", "museo", "museum")
to_search <- paste(to_search, collapse = "|")

not_to_search <- c("paleonto", "mineral", "antropol", "archeol")
not_to_search <- paste(not_to_search, collapse = "|")

# Subset
gbif_col <- gbif_col[grepl(to_search, gbif_col$name, ignore.case = TRUE) &
                       !grepl(not_to_search, gbif_col$name, ignore.case = TRUE), ]

# Removing potential animal collections
to_search <- c("invert", "zoo", "vertebr", "herperto", "arthro", "faun", "entomo",
                  "anfibi", "amphib", "ictio", "fish", "bird", "crustac", "mullusc",
                  "mammal", "reptil")
to_search <- paste(to_search, collapse = "|")

# From name column
gbif_col <- gbif_col[!grepl(to_search, gbif_col$name, ignore.case = TRUE), ]
# From description column
gbif_col <- gbif_col[!grepl(to_search, gbif_col$description, ignore.case = TRUE), ]
# From address column (Zoology departaments etc.)
gbif_col <- gbif_col[!grepl(to_search, gbif_col$address, ignore.case = TRUE), ]

# Get codes and institutions
gbif_col <- gbif_col[, c("institution_name", "name", "code")]
names(gbif_col)[which(names(gbif_col) == "institution_name")] <- "organization"
names(gbif_col)[which(names(gbif_col) == "name")] <- "name"
# names(gbif_col)[which(names(gbif_col) == "code")] <- "my_collectionCode"

# Add source column
gbif_col$source <- "GBIF"



#### Get collection codes from speciesLink ####
url <- "https://specieslink.net/col/?per_page=200&text=herb%C3%A1rio"

html_content <- rvest::read_html(url)
html_content2 <- rvest::html_nodes(html_content, "h4")
html_content2 <- rvest::html_text(html_content2, trim = TRUE)

# Remove subcoleções
remove_these <- which(html_content2 %in% "Subcoleções")
html_content2 <- html_content2[-remove_these]

# Getting codes and institutions
splink_codes <- c()

split_content <- strsplit(html_content2, " - ")

# Get collection and institution codes
spLink_col <- do.call(rbind, lapply(split_content, function(x) {
  data.frame(my_collectionCode = x[1],
             my_collectionName = x[2],
             source = "speciesLink", stringsAsFactors = FALSE)
}))




#### Get collection codes from JABOT ####
url <- "https://jabot.jbrj.gov.br/v3/herbarios.php"
html_content <- RCurl::getURLContent(url)

# Convert to text
txt <- strsplit(html_content, "\\n")[[1]]
txt <- txt[grepl("MBM", txt)]

# Get single ' '
# gregexpr get the position and length of each match
# "        ['RB', 813551],['MBM', 343090] 'MBM' on position 25 and has length 5
# Then regmatches uses the position and length to extract those exact strings
matches <- regmatches(txt, gregexpr("'(.*?)'", txt))

# Finally we gsub single ' by blank space
JABOT_col <- unlist(lapply(matches, function(x) gsub("'", "", x)))

JABOT_col <- data.frame(my_collectionCode = JABOT_col,
                        source = "JABOT")


#### Get collection codes from REFLORA ####
# https://reflora.jbrj.gov.br/reflora/herbarioVirtual

url <- "https://reflora.jbrj.gov.br/reflora/herbarioVirtual/ConsultaPublicoHVUC/ConsultaPublicoHVUC.do"
html_content <- rvest::read_html(url, encoding = "ISO-8859-1")
html_content <- rvest::html_element(html_content, "#textoIntrodutorio")
html_content <- rvest::html_text(html_content, trim = TRUE)
text <- htm2txt::htm2txt(html_content)

# Unable to automatically fix the encodings, so...
to_sub <- c("&aacute;", "&eacute;", "&ocirc;", "&acirc;","&iacute;",
            "&ccedil;", "&atilde;", "&ecirc;", "&otilde;", "&oacute;", "&rsquo;", "&uacute;")

to_sub1 <- c("á", "é", "ô", "â", "í", "ç", "ã", "ê", "õ", "ó", "'", "ú")

for(i in 1:length(to_sub)){
  text <- gsub(to_sub[i], to_sub1[i], text)
}

# Some splitting and regex...
text <- unlist(strsplit(text, "são elas:"))[2]
text <- unlist(strsplit(text, ".\r"))[1]
text <- gsub(" e Naturhistorisches", ", Naturhistorisches", text)
text <- gsub("Royal Botanic Gardens, Kew", "Royal Botanic Gardens Kew", text)
text <- gsub("Muséum national d'histoire naturelle, Paris", "Muséum national d'histoire naturelle Paris", text)
text <- trimws(text)

# Find and replace text inside parentheses
pattern <- "\\(([^)]+)\\)"
matches <- gregexpr(pattern, text)
matched_texts <- regmatches(text, matches)
matched_texts <- unlist(matched_texts)

replacement <- gsub(",", ";", matched_texts)

text2 <- text
for (i in seq_along(matched_texts)) {
  text2 <- gsub(matched_texts[i], replacement[i], text2, fixed = TRUE)
}

text2 <- unlist(strsplit(text2, ", "))

# To populate
institutions <- c()
collection_codes <- c()

# Loop through each entry
for (i in text2) {
  # Extract institution name
  institution <- sub("\\s*\\(.*", "", i)

  # Extract collection codes
  codes <- sub(".*\\((.*)\\)", "\\1", i)
  codes <- unlist(strsplit(codes, ";\\s*")) # Split codes by ';' and trim spaces

  # Append results
  institutions <- c(institutions, rep(institution, length(codes)))
  collection_codes <- c(collection_codes, codes)
}

# Create a data frame
REFLORA_col <- data.frame(my_institutionName = institutions,
                          my_collectionCode = collection_codes,
                          source = "REFLORA",
                          stringsAsFactors = FALSE)


#### Get collection codes from BIEN ####
url <- "https://bien.nceas.ucsb.edu/bien/data-contributors/herbaria/"
html_content <- rvest::read_html(url)
html_content <- rvest::html_element(html_content, ".hentry")
html_content <- rvest::html_text(html_content, trim = TRUE)

text <- unlist(strsplit(html_content, "\n"))

# Remove the first unwanted lines
text <- text[5:length(text)]

# Matrix by columns (Acronym, Institution, City, Country)
BIEN_col <- matrix(text, ncol = 4, byrow = TRUE)

# Get relevant columns and data.frame it
BIEN_col <- data.frame(BIEN_col[-c(1:2), c(1:2)])
names(BIEN_col) <- c("my_collectionCode", "my_institutionName")
BIEN_col$source <- "BIEN"

# Keep only unique
BIEN_col <- BIEN_col[!duplicated(BIEN_col$my_collectionCode), ]



#### Get collection codes from SiBBr ####
# https://collectory.sibbr.gov.br/collectory/
# I haven't found a way to automatize this step via API or scrapping
file_path <- here::here("data", "raw-data", "SiBBr")
file_name <- "SiBBr_collections.txt"

SiBBr_col <- read.delim(file.path(file_path, file_name))

# Build collection code from names
pattern <- gregexpr("\\(([^)]+)\\)", SiBBr_col$my_collectionCode)
pattern <- regmatches(SiBBr_col$my_collectionCode, pattern)

# Remove the parentheses
pattern_clean <- gsub("[()]", "", unlist(pattern))



# Now get collectionName
pattern <- gregexpr("[^()]+", SiBBr_col$my_collectionCode)
pattern <- regmatches(SiBBr_col$my_collectionCode, pattern)

SiBBr_col <- do.call(rbind, lapply(pattern, function(x) {
  data.frame(my_collectionName = trimws(x[1]),
             my_collectionCode = trimws(x[2]),
             source = "speciesLink", stringsAsFactors = FALSE)
}))



#### Aggregate all collections ####
all_collections <- plyr::rbind.fill(BIEN_col,
                                    gbif_col,
                                    JABOT_col,
                                    REFLORA_col,
                                    #SiBBr_col,
                                    spLink_col)

# First, aggregate collectionCode by source
aggregated_collections <- aggregate(source ~ my_collectionCode, data = all_collections,
                                    FUN = function(x) paste(unique(na.omit(x)), collapse = "|"))

# Get first non-na value since not all columns are present in all db
institution_name <- sapply(aggregated_collections$my_collectionCode,
                           function(x) {
                             non_na_values <- all_collections$my_institutionName[all_collections$my_collectionCode == x]
                             non_na_values <- non_na_values[!is.na(non_na_values)]
                             return(non_na_values[1])
                           })

# Same here
collection_name <- sapply(aggregated_collections$my_collectionCode,
                          function(x) {
                            non_na_values <- all_collections$my_collectionName[all_collections$my_collectionCode == x]
                            non_na_values <- non_na_values[!is.na(non_na_values)]
                            return(non_na_values[1])
                          })

# Now add the info to the final db
aggregated_collections$my_institutionName <- institution_name
aggregated_collections$my_collectionName <- collection_name

# There is some encoding issues in my_collectionName column
all_collections <- aggregated_collections



# Complementing plantR collection codes -----------------------------------

#### Get collection codes from plantR ####
plantR_col <- read.csv("data-raw/raw_dictionaries/collectionCodes.csv")

# Which collections we have that plantR don't
plantR_missing <- all_collections$my_collectionCode[!all_collections$my_collectionCode %in% plantR_col$index.herbariorum.or.working.code]

# Append these to plantR
# Get last ID from plantR
last_id <- max(plantR_col$ordem.colecao)

# Data to append
to_append <- all_collections[all_collections$my_collectionCode %in% plantR_missing, ]

# Generate new unique IDs for the new rows
new_ids <- (last_id + 1):(last_id + nrow(to_append))

# Add the new unique IDs to the new rows
to_append$ordem.colecao <- new_ids

# Rename code column from IH to match plantR_herbaria
colnames(to_append)[which(colnames(to_append) == "my_collectionCode")] <-
  "index.herbariorum.or.working.code"

# Add a col to identify these new additions
to_append$Guilherme_updatedDate <- Sys.time()

# Fill
plantR_complete <- plyr::rbind.fill(plantR_col, to_append) # from 4440 to 5213 collections
# length(unique(plantR_complete$index.herbariorum.or.working.code))



# Get information from Index Herbariorum ----------------------------------

#### Get the Index Herbariorum collections ####
url <- "http://sweetgum.nybg.org/science/api/v1/institutions/"
ih_herbaria_full <- jsonlite::fromJSON(url)$data

# Flattening nested data frames
for (col in colnames(ih_herbaria_full)) {
  # Detect if a column is a dataframe
  if (is.data.frame(ih_herbaria_full[[col]])) {
    # Separate it
    nested_df <- ih_herbaria_full[[col]]
    # Create a new column for each nested df column
    nested_colnames <- paste(col, colnames(nested_df), sep = "_")
    colnames(nested_df) <- nested_colnames
    # Bind it back and remove the previous nested df
    ih_herbaria_full <- cbind(ih_herbaria_full, nested_df)
    ih_herbaria_full[[col]] <- NULL
  }
}

ih_herbaria <- ih_herbaria_full

#### from IH, remove the following: ####
# # Inactive or permanently closed collections
# ih_herbaria <- ih_herbaria[!ih_herbaria$currentStatus %in% c("Inactive", "Permanently closed"), ]
#
# # Very small collections
# ih_herbaria <- ih_herbaria[ih_herbaria$specimenTotal >= 5000, ]
# # Collections that are not in the Americas (but keep those that culturally sample here)
# # Vector of wanted countries plus those that usually sample here
# country_names <- c("Anguilla", "Antigua and Barbuda", "Argentina", "Aruba",
#                    "Bahamas", "Barbados", "Belize", "Bermuda",
#                    "Bolivia, Plurinational State of", "Bolivia",
#                    "Bonaire, Sint Eustatius and Saba", "Brazil",
#                    "Cayman Islands", "Chile", "Colombia", "Costa Rica",
#                    "Cuba", "Curacao", "Dominica", "Dominican Republic",
#                    "Ecuador", "El Salvador", "French Guiana", "Grenada",
#                    "Guadeloupe", "Guatemala", "Guyana", "Haiti", "Honduras",
#                    "Jamaica", "Martinique", "Mexico", "Montserrat",
#                    "Nicaragua", "Panama", "Paraguay", "Peru", "Puerto Rico",
#                    "Saint Barthelemy", "Saint Kitts and Nevis", "Saint Lucia",
#                    "Saint Martin", "Saint Vincent and the Grenadines",
#                    "Sint Maarten", "Suriname", "Trinidad and Tobago",
#                    "Turks and Caicos Islands", "Uruguay",
#                    "Venezuela, Bolivarian Republic of", "Venezuela",
#                    "British Virgin Islands", "U.S. Virgin Islands",
#                    "Cocos (Keeling) Islands", "Amazon", "United States",
#                    "Canada", "U.S.A", "U.K.", "Germany", "Belgium", "",
#                    "Switzerland", "France", "Netherlands", "")
#
# ih_herbaria <- ih_herbaria[ih_herbaria$address_physicalCountry %in% country_names, ]

# Any of these collections are not in our data set?
missing_collections <- ih_herbaria$code[!ih_herbaria$code %in% plantR_complete$index.herbariorum.or.working.code]
# Nothing relevant

# Any collections that we have are not in IH?
missing_from_ih <- plantR_complete$index.herbariorum.or.working.code[!plantR_complete$index.herbariorum.or.working.code %in% ih_herbaria$code]


#### PRECISAMOS MELHORAR ISSO.
# A SAIDA DOS NOVOS TEM QUE SAIR NO MESMO FORMATO QUE A collectionCodes (SÓ COPIAR E COLAR)
# PARA OS QUE JÁ TINHAMOS, VER SE TEM UPDATE NO IH (SE EXISTE AGORA E SE TEM CAMPOS NOVOS PREENCHIDOS)

# Merge our db with information from IH
cols <- c("code", "organization", "address_physicalCountry", "address_physicalState")
ih_herbaria <- unique(ih_herbaria[, cols])
ih_herbaria <- ih_herbaria[!ih_herbaria$code %in% "",]
dup_codes <- ih_herbaria$code[duplicated(ih_herbaria$code)]
if (length(dup_codes) > 0) {
  # ih_herbaria[ih_herbaria$code %in% dup_codes, ]
  ih_herbaria <- ih_herbaria[!duplicated(ih_herbaria$code), ]
}

# Get last ID from plantR
last_id <- max(plantR_complete$ordem.colecao)
new_ids <- (last_id + 1):(last_id + nrow(ih_herbaria))
ih_herbaria$ordem.colecao <- new_ids
to_append$Guilherme_updatedDate <- Sys.time()
ih_herbaria$source <- "IH"

plantR_final <- dplyr::full_join(plantR_complete, ih_herbaria,
                            by = dplyr::join_by(index.herbariorum.or.working.code == code))
names(plantR_final) <- gsub(".y$", "", names(plantR_final))
col_names <- names(plantR_final)[match(names(plantR_col), names(plantR_final), nomatch = 0 )]
col_names <- na.omit(unique(c(col_names, names(plantR_final)[!names(plantR_final) %in% col_names])))
plantR_final <- plantR_final[, col_names]

# All new codes to be included
rep_these <- is.na(plantR_final$ordem.colecao)
if (any(rep_these))
  plantR_final$ordem.colecao[rep_these] <-
    plantR_final$ordem.colecao.x[rep_these]

plantR_new_codes <-
  plantR_final[!plantR_final$index.herbariorum.or.working.code %in% plantR_col$index.herbariorum.or.working.code,]
write.csv(plantR_new_codes, "data-raw/results/new_collection_codes.csv",
          fileEncoding = "UTF-8")

# IH info to be included in the file
plantR_final1 <- dplyr::full_join(plantR_col, ih_herbaria,
                                 by = dplyr::join_by(index.herbariorum.or.working.code == code))
names(plantR_final1) <- gsub(".y$", "", names(plantR_final1))
col_names <- names(plantR_final1)[match(names(plantR_col), names(plantR_final1), nomatch = 0 )]
col_names <- na.omit(unique(c(col_names, names(plantR_final1)[!names(plantR_final1) %in% col_names])))
plantR_final1 <- plantR_final1[, col_names]
plantR_old_codes <-
  plantR_final1[plantR_final$index.herbariorum.or.working.code %in% plantR_col$index.herbariorum.or.working.code,]
write.csv(plantR_old_codes, "data-raw/results/old_collection_codes.csv",
          fileEncoding = "UTF-8")














