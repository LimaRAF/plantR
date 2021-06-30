# Script to generate fieldNames table
library(dplyr)

# Downloading current versions of GBIF and speciesLink fiedls
spp <- c("Casearia sylvestris",
         "Euterpe edulis",
         "Trema micrantha")
occs_splink <- rspeciesLink(species = spp)
write.csv(occs_splink, "data-raw/results/speciesLink.csv")
occs_gbif <- rgbif2(species = spp)
write.csv(occs_gbif, "data-raw/results/gbif.csv")

# Reading data base results ----------------------------------------------------
### SARA: VER SE HÁ UM EFEITO NO NÚMERO DE COLUNAS ENCONTRADAS DEPENDENDO DA
#ESPÉCIE USADA PARA O DOWNLOAD. SE SIM, DAR PREFERÊNCIA A UMA ESPÉCIES COM MAIS
#REGISTRO QUE EUTERPE EDULIS
## gbif
df_gbif <- read.csv("data-raw/results/gbif.csv")

#editing gbif names (INCLUIDO PELO RENATO)
tmp <- names(df_gbif)[grepl("\\.\\.", names(df_gbif), perl = TRUE)]
tmp1 <- sapply(tmp,
               function(x) tail(unlist(strsplit(x, "([a-z])\\.(?=[a-zA-Z])", perl = TRUE)),1))
tmp[!duplicated(tmp1) & !tmp1 %in% names(df_gbif)] <-
  tmp1[!duplicated(tmp1) & !tmp1 %in% names(df_gbif)]
names(df_gbif)[grepl("\\.\\.", names(df_gbif), perl = TRUE)] <- tmp


## species link
df_splink <- read.csv("data-raw/results/speciesLink.csv")

# Necessary fields for plantR --------------------------------------------------
must_plantr <- c("institutionCode", "collectionCode", "catalogNumber",
                 "recordNumber", "recordedBy",
                 "year", "country", "stateProvince", "municipality", "locality",
                 "decimalLatitude", "decimalLongitude",
                 "identifiedBy", "dateIdentified",
                 "typeStatus", "family", "scientificName", "scientificNameAuthorship")
# Optional but recommended fields for plantR --------------------------------------------------
opt_plantr <- c("genus", "acceptedScientificName",
                "taxonRank", "taxonomicStatus", #"taxonRemarks",
                "eventDate", "month", "day", "verbatimEventDate",
                "dayIdentified", "monthIdentified", "yearIdentified", "identificationRemarks",
                "countryCode", "county", "verbatimLocality",
                "verbatimLatitude", "verbatimLongitude", "coordinatePrecision", "verbatimElevation",
                "basisOfRecord", "type",
                "fieldNotes", "occurrenceRemarks", "habitat", #"occurrenceDetails",
                "collectionID", "datasetID", "datasetName",
                "bibliographicCitation")
### SARA: ACRESCENTEI A COLUNA 'type', AS OPCIONAIS AGORA SÃO TRATADAS À PARTE EM formatDwc()
low_must <- tolower(must_plantr)
low_opt <- tolower(opt_plantr)

plantr <- data.frame(plantr = c(must_plantr, opt_plantr),
                     low_dwc = c(low_must, low_opt),
                     type = c(rep("required", length(must_plantr)),
                              rep("optional", length(opt_plantr))))


# Standard names from Darwin Core ----------------------------------------------
# from repository: https://github.com/tdwg/dwc
dwc_dic <- read.csv("https://raw.githubusercontent.com/tdwg/dwc/master/vocabulary/term_versions.csv")

dwc <- dwc_dic %>%
  select(term_localName, definition, organized_in) %>%
  filter(organized_in != "") %>%
  mutate(low_dwc = tolower(term_localName), dwc = term_localName) %>%
  select(dwc, low_dwc, definition) %>%
  distinct()
#%>%
#   mutate(organized_in = sapply(strsplit(organized_in, "/"),
#                                function(x) x[6]))
#
# dwc$organized_in[dwc$organized_in == ""] <- NA

# Creating base for fieldNames data frame -------------------------------------
## Column low_dwc will be always used for merge
df <- dwc %>%
  select(dwc, low_dwc) %>%
  left_join(plantr, by = 'low_dwc') %>%
  distinct()

# Adding speciesLink equivalences ----------------------------------------------
cols_splink <- names(df_splink)
low_splink <- tolower(cols_splink)

#checking
low_cols <- tolower(c(must_plantr, opt_plantr))
low_splink[!low_splink %in% low_cols]

splink <- data.frame(low_dwc = low_splink, speciesLink = cols_splink) %>%
  left_join(df, ., by = "low_dwc")

# Adding gbif equivalences -----------------------------------------------------
cols_gbif <- names(df_gbif)
low_gbif <- tolower(cols_gbif)

fieldNames <- data.frame(low_dwc = low_gbif, gbif = cols_gbif) %>%
  left_join(splink, ., by = "low_dwc") %>%
  left_join(dwc, ., by = c("dwc", "low_dwc"))

# Missing fields
#Renato: adicionando as opcionais, a lista aumenta. Problema? Acho que não.
setdiff(low_must, low_gbif) # "scientificnameauthorship"
setdiff(low_must, low_splink)  # "municipality", "dateidentified"

#### CODES ADDED BY RENATO ####
# Adding optional fields in speciesLink missing from the standard names from Darwin Core ------------------------------
tmp <- plantr[!plantr$plantr %in% fieldNames$plantr, ]
names(fieldNames)[!names(fieldNames) %in% names(tmp)]
tmp$dwc <- tmp$speciesLink <- tmp$plantr
tmp$gbif <- NA
tmp$definition <- "(extra fields from speciesLink database not in Darwin Core standards)"
tmp <- tmp[, match(names(fieldNames), names(tmp))]
fieldNames <- rbind.data.frame(fieldNames, tmp)

# Removing duplicated entries (all equal except the description)
dup.fields <- apply(fieldNames[, !names(fieldNames) %in% "definition"], 2, duplicated)
fieldNames <- fieldNames[!apply(dup.fields, 1, all),]

# Saving
write.csv(fieldNames,
          "data-raw/dictionaries/fieldNames.csv",
          row.names = FALSE)
