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
occs_bien <- BIEN::BIEN_occurrence_species(
  spp,
  cultivated = T,
  only.new.world = F,
  all.taxonomy = T,
  native.status = T,
  natives.only = F,
  observation.type = T,
  political.boundaries = T,
  collection.info = T
)
write.csv(occs_bien, "data-raw/results/bien.csv")
table(occs_bien$datasource)

# Reading data base results ----------------------------------------------------
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

## species link (WEB based version) - INCLUDE THIS ONE
df_splinkw <- read.delim("data-raw/results/speciesLink-20211105124052-0021061.txt")
#https://specieslink.net/search/download/20211105124052-0021061
 
### Necessary fields for plantR
must_plantr <- c("institutionCode", "collectionCode", "catalogNumber",
                 "recordNumber", "recordedBy",
                 "year", "country", "stateProvince", "municipality", "locality",
                 "decimalLatitude", "decimalLongitude",
                 "identifiedBy", "dateIdentified",
                 "typeStatus", "family", "scientificName", "scientificNameAuthorship")
### Optional but recommended fields for plantR
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
low_must <- tolower(must_plantr)
low_opt <- tolower(opt_plantr)
plantr <- data.frame(plantr = c(must_plantr, opt_plantr),
                     low_dwc = c(low_must, low_opt),
                     type = c(rep("required", length(must_plantr)),
                              rep("optional", length(opt_plantr))))


# Bien Data dictionary ----------------------------------------------------
df_bien <- read.csv("data-raw/results/bien.csv")

# from: https://bien.nceas.ucsb.edu/bien/biendata/bien-4/private-data-dictionary/https://www.tdwg.org/community/osr/
urlfile <- "http://bien.nceas.ucsb.edu/bien/db/csv_data_dict.php"
url <- readLines(urlfile)[-c(1:3)]
url <- strsplit(url, split = ',\"', perl = TRUE)
df <- data.frame(table_name = as.character(sapply(url, head, 1)),
                 description = as.character(sapply(url, tail, 1)),
                 stringsAsFactors = FALSE)
df$description <- gsub('\"$|', "", df$description, fixed = TRUE)


### Necessary fields for plantR
must_bien <- c(institutionCode = "custodial_institution_codes",
               collectionCode = "collection_code",
               catalogNumber = "catalog_number",
               recordNumber = "record_number",
               recordedBy = "recorded_by",
               eventDate = "date_collected",
               country = "country",
               stateProvince = "state_province",
               county = "county",
               locality = "locality",
               decimalLatitude = "latitude",
               decimalLongitude = "longitude",
               identifiedBy = "identified_by",
               dateIdentified= "date_identified",
               # typeStatus = "typeStatus",
               family = "family_matched",
               scientificName = "name_matched",
               scientificName = "scrubbed_species_binomial",
               scientificNameAuthorship = "name_matched_author")
### Optional but recommended fields for plantR
opt_bien <- c(genus = "scrubbed_genus",
              #acceptedNameUsage = "scrubbed_species_binomial", # not sure...
              datasource = "datasource",
              datasetName = "dataset",
              ownerInstitutionCode = "dataowner",
              datasetID = "datasource_id",
              identificationRemarks = "identification_remarks",
              # "is_new_world",
              basisOfRecord = "observation_type",
              #"verbatim_family",
              #originalNameUsage = "verbatim_scientific_name", # not sure...
              higherClassification = "higher_plant_group",
              taxonomicStatus = "scrubbed_taxonomic_status"
              # "scrubbed_family",
              # "scrubbed_author"
              # "native_status", # occurrenceRemarks? fieldNotes?
              # "native_status_reason",
              # "native_status_sources",
              # "is_introduced",
              # "native_status_country",
              # "native_status_state_province",
              # "native_status_county_parish",
              # "is_cultivated_observation",
              # "is_cultivated_in_region",
              # "is_location_cultivated"
              )
low_must1 <- tolower(names(must_bien))
low_opt1 <- tolower(names(opt_bien))
# cols <- c("term_localName", "label", "status", "issued", "definition")
# dwc_dic[grepl("Name",dwc_dic$term_localName) &
#           !grepl("deprecated|superseded",dwc_dic$status), cols]

# The BIEN-DWC data dictionary
bien.dd <- data.frame(bien = c(must_bien, opt_bien),
                     low_dwc = c(low_must1, low_opt1))


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

# Adding bien equivalences ----------------------------------------------
cols_bien <- c(must_bien, opt_bien)
low_bien <- tolower(names(c(must_bien, opt_bien)))

bien <- data.frame(low_dwc = low_bien, bien = cols_bien) %>%
  left_join(df, ., by = "low_dwc")

# Adding gbif equivalences -----------------------------------------------------
cols_gbif <- names(df_gbif)
low_gbif <- tolower(cols_gbif)

fieldNames <- data.frame(low_dwc = low_gbif, gbif = cols_gbif) %>%
  left_join(splink, ., by = "low_dwc") %>%
  left_join(bien, ., by = c("dwc", "low_dwc","plantr","type")) %>%
  left_join(dwc, ., by = c("dwc", "low_dwc"))

# Missing fields
#Renato: adicionando as opcionais, a lista aumenta. Problema? Acho que não.
setdiff(low_must, low_gbif) # "scientificnameauthorship"
setdiff(low_must, low_splink)  # "municipality", "dateidentified"
setdiff(low_must, low_bien)  # "year", "municipality", "typestatus"

#### CODES ADDED BY RENATO ####
# Adding optional fields in speciesLink missing from the standard names from Darwin Core ------------------------------
tmp <- plantr[!plantr$plantr %in% fieldNames$plantr, ]
names(fieldNames)[!names(fieldNames) %in% names(tmp)]
tmp$dwc <- tmp$speciesLink <- tmp$plantr
tmp$gbif <- NA
tmp$bien <- NA
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
rm(fieldNames)
