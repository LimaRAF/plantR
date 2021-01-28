# Script to generate fieldNames table

library(dplyr)

# Reading data base results ----------------------------------------------------
## gbif
df_gbif <- read.csv("vignettes/results/gbif.csv")
## species link
df_splink <- read.csv("vignettes/results/speciesLink.csv")

# Necessary fields for plantR --------------------------------------------------
must_plantr <- c("collectionCode", "catalogNumber", "recordNumber", "recordedBy",
                 "year", "country", "stateProvince", "county", "municipality",
                 "decimalLatitude", "decimalLongitude", "identifiedBy", "dateIdentified",
                 "typeStatus", "scientificName", "scientificNameAuthorship", "institutionCode")
low_must <- tolower(must_plantr)
plantr <- data.frame(plantr = must_plantr,
                     low_dwc = low_must)

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

splink <- data.frame(low_dwc = low_splink, speciesLink = cols_splink) %>%
  left_join(df, ., by = "low_dwc")

# Adding gbif equivalences -----------------------------------------------------
cols_gbif <- names(df_gbif)
low_gbif <- tolower(cols_gbif)

fieldNames <- data.frame(low_dwc = low_gbif, gbif = cols_gbif) %>%
  left_join(splink, ., by = "low_dwc") %>%
  left_join(dwc, ., by = c("dwc", "low_dwc"))

# Missing fields
setdiff(low_must, low_gbif) #"scientificnameauthorship"
setdiff(low_must, low_splink)  #"municipality", "dateidentified"

write.csv(fieldNames,
          "data-raw/dictionaries/fieldNames.csv",
          row.names = FALSE)
