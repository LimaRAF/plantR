## Tutorial

# Data entry
spp <- c("Casearia sylvestris",
         "Euterpe edulis",
         "Trema micrantha")
occs_splink <- rspeciesLink(species =  spp,
                            Scope = "plants",
                            basisOfRecord = "PreservedSpecimen",
                            Synonyms = "flora2020")
occs_gbif <- rgbif2(species =  spp,
                    basisOfRecord = "PRESERVED_SPECIMEN",
                    remove_na = FALSE, limit = 500000)
nrow(occs_splink)
nrow(occs_gbif)
occs <- formatDwc(splink_data = occs_splink,
                  gbif_data = occs_gbif, drop = TRUE)
dim(occs)

# Data editing
occs <- getCode(occs)
occs$recordedBy.new <- prepName(occs$recordedBy,
                                output = "first",
                                sep.out = "; ")
occs$recordedBy.aux <- prepName(occs$recordedBy,
                                output = "aux",
                                sep.out = "; ")
occs$identifiedBy.new <- prepName(occs$identifiedBy,
                                  output = "first",
                                  sep.out = "; ")
occs$identifiedBy.aux <- prepName(occs$identifiedBy,
                                  output = "aux",
                                  sep.out = "; ")
occs$recordedBy.new <- missName(occs$recordedBy.new,
                                type = "collector", noName = "s.n.")
occs$identifiedBy.new <- missName(occs$identifiedBy.new,
                                  type = "identificator", noName = "s.n.")
occs$last.name <- lastName(occs$recordedBy.new)
occs$recordNumber.new <- colNumber(occs$recordNumber, noNumb = "s.n.")
occs$year.new <- getYear(occs$year, noYear = "n.d.")
occs$yearIdentified.new <- getYear(occs$dateIdentified, noYear = "n.d.")


occs <- fixLoc(occs,
               loc.levels = c("country","stateProvince","municipality","locality"),
               scrap = TRUE)
locs <- strLoc(occs)
locs$loc.string  <- prepLoc(locs$loc.string) # priority string
locs$loc.string1 <- prepLoc(locs$loc.string1) # alternative string
locs$loc.string2 <- prepLoc(locs$loc.string2) # alternative string
locs <- getLoc(locs, gazet = "plantR")
occs <- cbind.data.frame(occs, locs[, c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")])

occs <- prepCoord(occs)
occs <- getCoord(occs)

occs <- fixSpecies(occs)
occs <- prepSpecies(occs, db = c("bfo", "tpl"), sug.dist = 0.85)
occs$scientificName.new <- occs$suggestedName
occs <- prepFamily(occs)

occs <- validateLoc(occs)
unique(occs$loc.check)

occs <- checkCoord(occs,
                   keep.cols = c("geo.check", "NAME_0", "country.gazet"))
unique(occs$geo.check)




occs <- validateTax(occs)
dups <- prepDup(occs,
                comb.fields = list(c("family","col.last.name","col.number","municipality"),
                                   c("family","col.last.name","col.number","col.year")))
dups <- getDup(dups, flag.ind = TRUE)
occs <- cbind.data.frame(occs,
                         dups[,c("numTombo","dup.ID","dup.numb","dup.prop")],
                         stringsAsFactors = FALSE)
saveRDS(occs, "tmp1.rds")
occs <- readRDS("tmp1.rds")

occs <- mergeDup(occs)
saveRDS(occs, "tmp2.rds")
occs <- readRDS("tmp2.rds")
occs <- getCult(occs)



##Vinheta
devtools::load_all()
spp <- c("Trema micrantha",
         "Casearia sylvestris",
         "Euterpe edulis")
occs_splink1 <- rspeciesLink(species = spp)
occs_gbif1 <- rgbif2(species = spp)
# occs_splink <- rspeciesLink(species = "Euterpe edulis")
# occs_gbif <- rgbif2(species = "Euterpe edulis")
occs.bind <- formatDwc(splink_data = occs_splink1,
                  gbif_data = occs_gbif1)
occs0 <- formatOcc(occs.bind)
occs1 <- formatLoc(occs0)
occs1 <- formatCoord(occs1)
occs1 <- formatTax(occs1)
occs2 <- validateLoc(occs1)
occs3 <- validateCoord(occs2)
occs4 <- validateTax(occs3)
occs5 <- validateDup(occs4)
summ <- summaryData(occs5)
flags <- summaryFlags(occs5)
checkList(occs5, n.vouch = 10, type = "short")






