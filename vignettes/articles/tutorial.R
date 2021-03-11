## Tutorial
spp <- c("Trema micrantha",
         "Casearia sylvestris",
         "Euterpe edulis")
#example <- rgbif2(species =  spp)
example <- rspeciesLink(species =  spp,
                        Scope = "plants",
                        basisOfRecord = "PreservedSpecimen",
                        Synonyms = "flora2020")
occs <- fixField(example$data, origin = "splink")
occs <- formatOcc(occs)

##Add a function/step to format collectionCodes
saveRDS(occs, "tmp.rds")
occs <- readRDS("tmp.rds")
occs <- fixLoc(occs, admin.levels = c("country","stateProvince","municipality","locality"), scrap = TRUE)
locs <- strLoc(occs)
locs$loc.string <- prepLoc(locs$loc.string) # priority string
locs$loc.string1 <- prepLoc(locs$loc.string1) # alternative string
locs$loc.string2 <- prepLoc(locs$loc.string2) # alternative string
locs <- getLoc(locs, gazet = "plantR")
occs <- cbind.data.frame(occs,  locs[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")])
occs <- prepCoord(occs)
occs <- getCoord(occs)
occs <- validateLoc(occs)
occs <- prepSpecies(occs)
occs <- formatSpecies(occs, db = c("bfo", "tpl"), sug.dist = 0.85)
occs$scientificName.new <- occs$suggestedName
occs <- formatFamily(occs)
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
spp <- c("Trema micrantha",
         "Casearia sylvestris",
         "Euterpe edulis")
occs_splink1 <- rspeciesLink(species = spp)
occs_gbif1 <- rgbif2(species = spp)
occs_splink <- rspeciesLink(species = "Euterpe edulis")
occs_gbif <- rgbif2(species = "Euterpe edulis")
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






