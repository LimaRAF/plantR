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

####validate coord
occs1 <- validateCoord(occs) #vai criar as colunas country, state, county e dizer se está certo
#source("R/checkBorder.R")
occs2 <- checkBorders(occs1)
#source("R/checkInverted.R")
occs3 <- checkInverted(occs2)
#source("R/fixInverted.R")
occs4 <- fixInverted(occs3)
##as coordenadas corrigidas deveriam passar de novo por validateCoord mas estou pulando esse loop de confirmação
#aqui demora mas vai
occs5 <- checkSea(occs4,
                   lat = "decimalLatitude.new",
                   lon = "decimalLongitude.new")
##estou checando visualmente aqui
library(tmap)
check_sea <- occs5 %>%
  filter(!is.na(sea.shore.check)) %>% #nao precisa checar tudo
  st_as_sf(coords = c( "decimalLongitude.new", "decimalLatitude.new"))

tmap_mode("view")
tm_shape(check_sea) +
  tm_dots(col = "sea.shore.check", size = 0.1 ,
          popup.vars = c("country", "inv.check"))

##o que NAO fiz foi decidir como juntar as colunas novas numa coluna única. ou decidir se é uma função extra que faz isto:
occs <- occs5 %>%
  dplyr::mutate(final_check = dplyr::case_when(
    geo.check == "coord_original/ok_country/ok_state/ok_county" ~ "ok",
    geo.check == "coord_original/ok_country/ok_state" ~ "ok",
    geo.check == "coord_original/ok_country" ~ "ok",
    geo.check == "coord_original/ok_country/estado_bad" ~ "check_gazetteer",
    geo.check == "coord_original/ok_country/estado_bad/county_bad" ~ "check_gazetteer",
    geo.check == "coord_original/ok_country/estado_bad/ok_county" ~ "check_gazetteer",
    geo.check == "coord_original/ok_country/ok_state/county_bad" ~ "check_gazetteer",
    inv.check == "inverted_lat" ~ "inverted",
    inv.check == "inverted_lon" ~ "inverted",
    inv.check == "inverted_both" ~ "inverted",
    inv.check == "transposed" ~ "inverted",
    inv.check == "transposed_inv_lat" ~ "inverted",
    inv.check == "transposed_inv_both" ~ "inverted",
    border.check == "check_borders" ~ "check_borders",
    sea.shore.check == "sea" ~ "sea",
    sea.shore.check == "shore" ~ "shore",
    sea.shore.check == "land" ~ "land",
    border.check == "check_inverted" ~ "check_inverted",
    geo.check == "no_cannot_check" ~ "no",
    geo.check == "coord_gazet" ~ "no",
    geo.check == "coord_original" ~ "falta"
  ))
###podemos discutir estas categorias e se vai numa função nova. a ordem do case_when importa
#falta [] renomear o que chamo hoje de validateCoord
#escrever o verdadeiro validateCoord que chamaria essa função e as seguintesb

###
tmp <- c("ok_county", "ok_county_close","ok_locality_gazet","ok_county_gazet",
         "ok_state", "ok_state_gazet", "ok_country", "ok_country_gazet", NA_character_)
probs <- c(0.35,0.05,0.05,0.3,0.05,0.1,0.05,0.01,0.04)
occs$geo.check = sample(tmp, dim(occs)[1], prob = probs, replace = TRUE)
occs <- mergeDup(occs)
saveRDS(occs, "tmp2.rds")
occs <- readRDS("tmp2.rds")
occs <- getCult(occs)



##Vinheta
occs_splink <- rspeciesLink(species = "Euterpe edulis")
occs_gbif <- rgbif2(species = "Euterpe edulis")
occs.bind <- formatDwc(splink_data = occs_splink,
                  gbif_data = occs_gbif)
occs0 <- formatOcc(occs.bind)
occs <- formatLoc(occs0)
occs <- formatCoord(occs)
occs <- formatTax(occs)
occs <- validateLoc(occs)
occs <- validateTax(occs)
occs <- validateDup(occs)
summ <- summaryData(occs)
flags <- summaryFlags(occs)
checkList(occs, n.vouch = 10, type = "short")






