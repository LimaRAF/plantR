#### Script to read shapefiles, edit and generate the data/worldMap.rds ####
require(rgdal)
require(rgeos)
require(sf)
require(sp)
require(countrycode)
devtools::load_all()

##List of Neotropical countries
countries.latam <- c("Anguilla","Antigua and Barbuda","Argentina","Aruba","Bahamas",
                     "Barbados","Belize","Bermuda","Bolivia","Brazil","British Virgin Islands",
                     "Caribbean Netherlands","Cayman Islands","Chile",
                     "Colombia","Costa Rica","Cuba","Curaçao","Dominica","Dominican Republic",
                     "Ecuador","El Salvador","Falkland Islands",
                     "French Guiana","Grenada","Guadeloupe","Guatemala","Guyana","Haiti",
                     "Honduras","Jamaica","Martinique","Mexico","Montserrat","Nicaragua",
                     "Panama","Paraguay","Peru","Puerto Rico","Saint Barthélemy",
                     "Saint Kitts and Nevis","Saint Lucia","Saint Martin (French part)","Saint Vincent and the Grenadines",
                     "Sint Maarten","Suriname","Trinidad and Tobago","Turks and Caicos Islands",
                     "United States Virgin Islands","Uruguay","Venezuela")
iso3 <- countrycode::countrycode(countries.latam, "country.name", "iso3c")
#BES = "Caribbean Netherlands" = "Bonaire, Saint Eustatius and Saba"

################################################################################H
################################################################################H
################################
### WORLD MAP FOR VALIDATION ###
################################
##new sf files
destfolder <- "vrac/latam"
destfolder <- "data-raw/GDAM"
if (!exists(destfolder)) dir.create(destfolder, recursive = T)
source("https://raw.githubusercontent.com/liibre/Rocc/master/R/getGADM.R")

#Dando problema
# purrr::walk2(.x = rep(iso3, each = 4),
#              .y = rep(3:0, length(countries.latam)),
#              ~ getGADM(
#                cod = .x,
#                level = .y,
#                best = FALSE, #importante para ter outros niveis
#                destfolder = destfolder
#              ))
gadm_files <- list.files(destfolder, pattern = "rds$",
                         full.names = TRUE)

##Loading, editing and converting the world country shapefile
path <- "E://ownCloud//W_GIS"
#wo <- readOGR(dsn=paste(path,"//WO_ADM_limits_GADM36//gadm36_levels_shp",sep=""),layer="gadm36_0")
wo <- readRDS(paste(path,"//WO_ADM_limits_GADM36//gadm36_levels_shp//gadm36_0.rds",sep = ""))
#wo <- readRDS(paste0(path,"//WO_ADM_limits_GADM36//gadm36_levels_shp//gadm36_0.rds"))

#Editing country name as in the gazetteer
wo@data$pais <- countrycode(as.character(wo@data$GID_0),'iso3c', 'country.name')
wo@data$pais[is.na(wo@data$pais)] <- as.character(wo@data$NAME_0[is.na(wo@data$pais)])
wo@data$pais <- prepCountry(wo@data$pais)
# wo@data$pais <- tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), wo@data$pais))
# wo@data$pais <- tolower(textclean::replace_non_ascii(wo@data$pais))
# wo@data$pais <- stringr::str_replace_all(wo@data$pais,  " & ", " and ")
# wo@data$pais <- gsub("^st. ", "saint ", wo@data$pais)
# wo@data$pais <- gsub(" of the ", " ", wo@data$pais)
# wo@data$pais <- gsub(" of ", " ", wo@data$pais)

tmp1 <- replaceNames[replaceNames$class %in% "country" & apply(is.na(replaceNames[, 2:4]), 1, all), ]
tmp2 <- as.character(tmp1$replace)
names(tmp2) = as.character(tmp1$pattern)
#names(tmp2) <- gsub("\\\\", "", names(tmp2))
wo@data$pais <- sapply(strsplit(wo@data$pais,' \\('), function(x) x[[1]][1])
wo@data$pais <- stringr::str_replace_all(wo@data$pais, tmp2)
# wo@data$pais <- plantR::prepLoc(wo@data$pais)

## Comparing the map with the gazetteer
tmp <- merge(wo@data, gazetteer[,c("loc", "loc.correct")], by.x = "pais", by.y = "loc", all.x = TRUE)
tmp[!tmp$pais %in% tmp$loc.correct,]

#Transforming and simplifying
wo@data <- wo@data[,c("GID_0","pais")]
names(wo@data)[2] <- "NAME_0"
wo.simp <- gSimplify(wo, tol=0.001, topologyPreserve = TRUE)
wo.simp <- gBuffer(wo.simp, byid = TRUE, width = 0)
wo.simp1 <- SpatialPolygonsDataFrame(wo.simp, wo@data)
worldMap <- sf::st_as_sf(wo.simp1)

wo.simp2 <- gSimplify(wo, tol = 0.0001, topologyPreserve = TRUE)
wo.simp2 <- gBuffer(wo.simp2, byid = TRUE, width = 0)
wo.simp3 <- SpatialPolygonsDataFrame(wo.simp2, wo@data)
worldMap_0001 <- sf::st_as_sf(wo.simp3)

#wo.simp2 <- sf::st_buffer(wo.simp1, dist = 0)
worldMap_full <- sf::st_as_sf(wo)

#Inspecting
par(mfrow = c(2,2), mar=c(0,0,1,0))
plot(wo[1,], main="Aruba original")
plot(wo.simp[1,], border = "red", main="Aruba gSimp 0.001")
plot(st_geometry(worldMap[1,1]), border = "green", main = "Aruba gSimp 0.001 sf")

#Saving
save(worldMap, file = "./data/worldMap.rda", compress = "xz")
# save(worldMap_0001, file = "./data/worldMap_0001.rda", compress = "xz")
# save(worldMap_full, file = "./data/worldMap_full.rda", compress = "xz")


################################################################################H
################################################################################H
#########################################
### LATIN AMERICAN MAP FOR VALIDATION ###
#########################################

##Loading, editing and converting the world country shapefile
path0 <- "E://ownCloud//W_GIS//Am_Lat_ADM_GADM_v3.6//"
countries <- list.files(path0, full.name=TRUE)
countries <- countries[grep(paste(iso3, collapse = "|") ,countries)]
countries <- countries[grep("sf.rds",countries)]
countries3 <- countries[grep('_3_sf.rds',countries)]
adm3 <- sapply(strsplit(countries3, "//|_"), function(x) x[11])
countries2 <- countries[grepl('_2_sf.rds',countries) & !grepl(paste0(c(adm3), collapse = "|"), countries)]
adm2 <- sapply(strsplit(countries2, "//|_"), function(x) x[11])
countries1 <- countries[grepl('_1_sf.rds',countries) & !grepl(paste0(c(adm2,adm3), collapse = "|"), countries)]
adm1 <- sapply(strsplit(countries1, "//|_"), function(x) x[11])
countries0 <- countries[grepl('_0_sf.rds',countries)  & !grepl(paste0(c(adm1,adm2,adm3), collapse = "|"), countries)]
adm0 <- sapply(strsplit(countries0, "//|_"), function(x) x[11])

all.countries <- sort(c(adm1, adm2, adm3))
iso3[!iso3 %in% all.countries] # all of these countries are only available at ADM_0, that is, already in worldMap.
countries <- sort(c(countries1, countries2, countries3))

country.list <- vector('list', length(countries))
pais = NULL
# wo <- readRDS(gadm_files)
# wo <- purrr::map(gadm_files, .f = function(x) readRDS(file = x))

for (i in 1:length(country.list)) {
  path = countries[i]
  tmp = readRDS(path)
  #tmp1 = tmp@data
  tmp1 = tmp[,grepl("^NAME_", names(tmp))]

  #country
  tmp1$NAME_0 <- tolower(textclean::replace_non_ascii(tmp1$NAME_0))
  tmp1$NAME_0 <- stringr::str_replace_all(tmp1$NAME_0,  " & ", " and ")
  tmp1$NAME_0 <- gsub("^st. ", "saint ", tmp1$NAME_0)
  tmp1$NAME_0 <- gsub(" of the ", " ", tmp1$NAME_0)
  tmp1$NAME_0 <- gsub(" of ", " ", tmp1$NAME_0)

  toto1 <- replaceNames[replaceNames$class %in% "country" & apply(is.na(replaceNames[, 2:4]), 1, all), ]
  toto2 <- as.character(toto1$replace)
  names(toto2) = as.character(toto1$pattern)
  names(toto2) <- gsub("\\\\", "", names(toto2))
  tmp1$NAME_0 <- sapply(strsplit(tmp1$NAME_0,' \\('), function(x) x[[1]][1])
  tmp1$NAME_0 <- stringr::str_replace_all(tmp1$NAME_0, toto2)
  tmp1$NAME_0 <- plantR::prepLoc(tmp1$NAME_0)

  tmp1$NAME_0 <- gsub("bonaire, sint eustatius and saba", "caribbean netherlands", tmp1$NAME_0)
  tmp1$NAME_0 <- gsub("saint vincent and the grenadines", "saint vincent and grenadines", tmp1$NAME_0)
  tmp1$NAME_0 <- gsub("virgin islands, u.s", "united states virgin islands", tmp1$NAME_0)

  #ADM_1
  tmp1$NAME_1 <- tolower(textclean::replace_non_ascii(tmp1$NAME_1))
  tmp1$NAME_1 <- plantR::prepLoc(tmp1$NAME_1)

  if ("NAME_2" %in% names(tmp1)) {
    tmp1$NAME_2 <- tolower(textclean::replace_non_ascii(tmp1$NAME_2))
    tmp1$NAME_2 <- plantR::prepLoc(tmp1$NAME_2)
  }

  #Symplifying the maps
  # tmp.simp <- gSimplify(tmp, tol=0.0001, topologyPreserve = TRUE)
  # tmp.simp <- gBuffer(tmp.simp, byid=TRUE, width=0)
  tmp.simp <- st_simplify(tmp1, dTolerance = 0.0001, preserveTopology = TRUE)
  tmp.simp <- st_buffer(tmp.simp, dist=0)
  # tmp.simp <- tmp1

  ##Simplify transform MULTIPOLYGONS in POLYGON: is that a problem?

  #Extract polygon ID's
  # pid <- sapply(slot(tmp.simp, "polygons"), function(x) slot(x, "ID"))
  #
  # #Create dataframe with correct rownames and merging
  # p.df <- as.data.frame(tmp1, row.names = pid, stringsAsFactors = FALSE)
  # tmp.simp1 <- SpatialPolygonsDataFrame(tmp.simp, p.df)

  #Saving
  # pais[i] = unique(p.df$NAME_0)
  pais[i] = unique(tmp.simp$NAME_0)
  country.list[[i]] = tmp.simp
}
names(country.list) = pais
#pais[!pais %in% wo.simp1$NAME_0]

## Standardizing the shapefile and gazetteer names (for Brazil only)

#loading the gazetteer and removing possible duplicated localities
#dic <- read.csv("data-raw//raw//gazetteer_utf8-win.csv",as.is=TRUE)
dic <- read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//dictionaries//gazetteer.csv",as.is=TRUE)
dic <- dic[dic$status %in% "ok",]
dic <- dic[dic$resolution.gazetteer %in% c("country","state","county"),]

j=8
#for(j in 31:length(country.list)) {
#Creating the locality strings for the shapefile dataframe
tmp = country.list[[j]]#@data
#for(i in 1:dim(tmp)[2]) tmp[,i] <- as.character(tmp[,i])

tmp.df <- tmp
st_geometry(tmp.df) <- NULL
# tmp1= data.frame(loc=apply(tmp, 1, paste, collapse="_"))
ids <- names(tmp.df) %in% c("NAME_0","NAME_1","NAME_2")
tmp1= data.frame(loc=apply(tmp.df[, ids], 1, paste, collapse="_"),
                 stringsAsFactors = FALSE)
# tmp1$loc = as.character(tmp1$loc)

tmp$order = 1:dim(tmp)[1]
tmp1$order = 1:dim(tmp1)[1]

#merging the two sources of info
tmp2 <- merge(tmp1, dic[,c("NAME_0","NAME_1","NAME_2","loc","loc.correct")],
              by="loc", all.x=TRUE)
tmp3 <- strsplit(tmp2$loc.correct,"_")

nms <- c("country", "state", "county")
for(w in 1:sapply(tmp3, length)[1]) {
  n <- rep(NA, dim(tmp2)[1])
  n <- sapply(tmp3, function(x) x[w])
  df <- data.frame(n, stringsAsFactors = FALSE); names(df) = nms[w]
  tmp2 <- cbind.data.frame(tmp2, df, stringsAsFactors = FALSE)
}

#comparing and replacing the correct info
tmp2 = tmp2[order(tmp2$order),]
table(tmp$order == tmp2$order)
id = !tmp2$loc == tmp2$loc.correct

if(any(id)) {
  cat(paste(i,": ",unique(tmp$NAME_0),table(id)),"\n")

  table(tmp$NAME_0[id] == tmp2[id,c("country")])
  #tmp[id,1] = tmp2[id,c("country")]
  table(tmp$NAME_1[id] == tmp2[id,c("state")]) #all differences were double-checked and the gazetteer is right!
  cbind(tmp$NAME_1[id], tmp2[id,c("state","county")])[tmp$NAME_1[id] == tmp2[id,c("state")],]
  #tmp[id,2] = tmp2[id,c("state")]
  table(!tmp$NAME_2[id] == tmp2[id,c("county")]) #all differences were double-checked and the gazetteer is right!
  cbind(tmp[id,c("NAME_1","NAME_2")], tmp2[id,c("county")])[!tmp$NAME_2[id] == tmp2[id,c("county")],]
  # tmp$NAME_2[id] = tmp2[id,c("county")]

  #Saving the changes
  country.list[[j]]$NAME_2[id] = tmp2[id,c("county")]
  # country.list[[j]]@data = tmp[,c("NAME_0","NAME_1","NAME_2")]
}
#}

## Converting all maps to 'sf'
# for(i in 1:length(country.list)){
#   tmp <- country.list[[i]]
#   tmp1 <- sf::st_as_sf(tmp)
#   country.list[[i]] <- tmp1
# }

## Inspecting
latamMap <- country.list
latamMap_full <- country.list

#names(latamMap) <- pais
plot(latamMap["belize"][[1]][,1])
plot(latamMap["colombia"][[1]][,1])
plot(latamMap["french guiana"][[1]][,1])
plot(latamMap["paraguay"][[1]][,1])
plot(latamMap["venezuela"][[1]][,1])

## Saving
save(latamMap, file = "./data/latamMap.rda", compress = "xz")
save(latamMap_full, file = "./data/latamMap_full.rda", compress = "xz")


################################################################################H
################################################################################H
#####################################
### WORLD, LAND BUFFER AND SHORES ###
#####################################


#### WORLD ####
# map for function share_borders()
world0 <- rnaturalearth::ne_download(scale = 10, type = 'countries', category = 'cultural')
world1 <- rgeos::gBuffer(world0, byid = TRUE, width = 0)
world1 <- cleangeo::clgeo_Clean(world1)
world2 <- rgeos::gSimplify(world1, tol = 0.001, topologyPreserve = TRUE)
world2 <- rgeos::gBuffer(world2, byid = TRUE, width = 0)
world2 <- cleangeo::clgeo_Clean(world2)
cols <- c("ISO_A2","NAME_LONG")
world3 <- sp::SpatialPolygonsDataFrame(world2, world0@data[,cols])
names(world3@data) <- c("iso_a2","name")

#Editing country name as in the gazetteer
world3@data$name <- prepCountry(world3@data$name)
#Replacing country names as in the gazetteer
tmp1 <- replaceNames[replaceNames$class %in% "country" & apply(is.na(replaceNames[, 2:4]), 1, all), ]
tmp2 <- as.character(tmp1$replace)
names(tmp2) = as.character(tmp1$pattern)
#names(tmp2) <- gsub("\\\\", "", names(tmp2))
world3@data$name <- sapply(strsplit(world3@data$name,' \\('), function(x) x[[1]][1])
world3@data$name <- stringr::str_replace_all(world3@data$name, tmp2)

#Converting to sf and projecting to WSG84
world <- sf::st_as_sf(world3)
prj <- sf::st_crs(4326)
world <- sf::st_set_crs(world, prj)

## Saving
save(world, file = "./data/world.rda", compress = "xz")

#Inspecting the maps and object sizes
# format(object.size(world0), units = "Mb") ## 15.8 Mb
# format(object.size(world2), units = "Mb") ## 15.6 Mb (not much but borders remains almost the same)
# format(object.size(world), units = "Mb") ## 10.3 Mb
# sp::plot(world0, ylim=c(-24,-22), xlim=c(-45, -42))
# sp::plot(world3, add = TRUE, border = 3)
# sp::plot(world[,1], add = TRUE, border = 4)


#### LAND BUFFER ####
land <- rnaturalearth::ne_download(scale = 10, type = 'land', category = 'physical')

## Buffering mainland to 0.5 degree (~50 km at the equator) and
#buffering major islands to 0.25 degree (~25 km at the equator)
# buff.rad <- 0.5
buff.rad <- land$scalerank
buff.rad[buff.rad <= 3] <- 0.5
buff.rad[buff.rad > 3 & buff.rad < 10] <- 0.25
buff.rad[buff.rad > 10] <- 0
land.buff <- rgeos::gBuffer(land, byid=TRUE, width = buff.rad)
land.buff1 <- raster::crop(land.buff, raster::extent(CoordinateCleaner::buffland))
land.buff1 <- sp::disaggregate(land.buff1)
land.buff1 <- cleangeo::clgeo_Clean(land.buff1)
land.buff2 <- raster::aggregate(land.buff1)
land.buff3 <- rgeos::gSimplify(land.buff2, tol = 0.01, topologyPreserve = TRUE)
land.buff3 <- rgeos::gBuffer(land.buff3, byid = TRUE, width = 0)
land.buff3 <- sp::SpatialPolygonsDataFrame(land.buff3, data.frame(class = "land"))
landBuff <- sf::st_as_sf(land.buff3)
prj <- sf::st_crs(4326)
landBuff <- sf::st_set_crs(landBuff, prj)

## Saving
save(landBuff, file = "./data/landBuff.rda", compress = "xz")

#Inspecting the maps and object sizes
# sp::plot(land, ylim=c(-24,-22), xlim=c(-47, -43))
# sp::plot(land, ylim=c(15,30), xlim=c(-80, -60))
# sp::plot(land.buff2, add = TRUE, border = 2)
# sp::plot(land.buff3, add = TRUE, border = 3)
# sp::plot(landBuff, add = TRUE)
# format(object.size(land), units = "Mb") ## 12.2 Mb
# format(object.size(land.buff2), units = "Mb") ## 2.3 Mb (not much but borders remains almost the same)
# format(object.size(land.buff3), units = "Mb") ## 1.6 Mb (not much but borders remains almost the same)
# format(object.size(landBuff), units = "Mb") ## 0.9 Mb

#### MINOR ISLANDS ####
minor.islands <- rnaturalearth::ne_download(scale = 10, type = 'minor_islands', category = 'physical')

#removing islands already in the land buffer
coords <- as.data.frame(sp::coordinates(minor.islands))
colnames(coords) <- c("lon", "lat")
sp::coordinates(coords) <- ~lon + lat
sp::proj4string(coords) <- raster::crs(land)
tmp <- sp::over(coords, land.buff3)
minor.islands1 <- minor.islands[is.na(tmp$class),]

#buffering to 0.25 degree (~25 km at the equator)
buff.rad = 0.25
minor.buff <- rgeos::gBuffer(minor.islands1, byid=TRUE, width = buff.rad)
minor.buff1 <- raster::crop(minor.buff, raster::extent(CoordinateCleaner::buffland))
minor.buff1 <- sp::disaggregate(minor.buff1)
minor.buff1 <- cleangeo::clgeo_Clean(minor.buff1)
minor.buff2 <- raster::aggregate(minor.buff1)
minor.buff3 <- rgeos::gSimplify(minor.buff2, tol = 0.01, topologyPreserve = TRUE)
minor.buff3 <- rgeos::gBuffer(minor.buff3, byid = TRUE, width = 0)
minor.buff3 <- sp::SpatialPolygonsDataFrame(minor.buff3, data.frame(class = "minor.islands"))
islandsBuff <- sf::st_as_sf(minor.buff3)
prj <- sf::st_crs(4326)
islandsBuff <- sf::st_set_crs(islandsBuff, prj)

## Saving
save(islandsBuff, file = "./data/islandsBuff.rda", compress = "xz")

#Inspecting the maps and object sizes
# sp::plot(landBuff, ylim=c(-24,-22), xlim=c(-47, -43))
# sp::plot(landBuff, ylim=c(10,30), xlim=c(-90, -60))
# sp::plot(minor.buff2, add = TRUE, border = 2)
# sp::plot(minor.buff3, add = TRUE, border = 3)
# format(object.size(minor.islands), units = "Mb") ## 9.1 Mb
# format(object.size(minor.buff2), units = "Mb") ## 0.3 Mb (not much but borders remains almost the same)
# format(object.size(minor.buff3), units = "Mb") ## 0.2 Mb (not much but borders remains almost the same)
# format(object.size(islandsBuff), units = "Mb") ## 0.1 Mb



#### SHORE LINES ####
land50 <- rnaturalearth::ne_download(scale = 50, type = 'land', category = 'physical')
coast <- rnaturalearth::ne_download(scale = 50, type = 'coastline', category = 'physical')

# shore lines
land1 <- rgeos::gBuffer(land50, byid=TRUE, width=0)
land1 <- cleangeo::clgeo_Clean(land1)
land2 <- rgeos::gSimplify(land1, tol = 0.001, topologyPreserve = TRUE)
land2 <- rgeos::gBuffer(land2, byid = TRUE, width = 0)
land2 <- raster::aggregate(land2)
land3 <- as(land2, "SpatialLines")
shoreLines <- sf::st_as_sf(land3)
prj <- sf::st_crs(4326)
shoreLines <- sf::st_set_crs(shoreLines, prj)

# coast1 <- coast
# coast1@data <- coast1@data[,1,drop=FALSE]
# coast2 <- rgeos::gSimplify(coast1, tol = 0.001, topologyPreserve = TRUE)
# coastLines <- sf::st_as_sf(coast2)
# prj <- sf::st_crs(4326)
# coastLines <- sf::st_set_crs(coastLines, prj)

## Saving
save(shoreLines, file = "./data/shoreLines.rda", compress = "xz")
# save(coastLines, file = "./data/coastLines.rda", compress = "xz")

#Inspecting the maps and object sizes
# sp::plot(shoreLines, ylim=c(-25,-20), xlim=c(-47, -42))
# sp::plot(coastLines, add = TRUE, col = 2)
# format(object.size(land50), units = "Mb") ## 5.3 Mb
# format(object.size(coast), units = "Mb") ## 3.3 Mb
# format(object.size(land2), units = "Mb") ## 3.5 Mb
# format(object.size(land3), units = "Mb") ## 2.5 Mb
# format(object.size(shoreLines), units = "Mb") ##1.7 Mb
# format(object.size(coastLines), units = "Mb") ##2.3 Mb



#### A DICTIONARY OF COUNTRY NAMES AND CODES ####
# cols <- c("SOVEREIGNT","TYPE","ADMIN", "NAME","NAME_LONG","FORMAL_EN","FORMAL_FR",
#           "NAME_CIAWF", "NAME_SORT","ISO_A2","ISO_A3","WB_A2","WB_A3",
#           "CONTINENT", "REGION_UN", "SUBREGION", "REGION_WB",
#           "NAME_DE", "NAME_EN", "NAME_ES","NAME_FR","NAME_HU","NAME_ID","NAME_IT",
#           "NAME_NL","NAME_PL","NAME_PT","NAME_SV","NAME_TR","NAME_VI")

