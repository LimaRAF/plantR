#### Script to read shapefiles, edit and generate the data/worldMap.rds ####
require(rgdal)
require(rgeos)
require(sf)
require(countrycode)
devtools::load_all()

### WORLD MAP FOR VALIDATION ###
##Loading, editing and converting the world country shapefile
path <- "E://ownCloud//W_GIS"
#wo <- readOGR(dsn=paste(path,"//WO_ADM_limits_GADM36//gadm36_levels_shp",sep=""),layer="gadm36_0")
wo <- readRDS(paste(path,"//WO_ADM_limits_GADM36//gadm36_levels_shp//gadm36_0.rds",sep=""))

#wo <- readRDS(paste0(path,"//WO_ADM_limits_GADM36//gadm36_levels_shp//gadm36_0.rds"))
wo@data$pais <- countrycode(as.character(wo@data$GID_0),'iso3c', 'country.name')
wo@data$pais[is.na(wo@data$pais)] <- as.character(wo@data$NAME_0[is.na(wo@data$pais)])
#wo@data$pais <- tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), wo@data$pais))
wo@data$pais <- tolower(textclean::replace_non_ascii(wo@data$pais))
wo@data$pais <- stringr::str_replace_all(wo@data$pais,  " & ", " and ")
wo@data$pais <- gsub("^st. ", "saint ", wo@data$pais)
wo@data$pais <- gsub(" of the ", " ", wo@data$pais)
wo@data$pais <- gsub(" of ", " ", wo@data$pais)

tmp1 <- replaceNames[replaceNames$class %in% "country" & apply(is.na(replaceNames[, 2:4]), 1, all), ]
tmp2 <- as.character(tmp1$replace)
names(tmp2) = as.character(tmp1$pattern)
#names(tmp2) <- gsub("\\\\", "", names(tmp2))
wo@data$pais <- sapply(strsplit(wo@data$pais,' \\('), function(x) x[[1]][1])
wo@data$pais <- stringr::str_replace_all(wo@data$pais, tmp2)
wo@data$pais <- plantR::prepLoc(wo@data$pais)

## Comparing the map with the gazetteer
tmp <- merge(wo@data, gazetteer[,c("loc", "loc.correct")], by.x = "pais", by.y = "loc", all.x = TRUE)
tmp[!tmp$pais %in% tmp$loc.correct,]

#Transforming and simplifying
wo@data <- wo@data[,c("GID_0","pais")]
names(wo@data)[2] <- "NAME_0"
wo.simp <- gSimplify(wo, tol=0.001, topologyPreserve = TRUE)
wo.simp <- gBuffer(wo.simp, byid=TRUE, width=0)
wo.simp1 <- SpatialPolygonsDataFrame(wo.simp, wo@data)
worldMap <- sf::st_as_sf(wo.simp1)

wo.simp2 <- gSimplify(wo, tol=0.0001, topologyPreserve = TRUE)
wo.simp2 <- gBuffer(wo.simp2, byid=TRUE, width=0)
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
save(worldMap_0001, file = "./data/worldMap_0001.rda", compress = "xz")
save(worldMap_full, file = "./data/worldMap_full.rda", compress = "xz")

### LATIN AMERICAN MAP FOR VALIDATION ###

##List of Neotropical countries
iso3 <- countrycode::countrycode(c("Anguilla","Antigua and Barbuda","Argentina","Aruba","Bahamas",
                      "Barbados","Belize","Bermuda","Bolivia","Brazil","British Virgin Islands",
                      "Caribbean Netherlands","Cayman Islands","Chile",
                      "Colombia","Costa Rica","Cuba","Curaçao","Dominica","Dominican Republic",
                      "Ecuador","El Salvador","Falkland Islands",
                      "French Guiana","Grenada","Guadeloupe","Guatemala","Guyana","Haiti",
                      "Honduras","Jamaica","Martinique","Mexico","Montserrat","Nicaragua",
                      "Panama","Paraguay","Peru","Puerto Rico","Saint Barthélemy",
                      "Saint Kitts and Nevis","Saint Lucia","Saint Martin (French part)","Saint Vincent and the Grenadines",
                      "Sint Maarten","Suriname","Trinidad and Tobago","Turks and Caicos Islands",
                      "United States Virgin Islands","Uruguay","Venezuela"), "country.name", "iso3c")

##Loading, editing and converting the world country shapefile
path0 <- "E://ownCloud//W_GIS//Am_Lat_ADM_GADM_v3.6//"
countries <- list.files(path0, full.name=TRUE)
countries <- countries[grep(paste(iso3, collapse = "|") ,countries)]
countries <- countries[grep("sf.rds",countries)]
countries2 <- countries[grep('_2_sf.rds',countries)]
countries3 <- countries[grep('_3_sf.rds',countries)]
adm2 <- sapply(strsplit(countries2, "//|_"), function(x) x[10])
#adm2 <- c("ARG","BOL","BRA","CHL","COL","ECU","GUY","GUF","SUR","PRY","PER","URY","VEN","MEX")
adm3 <- sapply(strsplit(countries3, "//|_"), function(x) x[10])

#countries2 <- countries2[grep(paste0(adm2, collapse = "|"),countries2)]
countries1 <- countries[grepl('_1_sf.rds',countries) & !grepl(paste0(adm2, collapse = "|"), countries)]
adm1 <- sapply(strsplit(countries1, "_"), function(x) x[7])
countries0 <- countries[grepl('_0_sp.rds',countries)  & !grepl(paste0(c(adm1,adm2), collapse = "|"), countries)]
all.countries <- sort(c(adm1, adm2))
iso3[!iso3 %in% all.countries] # all of these countries are only available at ADM_0, that is, already in worldMap.
countries <- sort(c(countries1, countries2))

country.list <- vector('list', length(countries))
pais = NULL
for (i in 1:length(country.list)) {
  path = countries[i]
  tmp = readRDS(path)
  tmp1 = tmp@data
  tmp1 = tmp1[,grepl("^NAME_", names(tmp1))]

  #country
  tmp1$NAME_0 <- tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), tmp1$NAME_0))
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
  tmp1$NAME_1 <- tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), tmp1$NAME_1))
  tmp1$NAME_1 <- plantR::prepLoc(tmp1$NAME_1)

  #ADM_2
  if("NAME_2" %in% names(tmp1)) {
    tmp1$NAME_2 <- tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), tmp1$NAME_2))
    tmp1$NAME_2 <- plantR::prepLoc(tmp1$NAME_2)
  }

  #Symplifying the maps
  tmp.simp <- gSimplify(tmp, tol=0.0001, topologyPreserve = TRUE)
  tmp.simp <- gBuffer(tmp.simp, byid=TRUE, width=0)

  #Extract polygon ID's
  pid <- sapply(slot(tmp.simp, "polygons"), function(x) slot(x, "ID"))

  #Create dataframe with correct rownames and merging
  p.df <- as.data.frame(tmp1, row.names = pid, stringsAsFactors = FALSE)
  tmp.simp1 <- SpatialPolygonsDataFrame(tmp.simp, p.df)

  #Saving
  pais[i] = unique(p.df$NAME_0)
  country.list[[i]] = tmp.simp1
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
  tmp = country.list[[j]]@data
  for(i in 1:dim(tmp)[2]) tmp[,i] <- as.character(tmp[,i])

  tmp1= data.frame(loc=apply(tmp, 1, paste, collapse="_"))
  tmp1$loc = as.character(tmp1$loc)

  tmp$order = 1:dim(tmp)[1]
  tmp1$order = 1:dim(tmp1)[1]

  #merging the two sources of info
  tmp2 <- merge(tmp1, dic[,c("NAME_0","NAME_1","NAME_2","loc","loc.correct")],by="loc",all.x=TRUE)
  tmp3 <- strsplit(tmp2$loc.correct,"_")

  nms <- c("country", "state", "county")
  for(w in 1:sapply(tmp3,length)[1]) {
    n <- rep(NA,dim(tmp2)[1])
    n <- sapply(tmp3,function(x) x[w])
    df <- data.frame(n, stringsAsFactors = FALSE); names(df) = nms[w]
    tmp2 <- cbind.data.frame(tmp2, df, stringsAsFactors = FALSE)
  }

  #comparing and replacing the correct info
  tmp2 = tmp2[order(tmp2$order),]
  table(tmp$order == tmp2$order)
  id = !tmp2$loc == tmp2$loc.correct

  if(any(id)) {
    cat(paste(i,": ",unique(tmp$NAME_0),table(id)),"\n")

    table(tmp[id,1] == tmp2[id,c("country")])
    #tmp[id,1] = tmp2[id,c("country")]
    table(tmp[id,2] == tmp2[id,c("state")]) #all differences were double-checked and the gazetteer is right!
    cbind(tmp[id,c(2)], tmp2[id,c("state","county")])[tmp[id,2] == tmp2[id,c("state")],]
    #tmp[id,2] = tmp2[id,c("state")]
    table(!tmp[id,3] == tmp2[id,c("county")]) #all differences were double-checked and the gazetteer is right!
    cbind(tmp[id,c(2,3)], tmp2[id,c("county")])[!tmp[id,3] == tmp2[id,c("county")],]
    tmp[id,3] = tmp2[id,c("county")]

    #Saving the changes
    country.list[[j]]@data = tmp[,c("NAME_0","NAME_1","NAME_2")]
  }
#}

## Converting all maps to 'sf'
for(i in 1:length(country.list)){
  tmp <- country.list[[i]]
  tmp1 <- sf::st_as_sf(tmp)
  country.list[[i]] <- tmp1
}

## Inspecting
latamMap <- country.list
names(latamMap) <- pais
plot(latamMap["belize"][[1]][,1])
plot(latamMap["colombia"][[1]][,1])
plot(latamMap["french guiana"][[1]][,1])
plot(latamMap["paraguay"][[1]][,1])
plot(latamMap["venezuela"][[1]][,1])

## Saving
save(latamMap, file = "./data/latamMap.rda", compress = "xz")


