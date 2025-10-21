#### Script to read shapefiles, edit and generate the data/worldMap.rds ####
require(rgdal)
require(rgeos)
require(sf)
require(sp)
require(countrycode)
devtools::load_all()


## WORLD MAP FOR VALIDATION --------------------------------------------------------------------------------------------------

##Loading, editing and converting the world country shapefile
#wo <- readOGR(dsn=paste(path,"//WO_ADM_limits_GADM36//gadm36_levels_shp",sep=""),layer="gadm36_0")
# wo <- readRDS(paste(path,"//WO_ADM_limits_GADM36//gadm36_levels_shp//gadm36_0.rds",sep = ""))
#wo <- readRDS(paste0(path,"//WO_ADM_limits_GADM36//gadm36_levels_shp//gadm36_0.rds"))
file_path <- file.path("E:", "ownCloud", "W_GIS", "WO_ADM_limits_GADM",
                       "gadm41", "gadm_41_adm0_r0.rds")
file_path <- file.path("C:", "Users", "renat", "Documents", "R_packages",
                       "plantRdata", "data-raw", "gadm", "gadm_41_adm0_r0.rds")
wo <- terra::vect(terra::unwrap(file_path))
names(wo) <- c("GID_0", "NAME_0")

#Editing country name as in the gazetteer
wo$pais <- countrycode(as.character(wo[["GID_0"]][,1]),'iso3c', 'country.name')
wo[["pais"]][[1]][is.na(wo[["pais"]][[1]])] <-
  as.character(wo[["NAME_0"]][[1]][is.na(wo[["pais"]][[1]])])
wo[["pais"]][[1]] <- plantR::prepCountry(wo[["pais"]][[1]])

tmp1 <- plantR:::replaceNames[plantR:::replaceNames$class %in% "country" &
                       apply(is.na(plantR:::replaceNames[, 2:4]), 1, all), ]
tmp2 <- as.character(tmp1$replace)
names(tmp2) = as.character(tmp1$pattern)
#names(tmp2) <- gsub("\\\\", "", names(tmp2))
wo[["pais"]][[1]] <- sapply(strsplit(wo[["pais"]][[1]],' \\('),
                            function(x) x[[1]][1])
wo[["pais"]][[1]] <- stringr::str_replace_all(wo[["pais"]][[1]], tmp2)

## Comparing the map with the gazetteer
wo_df <- terra::as.data.frame(wo)
tmp <- merge(wo_df, plantR:::gazetteer[,c("loc", "loc.correct")],
             by.x = "pais", by.y = "loc", all.x = TRUE)
tmp[!tmp$pais %in% tmp$loc.correct,] #ok if empty!
tmp[!tmp$loc.correct %in% tmp$pais,] #ok if empty!

#Transforming and simplifying
# wo@data <- wo@data[,c("GID_0","pais")]
# names(wo@data)[2] <- "NAME_0"
# wo.simp <- gSimplify(wo, tol=0.001, topologyPreserve = TRUE)
# wo.simp <- gBuffer(wo.simp, byid = TRUE, width = 0)
# wo.simp1 <- SpatialPolygonsDataFrame(wo.simp, wo@data)
# wo.simp1_sf <- sf::st_as_sf(wo.simp1)

wo[["NAME_0"]] <- NULL
names(wo)[2] <- "NAME_0"
wo.simp <- terra::simplifyGeom(wo, tol=0.001, preserveTopology = TRUE)
wo.simp <- terra::buffer(wo.simp, 0)
wo.simp1_sf <- sf::st_as_sf(wo.simp)

#Repairing possible issues related to spherical geometries
isv <- sf::st_is_valid(wo.simp1_sf) #Senegal had an issue before; now Chile and Argentina? Not showing in the validity test!
pais2fix <- c("argentina", "chile")
# if (any(!isv)) {
  # for (i in which(!isv)) {
  for (j in seq_along(pais2fix)) {
    i <- which(wo.simp1_sf$NAME_0 %in% pais2fix[j])
    tmp.i <- wo.simp1_sf[i, ]$geometry
    # proj <- "+proj=laea +lon_0=-63.4570303 +lat_0=-39.280776 +datum=WGS84 +units=m +no_defs"
    # tmp.ii <- sf::st_transform(tmp.i, proj)
    tmp.i.1 <- s2::as_s2_geography(sf::st_as_binary(tmp.i), check = FALSE)
    tmp.i.2 <- s2::s2_as_binary(s2::s2_snap_to_grid(tmp.i.1, grid_size = 1e-7))
    tmp.i.2.5 <- s2::s2_geog_from_wkb(tmp.i.2, check = FALSE, planar = TRUE)
    tmp.i.3 <- sf::st_as_sfc(tmp.i.2.5)
    # tmp.i.3.5 <- sf::st_transform(tmp.i, sf::st_crs(tmp.i))
    wo.simp1_sf[i, ]$geometry <- sf::st_make_valid(tmp.i.3)
  }
# }
# worldMap <- sf::st_make_valid(worldMap)
stopifnot(all(sf::st_is_valid(wo.simp1_sf)))

# set spatial coordinates
prj <- sf::st_crs(4326)
worldMap <- sf::st_set_crs(wo.simp1_sf, prj)


# wo.simp2 <- gSimplify(wo, tol = 0.0001, topologyPreserve = TRUE)
# wo.simp2 <- gBuffer(wo.simp2, byid = TRUE, width = 0)
# wo.simp3 <- SpatialPolygonsDataFrame(wo.simp2, wo@data)
# worldMap_0001 <- sf::st_as_sf(wo.simp3)

# wo.simp4 <- gSimplify(wo, tol = 0.1, topologyPreserve = TRUE)
# wo.simp4 <- gBuffer(wo.simp4, byid = TRUE, width = 0)
# wo.simp5 <- SpatialPolygonsDataFrame(wo.simp4, wo@data)
# worldMap_01 <- sf::st_as_sf(wo.simp5)

# #wo.simp2 <- sf::st_buffer(wo.simp1, dist = 0)
# worldMap_full <- sf::st_as_sf(wo)
#
# #Inspecting
# par(mfrow = c(2,2), mar=c(0,0,1,0))
# plot(wo[1,], main="Aruba original")
# plot(wo.simp[1,], border = "red", main="Aruba gSimp 0.001")
# plot(st_geometry(worldMap[1,1]), border = "green", main = "Aruba gSimp 0.001 sf")

#Saving
save(worldMap, file = "./data/worldMap.rda", compress = "xz")
# save(worldMap_0001, file = "./data/worldMap_0001.rda", compress = "xz")
# save(worldMap_full, file = "./data/worldMap_full.rda", compress = "xz")




## LATIN AMERICAN MAP FOR VALIDATION ----------------------------------------------------------------------------------

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
iso3 <- sort(countrycode::countrycode(countries.latam, "country.name", "iso3c"))
#BES = "Caribbean Netherlands" = "Bonaire, Saint Eustatius and Saba"

# Automatic download is not working, do it mannually
# options(timeout = max(300, getOption("timeout")))
# url <- paste0("https://geodata.ucdavis.edu/gadm/gadm4.1/gpkg/gadm41_", iso3.i,".gpkg")
# download.file(url, destfile = destfile.i, method = "auto")
# message(paste("Downloading ", iso3.i, "..."))
# close(url)
#
# source("https://raw.githubusercontent.com/liibre/Rocc/master/R/getGADM.R")
# Dando problema tb
# purrr::walk2(.x = rep(iso3, each = 4),
#              .y = rep(3:0, length(countries.latam)),
#              ~ getGADM(
#                cod = .x,
#                level = .y,
#                best = FALSE, #importante para ter outros niveis
#                destfolder = destfolder
#              ))
# gadm_files <- list.files(destfolder, pattern = "rds$",
#                          full.names = TRUE)


##Loading, editing and converting the world country shapefile
path0 <- "D://ownCloud//W_GIS//Am_Lat_ADM_GADM_v3.6//"
path0 <- "C:/Users/renat/Documents/R_packages/plantRdata/data-raw/gadm"
countries <- list.files(path0, full.name=TRUE)
countries <- countries[grep(paste(iso3, collapse = "|") ,countries)]
# countries <- countries[grep("sp.rds",countries)]
# countries3 <- countries[grepl('_3_sp.rds',countries)]
# adm3 <- sapply(strsplit(countries3, "_"), function(x) x[11])
# if (length(adm3) > 0) {
#   countries2 <- countries[grepl('_2_sp.rds',countries) & !grepl(paste0(c(adm3), collapse = "|"), countries)]
#   adm2 <- sapply(strsplit(countries2, "//|_"), function(x) x[11])
#   countries1 <- countries[grepl('_1_sp.rds',countries) & !grepl(paste0(c(adm2,adm3), collapse = "|"), countries)]
#   adm1 <- sapply(strsplit(countries1, "//|_"), function(x) x[11])
#   countries0 <- countries[grepl('_0_sp.rds',countries)  & !grepl(paste0(c(adm1,adm2), collapse = "|"), countries)]
#   adm0 <- sapply(strsplit(countries0, "//|_"), function(x) x[11])
#   all.countries <- sort(c(adm1, adm2, adm3))
# } else {
#   countries2 <- countries[grepl('_2_sp.rds',countries)]
#   adm2 <- sapply(strsplit(countries2, "//|_"), function(x) x[11])
#   countries1 <- countries[grepl('_1_sp.rds',countries) & !grepl(paste0(c(adm2), collapse = "|"), countries)]
#   adm1 <- sapply(strsplit(countries1, "//|_"), function(x) x[11])
#   countries0 <- countries[grepl('_0_sp.rds',countries)  & !grepl(paste0(c(adm1,adm2), collapse = "|"), countries)]
#   adm0 <- sapply(strsplit(countries0, "//|_"), function(x) x[11])
#   all.countries <- sort(c(adm1, adm2))
# }

all.countries <- gsub(".*\\/", "", countries, perl = TRUE)
all.countries <- gsub("^gadm41_|^gadm40_", "", all.countries, perl = TRUE)
all.countries <- gsub(".gpkg$", "", all.countries, perl = TRUE)
countries <- countries[match(iso3, all.countries)]
all.countries <- all.countries[match(iso3, all.countries)]

check_these <- !iso3 %in% all.countries
stopifnot(!any(check_these))
all.countries[!all.countries %in% iso3]

if (any(check_these)) {
  tmp.amd0 <- iso3[check_these]
  tmp.amd0[!tmp.amd0 %in% as.character(plantR::worldMap$GID_0)]
  # code above should be empty, meaning that all of the countries in
  # 'check_these' are only available at ADM_0, that is, already in worldMap.
}

country.list <- vector('list', length(countries))
pais = NULL
for (i in 1:length(country.list)) {

  path.i = countries[i]
  layer.i <- sf::st_layers(path.i)
  best.layer <- tail(layer.i$name, 1)
  if (best.layer == "ADM_ADM_0") {
    next
  } else {
    tmp <- terra::vect(path.i, best.layer)
    tmp <- sf::st_as_sf(tmp, "sf")
    # tmp = readRDS(path)
    # tmp1 = tmp@data
    tmp1 = tmp[,grepl("^NAME_[1-4]|^COUNTRY$", names(tmp), perl = TRUE)]
    colnames(tmp1) <- gsub("COUNTRY", "NAME_0", colnames(tmp1))

    # ADM0 (country)
    tmp1$NAME_0 <- prepCountry(tmp1$NAME_0)
    # tmp1$NAME_0 <- tolower(textclean::replace_non_ascii(tmp1$NAME_0))
    # tmp1$NAME_0 <- stringr::str_replace_all(tmp1$NAME_0,  " & ", " and ")
    # tmp1$NAME_0 <- gsub("^st. ", "saint ", tmp1$NAME_0)
    # tmp1$NAME_0 <- gsub(" of the ", " ", tmp1$NAME_0)
    # tmp1$NAME_0 <- gsub(" of ", " ", tmp1$NAME_0)

    toto1 <- replaceNames[replaceNames$class %in% "country" &
                            apply(is.na(replaceNames[, 2:4]), 1, all), ]
    toto2 <- as.character(toto1$replace)
    names(toto2) = as.character(toto1$pattern)
    # names(toto2) <- gsub("\\\\", "", names(toto2))
    tmp1$NAME_0 <- sapply(strsplit(tmp1$NAME_0,' \\('), function(x) x[[1]][1])
    tmp1$NAME_0 <- stringr::str_replace_all(tmp1$NAME_0, toto2)
    # tmp1$NAME_0 <- plantR::prepLoc(tmp1$NAME_0)

    tmp1$NAME_0 <- gsub("bonaire, sint eustatius and saba", "caribbean netherlands", tmp1$NAME_0)
    tmp1$NAME_0 <- gsub("bonaire, sint eustatius saba", "caribbean netherlands", tmp1$NAME_0)
    tmp1$NAME_0 <- gsub("saint vincent and the grenadines", "saint vincent grenadines", tmp1$NAME_0)
    tmp1$NAME_0 <- gsub("virgin islands, u.s.", "united states virgin islands", tmp1$NAME_0)

    # ADM_1
    if ("NAME_1" %in% names(tmp1)) {
      tmp1$NAME_1 <- tolower(textclean::replace_non_ascii(tmp1$NAME_1))
      tmp1$NAME_1 <- plantR:::prepState(tmp1$NAME_1) # new line to match changes made by prepState inside fixLoc
      tmp1$NAME_1 <- plantR::prepLoc(tmp1$NAME_1)
    }

    # ADM_2
    if ("NAME_2" %in% names(tmp1)) {
      tmp1$NAME_2 <- tolower(textclean::replace_non_ascii(tmp1$NAME_2))
      tmp1$NAME_2 <- sub("dist. ", "", tmp1$NAME_2)
      tmp1$NAME_2 <- plantR::prepLoc(tmp1$NAME_2)
    }

    # ADM_3
    if ("NAME_3" %in% names(tmp1)) {
      tmp1$NAME_3 <- tolower(textclean::replace_non_ascii(tmp1$NAME_3))
      tmp1$NAME_3 <- plantR::prepLoc(tmp1$NAME_3)
    }

    #Symplifying the maps
    # tmp.simp <- gSimplify(tmp, tol=0.0001, topologyPreserve = TRUE)
    # tmp.simp <- gBuffer(tmp.simp, byid=TRUE, width=0)
    tmp.simp <- sf::st_simplify(tmp1, dTolerance = 0.0001,
                                preserveTopology = TRUE)
    tmp.simp <- sf::st_buffer(tmp.simp, dist=0)
    # tmp.simp <- tmp1

    ##Simplify transform MULTIPOLYGONS in POLYGON: is that a problem?

    #Extract polygon ID's
    # pid <- sapply(slot(tmp.simp, "polygons"), function(x) slot(x, "ID"))
    #
    # #Create dataframe with correct rownames and merging
    # p.df <- as.data.frame(tmp1, row.names = pid, stringsAsFactors = FALSE)
    # tmp.simp1 <- SpatialPolygonsDataFrame(tmp.simp, p.df)

    # set spatial coordinates
    prj <- sf::st_crs(4326)
    tmp.simp <- sf::st_set_crs(tmp.simp, prj)

    #Saving
    # pais[i] = unique(p.df$NAME_0)
    pais[i] = unique(tmp.simp$NAME_0)
    country.list[[i]] = tmp.simp
  }
}
names(country.list) = pais
country.list1 <- country.list[!is.na(pais)]
all.countries[is.na(pais)] # countries only available at ADM0 (already in worldMap)


isv <- sapply(country.list1, sf::st_is_valid)
any(!sapply(isv, all))


## This section can be removed --------------------------------------------------------------
## Standardizing the shapefile and gazetteer names (for Brazil only)
# #loading the gazetteer and removing possible duplicated localities
# dic <- read.csv("data-raw//raw_dictionaries//gazetteer.csv",as.is=TRUE)
# # dic <- read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//dictionaries//gazetteer.csv",as.is=TRUE)
# dic <- dic[dic$status %in% "ok",]
# dic <- dic[dic$resolution.gazetteer %in% c("country","state","county"),]
#
# j = which(names(country.list) == "brazil")
# #for(j in 31:length(country.list)) {
# #Creating the locality strings for the shapefile dataframe
# tmp = country.list[[j]]#@data
# #for(i in 1:dim(tmp)[2]) tmp[,i] <- as.character(tmp[,i])
#
# tmp.df <- tmp
# st_geometry(tmp.df) <- NULL
# # tmp1= data.frame(loc=apply(tmp, 1, paste, collapse="_"))
# ids <- names(tmp.df) %in% c("NAME_0","NAME_1","NAME_2")
# tmp1= data.frame(loc=apply(tmp.df[, ids], 1, paste, collapse="_"),
#                  stringsAsFactors = FALSE)
# # tmp1$loc = as.character(tmp1$loc)
#
# tmp$order = 1:dim(tmp)[1]
# tmp1$order = 1:dim(tmp1)[1]
#
# #merging the two sources of info
# tmp2 <- merge(tmp1, dic[,c("NAME_0","NAME_1","NAME_2","loc","loc.correct")],
#               by="loc", all.x=TRUE)
# tmp3 <- strsplit(tmp2$loc.correct,"_")
#
# nms <- c("country", "state", "county")
# for(w in 1:sapply(tmp3, length)[1]) {
#   n <- rep(NA, dim(tmp2)[1])
#   n <- sapply(tmp3, function(x) x[w])
#   df <- data.frame(n, stringsAsFactors = FALSE); names(df) = nms[w]
#   tmp2 <- cbind.data.frame(tmp2, df, stringsAsFactors = FALSE)
# }
#
# #comparing and replacing the correct info
# tmp2 <- tmp2[match(tmp$order, tmp2$order),]
# table(tmp$order == tmp2$order)
# id = !tmp2$loc %in% tmp2$loc.correct
#
# if(any(id)) {
#   cat(paste(i,": ",unique(tmp$NAME_0),table(id)),"\n")
#
#   table(tmp$NAME_0[id] == tmp2[id,c("country")])
#   #tmp[id,1] = tmp2[id,c("country")]
#   table(tmp$NAME_1[id] == tmp2[id,c("state")]) #all differences were double-checked and the gazetteer is right!
#   cbind(tmp$NAME_1[id], tmp2[id,c("state","county")])[tmp$NAME_1[id] == tmp2[id,c("state")],]
#   #tmp[id,2] = tmp2[id,c("state")]
#   table(!tmp$NAME_2[id] == tmp2[id,c("county")]) #all differences were double-checked and the gazetteer is right!
#
#   cbind(tmp$NAME_1[id],tmp$NAME_2[id], tmp2$county[id])[!tmp$NAME_2[id] == tmp2$county[id],]
#   # tmp$NAME_2[id] = tmp2[id,c("county")]
#
#   #Saving the changes
#   country.list[[j]]$NAME_2[id] <- tmp2$county[id]
#   # country.list[[j]]@data = tmp[,c("NAME_0","NAME_1","NAME_2")]
# }
# #}

## This section can be removed ------------------------------------------
# ## Other GADM issues and mixed names
# #Manaus
# tmp <- country.list[[j]][country.list[[j]]$NAME_2 %in% "maues",]
# tmp1 <- sf::st_coordinates(sf::st_centroid(tmp))
# ids.manaus <- tmp1[,1] < (-59) & tmp1[,2] > (-3.5)
# country.list[[j]]$NAME_2[country.list[[j]]$NAME_2 %in% "maues"][ids.manaus] <- "manaus"
#
# country.list[[j]]$check[country.list[[j]]$NAME_2 %in% "maues"] <- ids.manaus
#
# #Brasilândia de Minas/Brazópolis
# replace_this <- country.list[[j]]$NAME_2 %in% "brazopolis" &
#   country.list[[j]]$NAME_3 %in% "brazilandia minas"
# country.list[[j]]$NAME_2[replace_this] <- "brasilandia minas"
# country.list[[j]]$NAME_3[replace_this] <- NA_character_
#
# #LEM
# replace_this <- country.list[[j]]$NAME_2 %in% "barreiras" &
#   country.list[[j]]$NAME_3 %in% "luis eduardo magalhaes"
# country.list[[j]]$NAME_2[replace_this] <- "luis eduardo magalhaes"
# country.list[[j]]$NAME_3[replace_this] <- NA_character_
#
# #Barrocas
# replace_this <- country.list[[j]]$NAME_2 %in% "serrinha" &
#   country.list[[j]]$NAME_3 %in% "barrocas"
# country.list[[j]]$NAME_2[replace_this] <- "barrocas"
# country.list[[j]]$NAME_3[replace_this] <- NA_character_
#
# #Capao Bonito do Sul
# replace_this <- country.list[[j]]$NAME_2 %in% "lagoa vermelha" &
#   country.list[[j]]$NAME_3 %in% "capao bonito"
# country.list[[j]]$NAME_2[replace_this] <- "capao bonito sul"
# country.list[[j]]$NAME_3[replace_this] <- NA_character_
#
# #Santa Cecília do Sul
# replace_this <- country.list[[j]]$NAME_2 %in% "tapejara" &
#   country.list[[j]]$NAME_3 %in% "santa cecilia"
# country.list[[j]]$NAME_2[replace_this] <- "santa cecilia sul"
# country.list[[j]]$NAME_3[replace_this] <- NA_character_
#
# #Balneario Rincão/SC
# replace_this <- country.list[[j]]$NAME_2 %in% "icara" &
#   country.list[[j]]$NAME_3 %in% "balnerio rincao"
# country.list[[j]]$NAME_2[replace_this] <- "balneario rincao"
# country.list[[j]]$NAME_3[replace_this] <- NA_character_
#
# #Três Barras/SC
# replace_this <- country.list[[j]]$NAME_2 %in% "sao mateus sul" &
#   country.list[[j]]$NAME_3 %in% "tres barras"
# country.list[[j]]$NAME_1[replace_this] <- "santa catarina"
# country.list[[j]]$NAME_2[replace_this] <- "tres barras"
# country.list[[j]]$NAME_3[replace_this] <- NA_character_
#
# #Teixeira Soares
# replace_this <- country.list[[j]]$NAME_2 %in% "texeira soares"
# country.list[[j]]$NAME_2[replace_this] <- "teixeira soares"
#
# #Mato Grosso as a county
# replace_this <- country.list[[j]]$NAME_2 %in% "mato grosso" &
#   country.list[[j]]$NAME_3 %in% "aguapei"
# country.list[[j]]$NAME_2[replace_this] <- "vila bela santissima trindade"
#
# #Itabirinha and Itabirito/MG
# replace_this <- country.list[[j]]$NAME_2 %in% "itabirito" &
#   country.list[[j]]$NAME_3 %in% "itabirinha mantena"
# country.list[[j]]$NAME_2[replace_this] <- "itabirinha"
#
#
## This section can be removed ------------------------------------------
# #Other counties
# map.checks <- readRDS("./data-raw/map.replacements.rds")
# paises <- sapply(map.checks$locs, function (x) strsplit(x, "_")[[1]][1])
# estados <- sapply(map.checks$locs, function (x) strsplit(x, "_")[[1]][2])
# j = which(names(country.list) == "brazil")
# for(w in 1:length(map.checks$locs)) {
#   pais.i <- paises[w]
#   estado.i <- estados[w]
#   bad <- map.checks$municipio.bad[w]
#   good <- map.checks$municipio.good[w]
#   replace_this <- country.list[[j]]$NAME_0 %in% pais.i &
#                   country.list[[j]]$NAME_1 %in% estado.i &
#                   country.list[[j]]$NAME_2 %in% bad &
#                   country.list[[j]]$NAME_3 %in% good
#   country.list[[j]]$NAME_2[replace_this] <- good
#   country.list[[j]]$NAME_3[replace_this] <- NA_character_
# }


## Fixing GADM problems -------------------------------------------------------------
map.checks <- read.csv("./data-raw/raw_dictionaries/gadmCheck.csv")
map.checks <- map.checks[grepl("ok", map.checks$status), ]

paises <- names(country.list1)
paises <- paises[paises %in% unique(map.checks$pais)]

for(w in 1:dim(map.checks)[1]) {
    pais.i <- map.checks$pais[w]
    estado.i <- map.checks$estado[w]
    bad.i <- map.checks$bad.mun[w]
    target.i <- map.checks$target.mun[w]
    replace.i <- map.checks$replace.mun[w]
    case.i <- map.checks$case[w]

    if (case.i == "first") {
      replace_this <- country.list1[[pais.i]]$NAME_0 %in% pais.i
      if ("NAME_1" %in% colnames(country.list1[[pais.i]]))
        replace_this <- replace_this & country.list1[[pais.i]]$NAME_1 %in% estado.i
      if ("NAME_2" %in% colnames(country.list1[[pais.i]]))
        replace_this <- replace_this & country.list1[[pais.i]]$NAME_2 %in% bad.i
      if ("NAME_3" %in% colnames(country.list1[[pais.i]]))
        replace_this <- replace_this & country.list1[[pais.i]]$NAME_3 %in% target.i

      if (any(replace_this))
        country.list1[[pais.i]]$NAME_2[replace_this] <- replace.i
    }

    if (case.i == "second") {
      replace_this <- country.list1[[pais.i]]$NAME_0 %in% pais.i
      if ("NAME_1" %in% colnames(country.list1[[pais.i]]))
        replace_this <- replace_this & country.list1[[pais.i]]$NAME_1 %in% estado.i
      if ("NAME_2" %in% colnames(country.list1[[pais.i]]))
        replace_this <- replace_this & country.list1[[pais.i]]$NAME_2 %in% bad.i

      if (any(replace_this))
        country.list1[[pais.i]]$NAME_2[replace_this] <- replace.i
    }

    if (case.i == "county_state") {
      replace.i.1 <- strsplit(replace.i,"\\|")[[1]][1]
      replace.i.2 <- strsplit(replace.i,"\\|")[[1]][2]
      replace_this <- country.list1[[pais.i]]$NAME_0 %in% pais.i &
        country.list1[[pais.i]]$NAME_1 %in% estado.i &
        country.list1[[pais.i]]$NAME_2 %in% bad.i &
        country.list1[[pais.i]]$NAME_3 %in% target.i
      if (any(replace_this)) {
        country.list1[[pais.i]]$NAME_1[replace_this] <- replace.i.1
        country.list1[[pais.i]]$NAME_2[replace_this] <- replace.i.2
      }
    }

    if (case.i == "state_state") {
      replace_this <- country.list1[[pais.i]]$NAME_0 %in% pais.i
      if ("NAME_1" %in% colnames(country.list1[[pais.i]]))
        replace_this <- replace_this & country.list1[[pais.i]]$NAME_1 %in% bad.i

      if (any(replace_this))
        country.list1[[pais.i]]$NAME_1[replace_this] <- estado.i
    }
}


# ## Finding possible problems in GADM polygons: counties with disjoint polygons
# #Function that aggreagte polygons sharing their borders
# clusterSF <- function(sfpolys, thresh){
#   df <- sfpolys[, grepl("NAME_", names(sfpolys))]
#   sf::st_geometry(df) <- NULL
#   nome <- unique(apply(df[,1:3], 1, paste, collapse = "_"))[1]
#
#   dmat <- sf::st_distance(sfpolys)
#   thresh.dist <- units::as_units('m', thresh)
#   hc <- stats::hclust(as.dist(dmat > thresh.dist), method="single")
#
#   groups = stats::cutree(hc, h=0.5)
#   d = sf::st_sf(
#     geom = do.call(c,
#                    lapply(1:max(groups), function(g){
#                      sf::st_union(sfpolys[groups==g,])
#                    })
#     )
#   )
#   d$name <- nome
#   d$group <- 1:nrow(d)
#   d
# }
#
# #Selected country map
# country.pol <- country.list[[j]]
# # country.pol <- country.list[[which(names(country.list) == "brazil")]]
#
# #List of counties with more than one polygon
# toto <- paste(country.pol$NAME_1,country.pol$NAME_2, sep="_")
# possible.counties <- unique(toto[duplicated(toto)])
# #possible.counties <- possible.counties[country.pol$NAME_2 %in% unique(c(map.checks$bad.mun, map.checks$target.mun, map.checks$replace.mun))]
#
# #Applying the function to all counties (takes time)
# require(parallel)
# require(doParallel)
# no_cores <- detectCores() - 2
# cl <- makeCluster(no_cores)
# clusterExport(cl, list("possible.counties","clusterSF","country.pol"),envir=environment())
# clusterEvalQ(cl, library(sf))
# clusterEvalQ(cl, library(units))
# tmp2 <- parLapply(cl = cl, X = 1:length(possible.counties), fun = function(x) {
#   state.i <- strsplit(possible.counties[x], "_")[[1]][1]
#   county.i <- strsplit(possible.counties[x], "_")[[1]][2]
#   check_these <- country.pol$NAME_1 %in% state.i &
#     country.pol$NAME_2 %in% county.i
#   polys <- country.pol[check_these,]
#   clust.polys <- clusterSF(polys, 100)
#   clust.polys$possible.counties[x]
#   return(clust.polys)
# })
# stopImplicitCluster()
# stopCluster(cl)
# prob.counties <- possible.counties[sapply(tmp2, function(x) max(x$group) >1)]
# prob.polys <- country.pol[toto %in% prob.counties,]
# prob.data <- prob.polys
# sf::st_geometry(prob.data) <- NULL
# prob.data$loc <- toto[toto %in% prob.counties]
#
# for (i in 1:length(unique(prob.data$loc))) {
#   loc <- unique(prob.data$loc)[i]
#   plot_these <- prob.data$loc %in% loc
#   plot(prob.polys[plot_these, 4],
#        main = loc, key.pos = 1)
#   # sf::st_coordinates(sf::st_centroid(prob.polys[plot_these, 4]))
#   # plot(country.pol[country.pol$NAME_2 %in% prob.polys$NAME_3[plot_these],4],
#   #      main = loc, key.pos = 1)
#   options(locatorBell=FALSE)
#   locator(1)
# }
#
# resulta <- vector("list", length(possible.counties))
# names(resulta) <- possible.counties
# for (i in 1:length(possible.counties)) {
#   state.i <- strsplit(possible.counties[i], "_")[[1]][1]
#   county.i <- strsplit(possible.counties[i], "_")[[1]][2]
#   check_these <- country.pol$NAME_1 %in% state.i &
#                   country.pol$NAME_2 %in% county.i
#   polys <- country.pol[check_these,]
#   resulta[[i]] <- clusterSF(polys, 100)
#   cat(i, "\n")
# }


## Converting all maps to 'sf'
# for(i in 1:length(country.list)){
#   tmp <- country.list[[i]]
#   tmp1 <- sf::st_as_sf(tmp)
#   country.list[[i]] <- tmp1
# }

## Bug fixes of state names appearing after version 0.1.9 (already added above sure)
# country.list <- plantR::latamMap
# country.list$bahamas$NAME_1 <- plantR:::prepState(country.list$bahamas$NAME_1)
# country.list$`turks caicos islands`$NAME_1 <- plantR:::prepState(country.list$`turks caicos islands`$NAME_1)
# country.list$`trinidad tobago`$NAME_1 <- plantR:::prepState(country.list$`trinidad tobago`$NAME_1)
# # Oaxaca case in Mexico
# toto <- country.list$mexico$NAME_2[grepl("dist.", country.list$mexico$NAME_2)]
# toto <- sub("dist. ", "", toto)
# country.list$mexico$NAME_2[grepl("dist.", country.list$mexico$NAME_2)] <- toto
# # Lima province in Peru
# toto <- sf::st_drop_geometry(country.list$peru[grepl("lima", country.list$peru$NAME_1), ])
# country.list$peru$NAME_1[grepl("lima", country.list$peru$NAME_1)] <-
#   plantR:::prepState(country.list$peru$NAME_1[grepl("lima", country.list$peru$NAME_1)])
# Few other cases
# toto <- country.list$brazil
# rep_these <- toto$NAME_3 %in% "vila bela santissima trindade"
# toto$NAME_2[rep_these] <- "vila bela santissima trindade"
# toto$NAME_3[rep_these] <- NA
#
# rep_these <- toto$NAME_2 %in% "sao luiz paraitinga"
# toto$NAME_2[rep_these] <- "sao luis paraitinga"
# country.list$brazil <- toto

## Inspecting
latamMap <- country.list1
# latamMap_full <- country.list

#names(latamMap) <- pais
# plot(latamMap["belize"][[1]][,1])
# plot(latamMap["colombia"][[1]][,1])
# plot(latamMap["french guiana"][[1]][,1])
# plot(latamMap["paraguay"][[1]][,1])
# plot(latamMap["venezuela"][[1]][,1])


## Saving
save(latamMap, file = "./data/latamMap.rda", compress = "xz")
# save(latamMap_full, file = "./data/latamMap_full.rda", compress = "xz")


################################################################################H
################################################################################H
#####################################
### WORLD, LAND BUFFER AND SHORES ###
#####################################


#### WORLD ####
## NOTE: Cannot reproduce the creation of map anymore. Natural Earth
## no longer provides the 258 countries at scale 110m, but only 177.
## Most of small islands where removed, creating a mismatch between
## objects worldMap and world. I tried getting the tuny countruies as
## well but there were too many mismacthes to deal with. So, the quick
## fix was to save a cipy of the old world object within the raw data
## and just add the new corrections and polygons. Move down to the
## point when the old map is loaded!

# ## Downloading maps for function share_borders()
# # world0 <- rnaturalearth::ne_download(scale = 110, type = 'countries', category = 'cultural')
# world0 <- rnaturalearth::ne_download(scale = 110, type = 'map_units',
#                                      category = 'cultural')
# # world1 <- rgeos::gBuffer(world0, byid = TRUE, width = 0)
# world1 <- sf::st_buffer(world0, dist = 0, )
# world1 <- cleangeo::clgeo_Clean(world1)
# world2 <- rgeos::gSimplify(world1, tol = 0.001, topologyPreserve = TRUE)
# world2 <- rgeos::gBuffer(world2, byid = TRUE, width = 0)
# world2 <- cleangeo::clgeo_Clean(world2)
# cols <- c("ISO_A2","NAME_LONG")
# # world3 <- sp::SpatialPolygonsDataFrame(world2, world0@data[,cols])
# world3 <- sp::SpatialPolygonsDataFrame(world2, sf::st_drop_geometry(world0)[,cols])
# names(world3@data) <- c("iso_a2","name")
#
# ## Editing country name as in the gazetteer
# world3@data$name <- prepCountry(world3@data$name)
# #Replacing country names as in the gazetteer
# tmp1 <- replaceNames[replaceNames$class %in% "country" &
#                        apply(is.na(replaceNames[, 2:4]), 1, all), ]
# tmp2 <- as.character(tmp1$replace)
# names(tmp2) = as.character(tmp1$pattern)
# #names(tmp2) <- gsub("\\\\", "", names(tmp2))
# world3@data$name <- sapply(strsplit(world3@data$name,' \\('), function(x) x[[1]][1])
# world3@data$name <- stringr::str_replace_all(world3@data$name, tmp2)
#
# #Converting to sf and projecting to WSG84
# world3 <- sf::st_as_sf(world3)
# prj <- sf::st_crs(4326)
# world3 <- sf::st_set_crs(world3, prj)
#
# ## Checking if all names in worldMap are in world
# world3$name[!world3$name %in% gazetteer$loc[gazetteer$resolution.gazetteer %in% "country"]]
# world3$name[grepl("falkland", world3$name)] <-
#   worldMap$NAME_0[grepl("falkland", worldMap$NAME_0)]
# world3$name[grepl("eswatini", world3$name)] <-
#   worldMap$NAME_0[grepl("swaziland", worldMap$NAME_0)]
#
# ## Adding smaller countries and islands from the backup version of the map
# world3.1 <- readRDS("data-raw/raw_dictionaries/world.rds")
# world3.1 <- world3.1[!world0.1$name %in% unique(world3$name),]
# world4 <- dplyr::bind_rows(world3, world3.1)
load("data-raw/raw_dictionaries/world.rda")
world4 <- world
setdiff(worldMap$NAME_0, world4$name)

## Adding missing countries - caspian sea
toto <- rnaturalearth::ne_download(scale = 110, type = 'ocean',
                                   category = 'physical')
caspian <- toto[1,1]
caspian <- cleangeo::clgeo_Clean(caspian)
caspian@data$iso_a2 <- "-99"
caspian@data$name <- "caspian sea"
caspian@data$scalerank <- NULL
caspian <- sf::st_as_sf(caspian)
prj <- sf::st_crs(4326)
world <- sf::st_set_crs(caspian, prj)
world4 <- rbind(world4, caspian)
setdiff(worldMap$NAME_0, world4$name)

# Adding some missing ISO A2 codes
adds <- c("papua new guinea","palestinian territories", "norway",
          "united kingdom", "somalia", "serbia", "kosovo")
names(adds) <- c("PG", "PS", "NO", "GB", "SO", "RS", "XK")
rep_these <- world4$iso_a2 %in% c("","-99",NA)
if(any(rep_these)) {
  miss.iso <- unique(world4$name[rep_these])
  for (i in seq_along(miss.iso)) {
    miss.country.i <- miss.iso[i]
    if (miss.country.i %in% adds)
      world4$iso_a2[world4$name %in% miss.country.i] <-
        names(adds)[adds %in% miss.country.i]
  }
}

#Converting to sf and projecting to WSG84
world <- sf::st_as_sf(world4)
prj <- sf::st_crs(4326)
world <- sf::st_set_crs(world, prj)

#Repairing possible issues related to spherical geometries
isv <- sf::st_is_valid(world) # Sudan and South Georgia South Sandwich islands
if (any(!isv)) {
  for (i in which(!isv)) {
    tmp.i <- world[i, ]$geometry
    tmp.i.1 <- s2::as_s2_geography(tmp.i, check = FALSE)
    tmp.i.2 <- s2::s2_union(tmp.i.1)
    tmp.i.3 <- sf::st_as_sfc(
      s2::s2_rebuild(tmp.i.2, s2::s2_options(split_crossing_edges = TRUE)))
    world[i, ]$geometry <- tmp.i.3
  }
}

#Comparing with the original world map
object.size(world4)
object.size(world)

## Saving
save(world, file = "./data/world.rda", compress = "xz")

#Inspecting the maps and object sizes
# format(object.size(world0), units = "Mb") ## 15.8 Mb
# format(object.size(world2), units = "Mb") ## 15.6 Mb (not much but borders remains almost the same)
# format(object.size(world), units = "Mb") ## 10.3 Mb before; now 0.4 Mb
# sp::plot(world0, ylim=c(-24,-22), xlim=c(-45, -42))
# sp::plot(world4, add = TRUE, border = 3)
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

