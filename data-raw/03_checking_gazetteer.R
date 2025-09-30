# SCRIPT TO CHECK THE INTERNAL GAZETTEER AND MISSING LOCALITIES

## Checking for problems in loc.correct search string -------------------------------------
dic <- read.csv("./data-raw/raw_dictionaries/gazetteer.csv", as.is = TRUE,
                encoding = "UTF-8")
dic$loc <- plantR:::squish(dic$loc)

names(dic)[grepl("NAME_",names(dic))] <-
  c("country", "stateProvince", "municipality", "locality", "sublocality")
tmp <- dic[, c("country", "stateProvince", "municipality", "locality", "sublocality",
               "order", "source", "status")]
dim(tmp) # was 35752 entries in 09/2025 and 42712 after the update

# Any duplicated order?
check_these <- duplicated(dic$order)
any(check_these) # Should be all FALSE: all accepted entries have a loc.correct
stopifnot(!any(check_these))

# Any missing loc.correct for accepted entries?
check_these <- is.na(dic$loc.correct) & grepl("ok", dic$status)
any(check_these) # Should be all FALSE: all accepted entries have a loc.correct
stopifnot(!any(check_these))

# Any gazzetter entry that does not have a loc.correct?
#Formatting the gazetteer locality info
tmp1 <- plantR::formatLoc(tmp)
#Flagging problematic records by names
check_these <- is.na(tmp1$loc.correct)
any(check_these) # Should be FALSE: all gazetteer entries have a match, at least at country level
stopifnot(!any(check_these))

# Any gazzetter entry that does not have a loc.correct at least at the stateProvince level?
#Flagging problematic records by names
tmp2 <- plantR::validateLoc(tmp1)
class2check <- c("check_local.2country",
                 "check_municip.2country", "check_state2country")
check_these <- #grepl("gadm|ibge", tmp2$source, perl = TRUE) &
                grepl("ok", tmp2$status, perl = TRUE) &
                tmp2$loc.check %in% class2check
any(check_these) # Should be FALSE: all gazetteer entries have a match, at least at country level
# stopifnot(!any(check_these))
table(tmp2$status[check_these], tmp2$source[check_these])
orders_to_check <- data.frame(order = tmp2$order[check_these],
                              checagem = "no_state_found_check_loc_or_NAMES")


# Possibly problematic records by strings
check_these <- tmp1$loc != dic$loc & tmp1$loc.correct != dic$loc.correct
check_these[is.na(check_these)] <- FALSE
tmp3 <- cbind(dic[, c("order","source", "status", "country", "stateProvince", "municipality", "locality", "loc")],
              tmp1[, c("loc", "loc.correct")])
tmp3$check <- check_these
sort(table(tmp2$source[check_these]))
tmp3[check_these & grepl("gadm", tmp3$source), ]
tmp3[check_these & grepl("ibge", tmp3$source), ]
tmp3[check_these & grepl("google", tmp3$source), ] # tycoporo forest reserve é ok!
tmp3[check_these & grepl("types", tmp3$source), ]
tail(tmp3[check_these & grepl("splink", tmp3$source), ])
df2check <- data.frame(order = tmp3$order[check_these],
                              checagem = "check_loc_string?")
orders_to_check <- rbind.data.frame(orders_to_check, df2check)
# writexl::write_xlsx(tmp3, "gazet.check.loc.string.xlsx")

## Checking for localities in the map but not in the gazetteer --------------------------------------------
toto <- latamMap
rep_these <- (plantR::prepLoc(toto$brazil$NAME_2) == plantR::prepLoc(toto$brazil$NAME_3) |
                toto$brazil$NAME_2 == toto$brazil$NAME_3) #&
              # !(duplicated(paste0(toto$brazil$NAME_1, toto$brazil$NAME_2)) |
              #    duplicated(paste0(toto$brazil$NAME_1, toto$brazil$NAME_2), fromLast = TRUE))
if(any(rep_these))
  toto$brazil$NAME_3[rep_these] <- NA

latam_all0 <- latam_all <- dplyr::bind_rows(toto)
sf::st_geometry(latam_all) <- NULL
#latam_all <- terra::as.data.frame(latam_all)
names(latam_all) <- c("country", "stateProvince","municipality","locality")
latam_all1 <- plantR::fixLoc(latam_all)
latam_all1 <- plantR::strLoc(latam_all1)
latam_all1$loc.string <- plantR::prepLoc(latam_all1$loc.string)
latam_all1$loc.string1 <- plantR::prepLoc(latam_all1$loc.string1)
locs <- plantR::getLoc(x = latam_all1)
colunas <- c("loc", "loc.correct", "latitude.gazetteer",
             "longitude.gazetteer", "resolution.gazetteer")
colunas <- colunas[colunas %in% names(locs)]
latam_all2 <- cbind.data.frame(latam_all, latam_all1,
                       locs[, colunas], stringsAsFactors = FALSE)

dic <- read.csv("./data-raw/raw_dictionaries/gazetteer.csv", as.is = TRUE,
                encoding = "UTF-8")
add_these <- !latam_all2$loc.string %in% dic$loc.correct[grepl("gadm|ibge", dic$source)] |
                !latam_all2$loc.string1 %in% dic$loc.correct[grepl("gadm|ibge", dic$source)]
if (any(add_these)) {
  coords <- sf::st_coordinates(sf::st_centroid(latam_all0[add_these, ]))
  #coords <- terra::crds(terra::centroids(latam_all0[add_these, ]))
  tmp2 <- cbind.data.frame(latam_all2[add_these, ], coords)
  tmp3 <- cbind.data.frame(tmp2, plantR::getAdmin(tmp2)[, c("NAME_0", "NAME_1", "NAME_2", "NAME_3")])

  col2keep <- c("resol.orig", "loc.string", "loc.string1", "resolution.gazetteer",
              "Y", "X", "NAME_0", "NAME_1", "NAME_2", "NAME_3", "loc", "loc.correct",
              "stateProvince", "municipality", "locality")
  tmp4 <- tmp3[, c(col2keep)]

  tmp4$resol.orig  <- gsub("stateProvince", "state", tmp4$resol.orig)
  tmp4$resol.orig  <- gsub("municipality", "county", tmp4$resol.orig)
  tmp4$check <- tmp4$resol.orig != tmp4$resolution.gazetteer
  tmp4 <- tmp4[tmp4$check,]

  rep_these <- is.na(tmp4$NAME_1) & !is.na(tmp4$stateProvince)
  if(any(rep_these))
    tmp4$NAME_1[rep_these] <- tmp4$stateProvince[rep_these]

  rep_these <- is.na(tmp4$NAME_2) & !is.na(tmp4$municipality)
  if(any(rep_these)) {
    tmp4$NAME_2[rep_these] <- stringr::str_to_title(tmp4$municipality[rep_these])
    tmp4$check[rep_these] <- "TRUE_state"
  }

  rep_these <- is.na(tmp4$NAME_3) & !is.na(tmp4$locality)
  if(any(rep_these))
    tmp4$NAME_3[rep_these] <- stringr::str_to_title(tmp4$locality[rep_these])

  colnames(tmp4) <- c("resolution.gazetteer", "loc", "loc.correct", "resolution.gazetteer_old",
                      "latitude.gazetteer", "longitude.gazetteer", "NAME_0", "NAME_1", "NAME_2", "NAME_3",
                      "loc_old", "loc.correct_old",
                      "stateProvince.new", "municipality.new", "locality.new",
                      "check")

  last_order <- max(dic$order, na.rm = TRUE) + 1
  tmp4$order <- last_order:(last_order + dim(tmp4)[1] - 1)
  tmp4$source <- "gadm_new"
  tmp4$status <- "ok?"

  tmp5 <- tmp4[, c("order", "source", "status",
                   "NAME_0", "NAME_1", "NAME_2", "NAME_3",
                   "loc", "loc.correct",
                   "latitude.gazetteer", "longitude.gazetteer", "resolution.gazetteer")]
  tmp6 <- dplyr::bind_rows(dic, tmp5)
  tmp6 <- tmp6[which(tmp6$order >= last_order), ]
  tmp7 <- tmp6[!grepl("[0-9]", tmp6$loc.correct, perl = TRUE), ]
  writexl::write_xlsx(tmp7, "new_entries_gazetteer.xlsx")
}


## Checking the precision of the coordinates in the gazetteer -----------------------------------------------
## CHECKING THE GAZETTEER COORDINATES ##
dic <- read.csv("./data-raw/raw_dictionaries/gazetteer.csv", as.is = TRUE,
                encoding = "UTF-8")
dic$loc <- plantR:::squish(dic$loc)

#Renaming dictionaries to the DwC standard
col.loc <-
  c("country", "stateProvince", "municipality", "locality", "sublocality")
names(dic)[grepl("NAME_",names(dic))] <- col.loc

col.coord <- c("decimalLatitude.new", "decimalLongitude.new", "resolution.gazetteer")
names(dic)[grepl(".gazetteer",names(dic))] <- col.coord
#Filtering the columns
tmp <- dic[, c("order", "status", "source", col.loc, "loc", "loc.correct", col.coord)]
tmp1 <- tmp[grepl("^ok", tmp$status, perl = TRUE), ]
tmp1$origin.coord <- "coord_original"

## Validating gazetteer coordinates
tmp2 <- plantR::checkCoord(tmp1,
                           keep.cols = c("geo.check", "NAME_0", "NAME_1", "NAME_2", "NAME_3"))
tmp3 <- dplyr::left_join(tmp, tmp2)

## Creating classes for validation
tmp3$geo.check <- gsub("^ok_", "", tmp3$geo.check)
tmp3$check_these <- grepl("^ok", tmp3$status, perl = TRUE) &
                    tmp3$source %in% c("gadm","gadm_new","gadm?", "ibge","ibge?","types","gadm_treeco","ibge_treeco") &
                    tmp3$resolution.gazetteer %in% c("country","state","county") &
                    (tmp3$geo.check %in% c("bad_country", "sea") | tmp3$resolution.gazetteer != tmp3$geo.check)
tmp3$check_these1 <- grepl("^ok", tmp3$status, perl = TRUE) &
                    tmp3$source %in% c("gbif","gbif_gsg","google","treeco","splink_jabot","cncflora") &
                    tmp3$resolution.gazetteer %in% c("country","state","county") &
                    (tmp3$geo.check %in% c("bad_country", "sea") | tmp3$resolution.gazetteer != tmp3$geo.check)
tmp3$pais <- sapply(tmp3$loc.correct, function (x) strsplit(x, "_")[[1]][1])
tmp3$estado <- sapply(tmp3$loc.correct, function (x) strsplit(x, "_")[[1]][2])
tmp3$municipio <- sapply(tmp3$loc.correct, function (x) strsplit(x, "_")[[1]][3])
tmp3$replace_these <- tmp3$pais == tmp3$NAME_0 & tmp3$estado == tmp3$NAME_1 &
                      tmp3$resolution.gazetteer %in% "county" &
                      tmp3$geo.check == "state" &
                      tmp3$municipio == tmp3$NAME_3
tmp3$replace_these[is.na(tmp3$replace_these) & !tmp3$check_these & !tmp3$check_these1 & !is.na(tmp3$decimalLatitude.new)] <- TRUE
tmp3$replace_these[is.na(tmp3$replace_these)] <- FALSE
toto <- tmp3[tmp3$check_these1 , c("source","loc.correct", "NAME_2", "NAME_3","decimalLatitude.new","decimalLongitude.new")]
toto[order(toto$loc.correct),]
toto <- tmp3[tmp3$check_these, c("source","loc.correct", "NAME_2", "NAME_3","decimalLatitude.new","decimalLongitude.new")]
toto[order(toto$loc.correct),]

locs <- tmp3$loc.correct[tmp3$replace_these %in% TRUE]
paises <- sapply(locs, function (x) strsplit(x, "_")[[1]][1])
estados <- sapply(locs, function (x) strsplit(x, "_")[[1]][2])
bad <- tmp3$municipality[tmp3$replace_these %in% TRUE]
good <- tmp3$municipio[tmp3$replace_these %in% TRUE]
toto1 <- data.frame(pais = as.character(paises), estado = as.character(estados),
                    bad.mun = as.character(bad), replace.mun = as.character(good))
toto1$case <- "second"
writexl::write_xlsx(toto1, "./data-raw/check.xlsx")
# write.csv(toto1, "./data-raw/check.csv", fileEncoding = "UTF-8")
# # write.csv(tmp3, "gazet.check.tmp.csv")
# # map.replace <- list(locs = tmp3$loc.correct[tmp3$replace_these %in% TRUE],
# #                      municipio.bad = tmp3$NAME_2[tmp3$replace_these %in% TRUE],
# #                      municipio.good = tmp3$NAME_3[tmp3$replace_these %in% TRUE])
# # saveRDS(map.replace, "./data-raw/map.replacements.rds")

## Preparing the table for checking
# coord.check <- tmp3[tmp3$check_these | tmp3$check_these1, ]
# coord.check <- tmp3[tmp3$replace_these %in% TRUE, ]
# coord.check$name <- coord.check$adm <- NA
# adm.lvl <- coord.check$resolution.gazetteer %in% "country"
# coord.check$name[adm.lvl] <- coord.check$country[adm.lvl]
# coord.check$adm[adm.lvl] <- "ADM0"
#
# adm.lvl <- coord.check$resolution.gazetteer %in% "state"
# coord.check$name[adm.lvl] <- coord.check$stateProvince[adm.lvl]
# coord.check$adm[adm.lvl] <- "ADM1"
#
# adm.lvl <- coord.check$resolution.gazetteer %in% "county"
# coord.check$name[adm.lvl] <- coord.check$municipality[adm.lvl]
# coord.check$adm[adm.lvl] <- "ADM2"
#
# #Removing species characters
# coord.check$name <- textclean::replace_non_ascii(coord.check$name)
#
# ## Getting names and coordinates from GeoNames
# library(XML)
# datahere = "C:/Users/renato/Documents/raflima/R_packages/Backups/plantR"
# resultados <- vector("list", dim(coord.check)[1])
# names(resultados) <- coord.check$order
# for (i in 1:dim(coord.check)[1]) {
#   name <- coord.check$name[i]
#   adm <- coord.check$adm[i]
#   pais <- coord.check$country[i]
#   file <- paste0("http://api.geonames.org/search?q=", name,
#                  "&fcode=", adm,
#                  "&countryName=", pais,
#                  "&featureClass=A&username=raflima")
#   download.file(file, paste(datahere, "tmp.xml", sep = ""))
#   tt <- xmlParse(paste(datahere, "tmp.xml", sep = ""))
#   out <- xmlToList(tt)$geoname
#   if (is.null(out)) {
#     nomes <- c("toponymName","name","lat","lng","geonameId","countryCode","countryName","fcl","fcode")
#     df <- rep(NA, length(nomes))
#     names(df) <- nomes
#   } else {
#     df <- data.frame(do.call(cbind, out))
#   }
#   resultados[[i]] <- df
#   cat(i,"\n")
# }
# tmp4 <- do.call(rbind, resultados)
# table(names(resultados) == coord.check$order)
# coord.check <- cbind.data.frame(coord.check, tmp4)
#
# ## Distance between gazetter coordinates and geoNames coordinates
# coord.check$dist_km <- NA
# x1 <- as.matrix(coord.check[!is.na(coord.check$lng),c("decimalLongitude.new","decimalLatitude.new")])
# dimnames(x1) <- NULL
# x2 <- matrix(c(as.double(coord.check[!is.na(coord.check$lng),c("lng")]),
#                as.double(coord.check[!is.na(coord.check$lng),c("lat")])),
#              ncol = 2)
# coord.check$dist_km[!is.na(coord.check$lng)] <-
  # fields::rdist.earth.vec(x1, x2, miles = FALSE)
# coord.check[!is.na(coord.check$dist_km) & coord.check$dist_km>500,
#             c("source","country","stateProvince","municipality",
#               "decimalLatitude.new","decimalLongitude.new","lat","lng","countryName")]
# coord.check[!is.na(coord.check$dist_km) & coord.check$dist_km>100 & coord.check$dist_km<=500,
#             c("source","country","stateProvince","municipality",
#               "decimalLatitude.new","decimalLongitude.new","lat","lng","countryName")]
#
# tmp5 <- merge(tmp3, coord.check,
#               by = "order", all.x = TRUE, sort = FALSE)
tmp5 <- tmp3
tmp5 <- tmp5[match(dic$order, tmp5$order),]
table(dic$order == tmp5$order)
dim(tmp5[tmp5$check_these, ])[1] # numero de locais do gazeteiro que não batem com o mapa
head(tmp5[tmp5$check_these, ])
dim(tmp5[tmp5$check_these1, ])[1] # numero de locais do gazeteiro que não batem com o mapa
head(tmp5[tmp5$check_these1, ])
dim(tmp5[tmp5$replace_these, ])[1] # numero de locais do gazeteiro que não batem com o mapa
head(tmp5[tmp5$replace_these, ])
df2check <- data.frame(order = tmp5$order[tmp5$check_these %in% TRUE],
                       checagem = "coordenada do gazeteiro não bate com o mapa")
orders_to_check <- rbind.data.frame(orders_to_check, df2check)
df2check <- data.frame(order = tmp5$order[tmp5$check_these1 %in% TRUE],
                       checagem = "erro_de_hierarquia_ou_no_mapa")
orders_to_check <- rbind.data.frame(orders_to_check, df2check)
df2check <- data.frame(order = tmp3$order[tmp3$replace_these %in% TRUE],
                       checagem = "possivel erro de coordenada no gazeteiro")
orders_to_check <- rbind.data.frame(orders_to_check, df2check)
# writexl::write_xlsx(tmp5, "data-raw/gazet.check.tmp.xlsx")
# write.csv(tmp5, "gazet.check.tmp.csv")
to_save <- aggregate(orders_to_check$checagem,
                     list(orders_to_check$order),
                     paste, collapse = "|")
names(to_save) <- c("order", "checagem")
dic <- read.csv("./data-raw/raw_dictionaries/gazetteer.csv", as.is = TRUE,
                encoding = "UTF-8")
to_save_order <- dplyr::left_join(dic, to_save, by = "order")
writexl::write_xlsx(to_save_order, "data-raw/gazet.check.tmp.xlsx")

## -------------------------------------------------------------------------------
## PLOTTING POSSIBLE PROBLEMS IN THE GAZETTEER
# Bahamas
tt <- latamMap$bahamas
tt <- tt[order(sf::st_coordinates(sf::st_centroid(tt))[,2]),]
plot(tt[2], reset=FALSE, col = "white")
nomes <- tt$NAME_1[c(27:30)]
plot(tt[tt$NAME_1 %in% nomes, 2], add = TRUE, lwd = 2)

nomes <- tt$NAME_1[c(20, 21, 22)]
plot(tt[tt$NAME_1 %in% nomes, 2])
# Issue fixed in the 4.1 GADM version? No!
destfile.i <- "C:/Users/renat/Documents/R_packages/plantRdata/data-raw/gadm/gadm41_BHS.gpkg"
map.i <- sf::st_as_sf(terra::vect(destfile.i, "ADM_ADM_1"))
plot(map.i[plantR::prepLoc(map.i$NAME_1) %in% nomes[3], "NAME_1"])

## Original island tagged as Spanish Wells is actually Royal Island
## Original island tagged as Harbour Island is actually Spanish Wells
## Harbour Island is originally part of North Eleuthera but should be a separated ADM_1/polygon



## -------------------------------------------------------------------------------
## CHECKING MISSING LOCALITIES IN THE GAZETTEER
# From the worldMap
mundo <- sf::st_drop_geometry(worldMap)
names(mundo) <- c("GID_0", "country")
mundo1 <- formatLoc(mundo, loc.levels = c("country"),
                    adm.names = c("country.new"))
head(mundo1[is.na(mundo1$loc.correct),]) # ok if empty
head(mundo1[mundo1$country.new != mundo1$loc.correct,]) # ok if empty
names(mundo1)[names(mundo1) %in% c("latitude.gazetteer", "longitude.gazetteer")] <-
  c("decimalLatitude.new", "decimalLongitude.new")
mundo1$origin.coord <- "coord_original"
mundo2 <- plantR::checkCoord(mundo1, keep.cols = c("geo.check", "NAME_0", "NAME_1", "NAME_2", "NAME_3"))
table(mundo2$geo.check) # must be all "ok_country

# From latamMap
latam_all <- dplyr::bind_rows(latamMap)
sf::st_geometry(latam_all) <- NULL
names(latam_all) <- c("country", "stateProvince","municipality","locality")
latam_all1 <- formatLoc(latam_all)
head(latam_all1[is.na(latam_all1$loc.correct),]) # ok if empty

dic <- read.csv("./data-raw/raw_dictionaries/gazetteer.csv", as.is = TRUE,
                 encoding = "UTF-8")
head(latam_all1[!latam_all1$loc %in% unique(dic$loc),]) #ok

