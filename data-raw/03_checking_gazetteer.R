# #### CHECKING THE GAZETTER ####
#
#
# ## CHECKING THE FORMAT OF THE SEARCH STRING ##
# dic <- read.csv("./data-raw/raw_dictionaries/gazetteer.csv", as.is = TRUE,
#                 encoding = "UTF-8")
#
# #Renaming dictionaries to the DwC standard
# names(dic)[grepl("NAME_",names(dic))] <-
#   c("country", "stateProvince", "municipality", "locality", "sublocality")
# tmp <- dic[, c("country", "stateProvince", "municipality", "locality", "sublocality")]
# #Formatting the gazetteer locality info
# tmp <- formatLoc(tmp)
# #Flagging problematic records by names
# check_these <- is.na(tmp$loc.correct) #& dic$status %in% "ok"
# head(tmp[check_these, ])
#
# #Flagging problematic records by strings
# check_these <- tmp$loc != dic$loc
# tmp1 <- cbind(dic[, c("X.U.FEFF.order","country", "stateProvince", "municipality", "locality", "loc")], tmp[, "loc"])
# tmp1$check <- check_these
# write.csv(tmp1, "gazet.check.tmp.csv")
#
# ## CHECKING THE GAZETTEER COORDINATES ##
# dic <- read.csv("./data-raw/raw_dictionaries/gazetteer.csv", as.is = TRUE,
#                 encoding = "UTF-8")
#
# #Renaming dictionaries to the DwC standard
# col.loc <-
#   c("country", "stateProvince", "municipality", "locality", "sublocality")
# names(dic)[grepl("NAME_",names(dic))] <- col.loc
#
# col.coord <- c("decimalLatitude.new", "decimalLongitude.new", "resolution.gazetteer")
# names(dic)[grepl(".gazetteer",names(dic))] <- col.coord
# #Filtering the columns
# tmp <- dic[, c("X.U.FEFF.order", "status", "source", col.loc, "loc", "loc.correct", col.coord)]
# tmp1 <- tmp[tmp$status %in% "ok", ]
# tmp1$origin.coord <- "coord_original"
#
# ## Validating gazetteer coordinates
# tmp2 <- plantR::checkCoord(tmp1, keep.cols = c("geo.check", "NAME_0", "NAME_1", "NAME_2", "NAME_3"))
# tmp3 <- dplyr::left_join(tmp, tmp2)
#
# ## Creating classes for validation
# tmp3$geo.check <- gsub("^ok_", "", tmp3$geo.check)
# tmp3$check_these <- tmp3$status %in% "ok" &
#                     tmp3$source %in% c("gdam","ibge","ibge?","types","gdam_treeco","ibge_treeco") &
#                     tmp3$resolution.gazetteer %in% c("country","state","county") &
#                     (tmp3$geo.check %in% c("bad_country", "sea") | tmp3$resolution.gazetteer != tmp3$geo.check)
# tmp3$check_these1 <- tmp3$status %in% "ok" &
#                     tmp3$source %in% c("gbif","google","treeco","splink_jabot","cncflora") &
#                     tmp3$resolution.gazetteer %in% c("country","state","county") &
#                     (tmp3$geo.check %in% c("bad_country", "sea") | tmp3$resolution.gazetteer != tmp3$geo.check)
# tmp3$pais <- sapply(tmp3$loc.correct, function (x) strsplit(x, "_")[[1]][1])
# tmp3$estado <- sapply(tmp3$loc.correct, function (x) strsplit(x, "_")[[1]][2])
# tmp3$municipio <- sapply(tmp3$loc.correct, function (x) strsplit(x, "_")[[1]][3])
# tmp3$replace_these <- tmp3$pais == tmp3$NAME_0 & tmp3$estado == tmp3$NAME_1 &
#                       tmp3$resolution.gazetteer %in% "county" &
#                       tmp3$geo.check == "state" &
#                       tmp3$municipio == tmp3$NAME_3
# toto <- tmp3[tmp3$check_these1 , c("source","loc.correct", "NAME_2", "NAME_3","decimalLatitude.new","decimalLongitude.new")]
# toto[order(toto$loc.correct),]
# toto <- tmp3[tmp3$check_these, c("source","loc.correct", "NAME_2", "NAME_3","decimalLatitude.new","decimalLongitude.new")]
# toto[order(toto$loc.correct),]
#
# # # write.csv(tmp3, "gazet.check.tmp.csv")
# # # map.replace <- list(locs = tmp3$loc.correct[tmp3$replace_these %in% TRUE],
# # #                      municipio.bad = tmp3$NAME_2[tmp3$replace_these %in% TRUE],
# # #                      municipio.good = tmp3$NAME_3[tmp3$replace_these %in% TRUE])
# # # saveRDS(map.replace, "./data-raw/map.replacements.rds")
#
# ## Preparing the table for checking
# # coord.check <- tmp3[tmp3$check_these | tmp3$check_these1, ]
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
# names(resultados) <- coord.check$X.U.FEFF.order
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
# table(names(resultados) == coord.check$X.U.FEFF.order)
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
#   fields::rdist.earth.vec(x1, x2, miles = FALSE)
# # coord.check[!is.na(coord.check$dist_km) & coord.check$dist_km>500,
# #             c("source","country","stateProvince","municipality",
# #               "decimalLatitude.new","decimalLongitude.new","lat","lng","countryName")]
# # coord.check[!is.na(coord.check$dist_km) & coord.check$dist_km>100 & coord.check$dist_km<=500,
# #             c("source","country","stateProvince","municipality",
# #               "decimalLatitude.new","decimalLongitude.new","lat","lng","countryName")]
#
# tmp5 <- merge(tmp3, coord.check[, c("X.U.FEFF.order", names(tmp4))],
#               by = "X.U.FEFF.order", all.x = TRUE, sort = FALSE)
# tmp5 <- tmp5[match(dic$X.U.FEFF.order, tmp5$X.U.FEFF.order),]
# table(dic$X.U.FEFF.order == tmp5$X.U.FEFF.order)
# write.csv(tmp5, "gazet.check.tmp.csv")
#
#
# ## CHECKING MISSING LOCALITIES IN THE GAZETTEER ##
# occs1 <- formatLoc(occs0)
# # occs1[occs1$resolution.gazetteer %in% "no_info" & !is.na(occs1$loc),
# #       c("country","country.new","loc","loc.correct")]
# occs1 <- formatCoord(occs1)
# occs2 <- validateLoc(occs1)
# # occs2[grepl("2country",occs2$loc.check) & !occs2$country.new %in% "united states" ,
# #       c("country","country.new","loc","loc.correct")]
# head(unique(occs2[grepl("2state",occs2$loc.check) & !occs2$country.new %in% "united states" ,
#                   c("stateProvince","stateProvince.new","loc","loc.correct")]))
# sort(table(occs2[grepl("2state",occs2$loc.check) & occs2$country.new %in% "brazil" ,
#                  c("loc")]))
# sort(table(occs2[grepl("2state",occs2$loc.check) & !occs2$country.new %in% "united states" ,
#                  c("stateProvince")]))
#
# occs3 <- validateCoord(occs2)
