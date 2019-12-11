#' @title Validate Geographical Coordinates
#'
#' @description
#'
#' @param x Coordinates in decimal degrees
#'
validateCoord = function(x) {
  loc = x
  return(loc)
}
#############################################-
#### GEOGRAPHICAL COORDINATES VALIDATION ####
#############################################-
# ### MAP FOR VALIDATION AT COUNTRY-LEVEL ###
# ##Loading, editing and converting the world country shapefile
# path = "E://ownCloud//W_GIS"
# #wo <- readOGR(dsn=paste(path,"//WO_ADM_limits_GADM36//gadm36_levels_shp",sep=""),layer="gadm36_0")
# #wo@data$pais = countrycode(wo@data$GID_0,'iso3c', 'country.name')
# #wo@data$pais[is.na(wo@data$pais)] = as.character(wo@data$NAME_0[is.na(wo@data$pais)])
# #wo@data$pais = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), wo@data$pais))
# #wo@data$pais = sapply(strsplit(wo@data$pais,' - | \\('), function(x) x[[1]][1])
# #wo@data$pais = str_trim(wo@data$pais)
# #wo@data$pais = gsub("-"," ",wo@data$pais)
# #wo@data$pais = gsub(" - "," / ",wo@data$pais)
# #wo@data$pais = gsub(" do | da | de "," ",wo@data$pais)
# #wo@data$pais = gsub(" dos | das | des "," ",wo@data$pais)
# #wo@data$pais = gsub(' d\' | d\'| d?| d\"| d?'," ",wo@data$pais)
# #wo@data$pais = gsub("\\.$","",wo@data$pais)
# #wo@data$pais = gsub(" dx "," ",wo@data$pais)
#
# #Saving the edited shapefile as .rds
# #saveRDS(wo,file=paste(path,"//WO_ADM_limits_GADM36//gadm36_levels_shp//gadm36_0.rds",sep=""))
#
# #Loading the edited .rds with the country limits of the world
# wo1 = readRDS(file=paste(path,"//WO_ADM_limits_GADM36//gadm36_levels_shp//gadm36_0.rds",sep=""))
#
# #Defining the projection used by the world country shapefile
# pj = crs(wo1) #defining and standardizing the geographical projections
#
# ### MAP FOR VALIDATION AT STATE AND COUNTY LEVEL - ARG, BRA AND PAY ###
# #Loading and edititing country-specific shapefiles
# path0 = "E://ownCloud//W_GIS//Am_Lat_ADM_GADM_v3.6//"
# countries = list.files(path0,full.name=TRUE)
# #countries = countries[grep("BRA|ARG|PRY",countries)]
# countries = countries[grep("ARG|BOL|BRA|CHL|COL|ECU|GUY|GUF|SUR|PRY|PER|URY|VEN|MEX",countries)]
# countries = countries[grep('_2_sp.rds',countries)]
# country.list = vector('list',length(countries))
# pais = NULL
# for (i in 1:length(country.list)) {
#   path = countries[i]
#   tmp = readRDS(path)
#   tmp1 = tmp@data
#   tmp1 = tmp1[,c("NAME_0","NAME_1","NAME_2")]
#   pais[i] = unique(tmp1$NAME_0)
#   #Editing names to match the loc format
#   tmp1$NAME_0 = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), tmp1$NAME_0))
#   tmp1$NAME_1 = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), tmp1$NAME_1))
#   tmp1$NAME_2 = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), tmp1$NAME_2))
#   tmp1 = apply(tmp1,2,function(x) as.character(gsub("-"," ",x)))
#   tmp1 = apply(tmp1,2,function(x) as.character(gsub(" - "," / ",x)))
#   tmp1 = apply(tmp1,2,function(x) as.character(gsub(" do | da | de "," ",x)))
#   tmp1 = apply(tmp1,2,function(x) as.character(gsub(" dos | das | des "," ",x)))
#   tmp1 = apply(tmp1,2,function(x) as.character(gsub(' d\' | d\'| d?| d\"| d?'," ",x)))
#   tmp1 = apply(tmp1,2,function(x) as.character(gsub('\\.$',"",x)))
#   tmp1 = apply(tmp1,2,function(x) as.character(gsub(" dx "," ",x)))
#   tmp@data = as.data.frame(tmp1)
#   country.list[[i]] = tmp
# }
# names(country.list) = pais
#
# ## Standardizing the shapefile and the gazetteer names for Brazil
# #Creating the locality strings for the shapefile dataframe
# tmp = country.list$Brazil@data
# tmp[,1] = as.character(tmp[,1]); tmp[,2] = as.character(tmp[,2]); tmp[,3] = as.character(tmp[,3])
# tmp$order = 1:dim(tmp)[1]
# tmp1= data.frame(loc=paste(tmp[,1],tmp[,2],tmp[,3],sep="_"))
# tmp1$loc = as.character(tmp1$loc)
# tmp1$order = 1:dim(tmp1)[1]
# #loading the gazetteer and removing possible duplicated localities
# dic = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//gazetteer.csv",as.is=TRUE)
# dic = dic[dic$status %in% "ok",]
# #dic1= dic[!duplicated(dic$loc),]
# #dic1= dic1[!dic1$resolution.gazetteer%in%c("localidade","localidade|sublocalidade","distrito|vila","distrito|bairro","cachoeira","mina","vila","serra"),]
# #merging the two sources of info
# tmp2= merge(tmp1,dic[,c("NAME_0","NAME_1","NAME_2","loc","loc.correct")],by="loc",all.x=TRUE)
# tmp3 = strsplit(tmp2$loc.correct,"_")
# n0 = rep(NA,dim(tmp2)[1]); n1 = rep(NA,dim(tmp2)[1]); n2 = rep(NA,dim(tmp2)[1])
# n0 = sapply(tmp3,function(x) x[1])
# n1 = sapply(tmp3,function(x) x[2])
# n2 = sapply(tmp3,function(x) x[3])
# tmp2$country = n0
# tmp2$state = n1
# tmp2$county = n2
# #replacing and saving the correct info
# tmp2 = tmp2[order(tmp2$order),]
# table(tmp$order == tmp2$order)
# id = !tmp2$loc == tmp2$loc.correct
# table(tmp[id,1] == tmp2[id,c("country")])
# #tmp[id,1] = tmp2[id,c("country")]
# table(tmp[id,2] == tmp2[id,c("state")]) #all differences were double-checked and the gazetteer is right!
# cbind(tmp[id,c(2)], tmp2[id,c("state","county")])[tmp[id,2] == tmp2[id,c("state")],]
# #tmp[id,2] = tmp2[id,c("state")]
# table(!tmp[id,3] == tmp2[id,c("county")]) #all differences were double-checked and the gazetteer is right!
# cbind(tmp[id,c(2,3)], tmp2[id,c("county")])[!tmp[id,3] == tmp2[id,c("county")],]
# tmp[id,3] = tmp2[id,c("county")]
# country.list$Brazil@data = tmp[,c("NAME_0","NAME_1","NAME_2")]
#
#
# #### 1 - PERFORMING THE VALIDATION ITSELF ####
#
# #### CHECK: Why some cases are not being correctly validated at country level? (e.g. numTombo: ALCB 82966, GB GB-30071)
# #### CHECK: Why some cases are not being correctly validated at country level? (e.g. numTombo: ALCB 82966, GB GB-30071)
#
# #Defining the projection used by the world country shapefile
# pj1 = crs(country.list$Brazil) #defining and standardizing the geographical projections
#
# ## Uploading the final/edited version of the gazetter, with missing counties and ortographical variants of some names (not exaustive)
# dic = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//gazetteer.csv",as.is=TRUE)
# dic = dic[dic$status %in% "ok",]
#
# ##Getting the file paths for the herbarium data
# splink = read.csv("speciesLink.csv", as.is=T)
# paths = dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink",full.names=TRUE)
# paths1 = "C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink_new"
# paths2 = "C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//jabot"
# #paths1 = dir(paths1,full.names=TRUE)[!dir(paths1) %in% splink$acronym]
# #paths2 = dir(paths2,full.names=TRUE)[!dir(paths2) %in% splink$acronym]
# paths1 = dir(paths1,full.names=TRUE)
# paths2 = dir(paths2,full.names=TRUE)
#
# paths = c(paths,paths1,paths2)
# paths = list.files(paths,full.names=TRUE)
# paths = c(paths, list.files("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//gbif",full.names=TRUE))
# paths = paths[grep("-edited-cleaned-loc-coord-tax.csv",paths)]
#
# ### Performing the geographical validation for all collections ###
# for(i in 1:length(paths)) {
#   ### PRE-VALIDATION STEPS ###
#   ##Loading the occurrence data
#   myfiles0 = paths[i]
#   herb.data = read.csv(myfiles0,as.is=TRUE,header=TRUE,na.strings=c(""," ","NA"))
#   ##Removing unwanted columns
#   cols = c("loc.correct","latitude.gazetteer","longitude.gazetteer","origin.coord","resolution.coord","latitude.work","longitude.work")
#   cls =  unique(cols[cols %in% names(herb.data)])
#   herb.data1 = herb.data[,cls]
#   herb.data1$order = 1:dim(herb.data1)[1] #putting the data back on its original order
#   ##Defining the country, state and county columns
#   tmp = strsplit(herb.data1$loc.correct,"_")
#   id = sapply(tmp,length)
#   n0 = rep(NA,dim(herb.data1)[1]); n1 = rep(NA,dim(herb.data1)[1]); n2 = rep(NA,dim(herb.data1)[1])
#   n0[id>=1] = sapply(tmp[id>=1],function(x) x[1])
#   n1[id>=2] = sapply(tmp[id>=2],function(x) x[2])
#   n2[id>=3] = sapply(tmp[id>=3],function(x) x[3])
#   herb.data1$country = n0
#   herb.data1$state = n1
#   herb.data1$county = n2
#   ##Creating the geo.check columns
#   herb.data1$geo.check = NA
#   herb.data1$geo.check[herb.data1$origin.coord %in% c("ok_country_gazet")] = "ok_country_gazet"
#   herb.data1$geo.check[herb.data1$origin.coord %in% c("ok_state_gazet")] = "ok_state_gazet"
#   herb.data1$geo.check[herb.data1$origin.coord %in% c("ok_county_gazet")] = "ok_county_gazet"
#   herb.data1$geo.check[herb.data1$origin.coord %in% c("ok_locality_gazet")] = "ok_locality_gazet"
#   herb.data1$geo.check[herb.data1$origin.coord %in% "no_coord"] = "no_coord"
#   herb.data1$geo.check[is.na(herb.data1$latitude.work)&is.na(herb.data1$latitude.gazetteer)] = "no_coord"
#   ##Subsetting the coordinates that need to be check
#   tmp = herb.data1[herb.data1$origin.coord %in% "coord_original",]
#   if(dim(tmp)[1]>0) {
#     tmp = tmp[!is.na(tmp$longitude.work),]
#     tmp = tmp[!is.na(tmp$latitude.work),]
#     ##Getting data frame with spatial coordinates (points) and standardizing the projection
#     coordinates(tmp) <- c("longitude.work", "latitude.work")  # set spatial coordinates
#     proj4string(tmp) <- pj  # define projection system of our data
#     ##Comparing the spatial data frame with the world country shapefile
#     if(dim(tmp)[1]>20000) {
#       #for big verifications
#       no_cores = detectCores() -1
#       n <- 10 #number of dividision to the input data frame
#       parts <- split(x = 1:dim(tmp)[1], f = cut(1:dim(tmp)[1], n))
#       cl <- makeCluster(no_cores)
#       clusterExport(cl, list("tmp","parts","n","wo1"),envir=environment())
#       clusterEvalQ(cl, library(sp))
#       overParts <- parLapply(cl = cl, X = 1:n, fun = function(x) {
#         over  <- over(tmp[parts[[x]],], wo1)
#         over1 <- cbind.data.frame(tmp[parts[[x]],]@data,pais=as.character(over[,c("pais")]))
#         gc() # release memory
#         return(over1)
#       })
#       stopImplicitCluster()
#       stopCluster(cl)
#       tmp2 = do.call(rbind.data.frame, overParts)
#
#     } else {
#       #for small verifications
#       tmp0 = over(tmp, wo1, fn=NULL) ##Old version, took too long
#       tmp2 = cbind.data.frame(tmp,pais=as.character(tmp0[,c("pais")]))
#     }
#     tmp3 = left_join(herb.data1,tmp2,by="order")
#     herb.data1$pais = as.character(tmp3$pais)
#   } else { 	herb.data1$pais = NA }
#
#   ##Comparing the spatial data frame with the selected country shapefiles
#   lista.paises = tolower(names(country.list))
#   tmp = herb.data1[herb.data1$pais %in% lista.paises,]
#   #tmp = herb.data1[herb.data1$pais %in% c("argentina","brazil","paraguay"),]
#   if(dim(tmp)[1]>0) {
#     if(dim(tmp)[1]>20000) {
#       #For big verifications
#       no_cores = detectCores() - 1
#       n <- unique(tmp$pais)[unique(tmp$pais) %in% lista.paises] #list of countries in the database
#       cl <- makeCluster(no_cores)
#       #clusterExport(cl, list("tmp","n1","n2","lista.paises","pj1","n","country.list"),envir=environment())
#       clusterExport(cl, list("tmp","pj1","n","lista.paises","country.list"),envir=environment())
#       clusterEvalQ(cl, library(sp))
#       overParts <- parLapply(cl = cl, X = 1:length(n), fun = function(x) {
#         tmp1.1 = tmp[tmp$pais %in% n[x],]
#         coordinates(tmp1.1) <- c("longitude.work", "latitude.work")  # set spatial coordinates
#         proj4string(tmp1.1) <- pj1  # define projection system of our data
#         id = which(n[x] == lista.paises)
#         pais.shp = country.list[[id]]
#         over  <- over(tmp1.1, pais.shp, fn=NULL)
#         tmp1.1@data$n1 = as.character(over$NAME_1)
#         tmp1.1@data$n2 = as.character(over$NAME_2)
#         gc() # release memory
#         return(tmp1.1@data)
#       })
#       stopImplicitCluster()
#       stopCluster(cl)
#       tmp2 = do.call(rbind.data.frame, overParts)
#       tmp2 = tmp2[order(tmp2$order),]
#       #Creating the vectors with state and county/province/department names
#       n1 = rep(NA,dim(tmp)[1])
#       n2 = rep(NA,dim(tmp)[1])
#       n1[is.na(n1)&!is.na(as.character(tmp2$n1))] = as.character(tmp2$n1)[is.na(n1)&!is.na(as.character(tmp2$n1))]
#       n2[is.na(n2)&!is.na(as.character(tmp2$n2))] = as.character(tmp2$n2)[is.na(n2)&!is.na(as.character(tmp2$n2))]
#
#     } else {
#       #For small verifications
#       list.countries = unique(tmp$country)
#       list.countries = list.countries[!is.na(list.countries)]
#       list.countries = list.countries[list.countries %in% lista.paises]
#       coordinates(tmp) <- c("longitude.work", "latitude.work")  # set spatial coordinates
#       proj4string(tmp) <- pj1  # define projection system of our data
#       n1 = rep(NA,dim(tmp)[1])
#       n2 = rep(NA,dim(tmp)[1])
#       for (j in 1:length(list.countries)) {
#         pais.shp = which(tolower(names(country.list))==list.countries[j])
#         tmp1 = country.list[[pais.shp]]
#         tmp2 = over(tmp, tmp1, fn=NULL)
#         n1[is.na(n1)&!is.na(as.character(tmp2$NAME_1))] = as.character(tmp2$NAME_1)[is.na(n1)&!is.na(as.character(tmp2$NAME_1))]
#         n2[is.na(n2)&!is.na(as.character(tmp2$NAME_2))] = as.character(tmp2$NAME_2)[is.na(n2)&!is.na(as.character(tmp2$NAME_2))]
#       }
#     }
#     #Saving the state and county/province/department names
#     herb.data1$estado = NA; herb.data1$municipio = NA
#     herb.data1[herb.data1$pais %in% lista.paises,]$estado = n1
#     herb.data1[herb.data1$pais %in% lista.paises,]$municipio = n2
#     herb.data1$loc.coord = paste(herb.data1$pais,herb.data1$estado,herb.data1$municipio,sep="_")
#     herb.data1$loc.coord[herb.data1$loc.coord %in% "NA_NA_NA"] = NA
#   } else {
#     herb.data1$estado = NA
#     herb.data1$municipio = NA
#     herb.data1$loc.coord = NA
#   }
#
#   ### GEO-VALIDATION STEPS ###
#   ##1- Validating the coordinates at different levels - exact matchs
#   #1.1 Cases with original coordinates but without country, state or county information (cannot check)
#   herb.data1$geo.check[is.na(herb.data1$geo.check)&!is.na(herb.data1$latitude.work)&!is.na(herb.data1$pais)&is.na(herb.data1$loc.correct)] = "no_cannot_check"
#   #1.2 Country-level: good country? All countries
#   tmp = herb.data1[is.na(herb.data1$geo.check)&!is.na(herb.data1$latitude.gazetteer)&!is.na(herb.data1$latitude.work)&!is.na(herb.data1$country)&!is.na(herb.data1$pais),]
#   if(dim(tmp)[1]>0) {
#     tmp1 = as.factor(tmp$country != tmp$pais)
#     levels(tmp1) = c("ok_country","no_problem")
#     herb.data1$geo.check[is.na(herb.data1$geo.check)&!is.na(herb.data1$latitude.gazetteer)&!is.na(herb.data1$latitude.work)&!is.na(herb.data1$country)&!is.na(herb.data1$pais)] = as.character(tmp1)
#   } else { 	}
#   #1.3 State-level: good state? All countries
#   tmp = herb.data1[herb.data1$country %in% lista.paises & !is.na(herb.data1$latitude.gazetteer) & !is.na(herb.data1$latitude.work) & !is.na(herb.data1$state) & !is.na(herb.data1$estado),]
#   #tmp = herb.data1[herb.data1$country %in% c("argentina","brazil","paraguay")&!is.na(herb.data1$latitude.gazetteer)&!is.na(herb.data1$latitude.work)&!is.na(herb.data1$state)&!is.na(herb.data1$estado),]
#   if(dim(tmp)[1]>0) {
#     tmp1 = as.factor(tmp$state != tmp$estado)
#     levels(tmp1) = c("ok_state","no_problem")
#     herb.data1$geo.check[herb.data1$country %in% lista.paises & !is.na(herb.data1$latitude.gazetteer) & !is.na(herb.data1$latitude.work) & !is.na(herb.data1$state) & !is.na(herb.data1$estado)] = as.character(tmp1)
#   } else { 	}
#   #1.4 County-level. All countries
#   tmp = herb.data1[herb.data1$country %in% lista.paises & !is.na(herb.data1$latitude.gazetteer)&!is.na(herb.data1$latitude.work)&!is.na(herb.data1$county)&!is.na(herb.data1$municipio),]
#   #tmp = herb.data1[herb.data1$country %in% c("argentina","brazil","paraguay")&!is.na(herb.data1$latitude.gazetteer)&!is.na(herb.data1$latitude.work)&!is.na(herb.data1$county)&!is.na(herb.data1$municipio),]
#   if(dim(tmp)[1]>0) {
#     tmp1 = as.factor(tmp$county == tmp$municipio)
#     levels(tmp1) = c(NA,"ok_county"); tmp1 = as.character(tmp1)
#     tmp1[is.na(tmp1)] = as.character(tmp$geo.check[is.na(tmp1)])
#     herb.data1$geo.check[herb.data1$country %in% lista.paises & !is.na(herb.data1$latitude.gazetteer)&!is.na(herb.data1$latitude.work)&!is.na(herb.data1$county)&!is.na(herb.data1$municipio)] = tmp1
#   } else { 	}
#   #1.5 Calculating the distance between the original coordinates and the gazetter
#   herb.data1$dist_m = NA
#   tmp = herb.data1[herb.data1$geo.check%in%c("ok_country","ok_state","no_problem",NA)&!is.na(herb.data1$latitude.work)&!is.na(herb.data1$latitude.gazetteer),]
#   if(dim(tmp)[1]>0) {
#     if(dim(tmp)[1]>10000) {
#       #for big checks
#       no_cores = detectCores() - 1
#       n <- 10 #number of dividision to the input data frame
#       parts <- split(x = 1:dim(tmp)[1], f = cut(1:dim(tmp)[1], n))
#       cl <- makeCluster(no_cores)
#       clusterExport(cl, list("tmp","parts","n"),envir=environment())
#       clusterEvalQ(cl, library(geosphere))
#       overParts <- parLapply(cl = cl, X = 1:n, fun = function(x) {
#         tmp1.1 = tmp[parts[[x]],]
#         dist1 = rep(NA,dim(tmp1.1)[1])
#         for (w in 1:length(dist1)) {
#           dist1[w] = distm(c(tmp1.1$longitude.gazetteer[w],tmp1.1$latitude.gazetteer[w]),c(tmp1.1$longitude.work[w],tmp1.1$latitude.work[w]), fun = distHaversine)
#         }
#         tmp1.1$dist_m = dist1
#         gc() # release memory
#         return(tmp1.1)
#       })
#       stopImplicitCluster()
#       stopCluster(cl)
#       tmp2 = do.call(rbind.data.frame, overParts)
#       dist = tmp2$dist_m
#     } else {
#       #for small checks
#       dist = rep(NA,dim(tmp)[1])
#       for (w in 1:length(dist)) { dist[w] = distm(c(tmp$longitude.gazetteer[w],tmp$latitude.gazetteer[w]),c(tmp$longitude.work[w],tmp$latitude.work[w]), fun = distHaversine)	}
#     }
#     herb.data1$dist_m[herb.data1$geo.check%in%c("ok_country","ok_state","no_problem",NA)&!is.na(herb.data1$latitude.work)&!is.na(herb.data1$latitude.gazetteer)] = dist
#   } else { 	}
#   #1.6 Cases with original coordinates but probably with a projection/precision issue (e.g. coordinates falling into the sea or bays)
#   tmp = herb.data1[is.na(herb.data1$geo.check)&!is.na(herb.data1$country)&is.na(herb.data1$pais)&!is.na(herb.data1$dist_m)&herb.data1$origin.coord%in%"coord_original",]
#   if(dim(tmp)[1]>0) {
#     tmp2 = tmp$geo.check
#     tmp2[tmp$dist_m<40000 & tmp$resolution.coord%in%c("degrees_only")] = "ok_county_coord_corrected"
#     tmp2[tmp$dist_m<20000] = "ok_county_coord_corrected"
#     tmp2[is.na(tmp2)] = "no_problem_in_water"
#     herb.data1$geo.check[is.na(herb.data1$geo.check)&!is.na(herb.data1$country)&is.na(herb.data1$pais)&!is.na(herb.data1$dist_m)&herb.data1$origin.coord%in%"coord_original"] = as.character(tmp2)
#   } else { 	}
#   ##2- Verification of coordinate problems: inversions and swaps
#   #2.1 Finding original coordinates too far away from their
#   herb.data1$geo.check[herb.data1$geo.check%in%c("ok_country","ok_state")&herb.data1$dist_m>=100000&!is.na(herb.data1$county)&!is.na(herb.data1$dist_m)] = "no_problem_too_far"
#   herb.data1$geo.check[is.na(herb.data1$geo.check)&is.na(herb.data1$pais)&!is.na(herb.data1$latitude.work)] = "no_problem_in_water"
#   #2.2 Creating the data frame with all possible variations of coordinates
#   tmp = herb.data1[herb.data1$geo.check%in%c("no_problem","no_problem_in_water","no_problem_too_far")&!is.na(herb.data1$latitude.work)&!is.na(herb.data1$longitude.work),]
#   if(dim(tmp)[1]>0) {
#     tmp1 = tmp
#     tmp1[,"latitude.work"] = -tmp1[,"latitude.work"] #inverting latitude
#     tmp2 = tmp1
#     tmp1 = tmp; tmp1[,"longitude.work"] = -tmp1[,"longitude.work"] #inverting longitude
#     tmp2 = rbind.data.frame(tmp2,tmp1)
#     tmp1 = tmp; tmp1[,c("longitude.work","latitude.work")] = -tmp1[,c("longitude.work","latitude.work")] #inverting both coordinates
#     tmp2 = rbind.data.frame(tmp2,tmp1)
#     tmp1 = tmp; tmp1[,c("longitude.work","latitude.work")] = tmp1[,c("latitude.work","longitude.work")] #swapping coordinates
#     tmp2 = rbind.data.frame(tmp2,tmp1)
#     tmp2$type.problem = c(rep("invert.lat",dim(tmp)[1]),rep("invert.long",dim(tmp)[1]),rep("invert.both",dim(tmp)[1]),rep("swap",dim(tmp)[1]))
#     #2.3 Transforming points into spatial coordinates (points) and standardizing the projection
#     coordinates(tmp2) <- c("longitude.work", "latitude.work")  # set spatial coordinates
#     projection(tmp2) <- pj  # define projection system of our data
#     #2.4 Comparing the spatial data frame with the world country shapefile and creating the new locality columns
#     tmp0.1 = over(tmp2, wo1, fn=NULL) ##PARALELLIZE according to the codes above (paralelliz only for big checkings!)
#     tmp3 = cbind.data.frame(tmp2,pais1=tmp0.1$pais)
#     tmp3$pais1 = as.character(tmp3$pais1)
#     tmp3 = tmp3[!is.na(tmp3$pais1),]
#     if(dim(tmp3)[1]>0) {
#       tmp3$estado1 = NA
#       tmp3$municipio1 = NA
#       tmp3$geo.check1 = NA
#       #2.4.1 Saving the new country information and cleaning the cases that remained as problems (country=NA)
#       tmp3$geo.check1[tmp3$country==tmp3$pais1&!is.na(tmp3$country)&!is.na(tmp3$pais1)] =
#         paste("ok_country",as.character(tmp3$type.problem[tmp3$country==tmp3$pais1&!is.na(tmp3$country)&!is.na(tmp3$pais1)]),sep="_")
#       tmp3 = tmp3[grepl("ok_country",tmp3$geo.check1),]
#       #2.5 Comparing the spatial data frame with the selected country shapefiles
#       tmp4 = tmp3[tmp3$pais1 %in% c("argentina","brazil","paraguay"),]
#       if(dim(tmp4)[1]>0) {
#         coordinates(tmp4) <- c("longitude.work", "latitude.work")  # set spatial coordinates
#         proj4string(tmp4) <- pj1  # define projection system of our data
#         n1 = rep(NA,dim(tmp4@data)[1])
#         n2 = rep(NA,dim(tmp4@data)[1])
#         for (j in 1:length(countries)) {
#           tmp1 = country.list[[j]]
#           tmp2 = over(tmp4, tmp1, fn=NULL)
#           n1[is.na(n1)&!is.na(as.character(tmp2$NAME_1))] = as.character(tmp2$NAME_1)[is.na(n1)&!is.na(as.character(tmp2$NAME_1))]
#           n2[is.na(n2)&!is.na(as.character(tmp2$NAME_2))] = as.character(tmp2$NAME_2)[is.na(n2)&!is.na(as.character(tmp2$NAME_2))]
#         }
#         #2.5.1 Saving the new state and county info retrieved and celanig the cases that remained as problems (county=NA)
#         tmp4@data$estado1 = n1
#         tmp4@data$municipio1 = n2
#         tmp4 = tmp4@data[!is.na(tmp4$estado1),]
#         #2.5.1 Updating the geographical coordinate validation classes
#         tmp4$geo.check1[tmp4$county==tmp4$municipio1&tmp4$state==tmp4$estado1&!is.na(tmp4$county)&!is.na(tmp4$municipio1)] =
#           paste("ok_county",as.character(tmp4$type.problem[tmp4$county==tmp4$municipio1&tmp4$state==tmp4$estado1&!is.na(tmp4$county)&!is.na(tmp4$municipio1)]),sep="_")
#         tmp4$geo.check1[tmp4$county!=tmp4$municipio1&tmp4$state==tmp4$estado1&!is.na(tmp4$county)&!is.na(tmp4$municipio1)] =
#           paste("ok_county",as.character(tmp4$type.problem[tmp4$county!=tmp4$municipio1&tmp4$state==tmp4$estado1&!is.na(tmp4$county)&!is.na(tmp4$municipio1)]),sep="_")
#         tmp4$geo.check1[tmp4$county!=tmp4$municipio1&tmp4$state!=tmp4$estado1&!is.na(tmp4$county)&!is.na(tmp4$municipio1)] =
#           paste("ok_county",as.character(tmp4$type.problem[tmp4$county!=tmp4$municipio1&tmp4$state!=tmp4$estado1&!is.na(tmp4$county)&!is.na(tmp4$municipio1)]),sep="_")
#         #2.6 Merging the results
#         tmp3[tmp3$pais1 %in% c("argentina","brazil","paraguay"),c("estado1","municipio1","geo.check1")] = tmp4[,c("estado1","municipio1","geo.check1")]
#         #2.7 Recalculating dist_m for the the new coordinates
#         dist = rep(NA,dim(tmp3)[1])
#         for (w in 1:length(dist)) { dist[w] = distm(c(tmp3$longitude.gazetteer[w],tmp3$latitude.gazetteer[w]),c(tmp3$longitude.work[w],tmp3$latitude.work[w]), fun = distHaversine)	}
#         tmp3$dist_m1 = dist
#       } else {
#         if(dim(tmp3)[1]>0) {
#           tmp3$dist_m1 = NA
#           #2.8 Saving the results
#           tmp5 = left_join(herb.data1,tmp3[,c("order","latitude.work","longitude.work","geo.check1","pais1","estado1","municipio1","dist_m1")],by="order")
#           herb.data1[!is.na(tmp5$pais1),c("latitude.work","longitude.work","geo.check","pais","estado","municipio","dist_m")] =
#             tmp5[!is.na(tmp5$pais1),c("latitude.work.y","longitude.work.y","geo.check1","pais1","estado1","municipio1","dist_m1")]
#         } else { }
#       }
#     } else { 	}
#   } else { 	}
#   ##3- Final decisions and classification of particular cases based on coordinates resolution and distance to the reference/gazetteer coordinate
#   #3.1 Cases with original coordinates but probably low precision (seconds not explicitly defined)
#   #max. distance due to minutes rounding = 2.624km (equator) to 2.199km (latitude 50)
#   #max. distance due to degrees rounding = 78.714km (equator) to 66.065km (latitude 50)
#   #quantiles of the distribution od dist_m for "ok_county"
#   #tmp = herb.data1[herb.data1$geo.check %in% c("ok_county") & herb.data1$state%in%"bahia",]
#   #dist = rep(NA,dim(tmp)[1])
#   #for (w in 1:length(dist)) { dist[w] = distm(c(tmp$longitude.gazetteer[w],tmp$latitude.gazetteer[w]),c(tmp$longitude.work[w],tmp$latitude.work[w]), fun = distHaversine)	}
#   #quantile(dist)
#
#   #for Bahia (large county-size AF state - max. limits), up to:
#   #2.5km (same), 5km (very close), 10km (close), 20km (not_too_far), 40km (far), >100km (problem)
#   tmp = herb.data1[herb.data1$geo.check%in%"ok_state"&!is.na(herb.data1$dist_m)&!is.na(herb.data1$county)&!is.na(herb.data1$municipio),]
#   if(dim(tmp)[1]>0) {
#     tmp$geo.check[tmp$dist_m<2500 & tmp$resolution.coord%in%c("seconds","seconds?")] = "ok_county_very_close"
#     tmp$geo.check[tmp$dist_m<5000 & tmp$resolution.coord%in%c("minutes_only","degrees_only")] = "ok_county_very_close"
#     tmp$geo.check[tmp$dist_m<5000 & tmp$resolution.coord%in%c("seconds","seconds?") & tmp$geo.check%in%"ok_state"] = "ok_county_close"
#     tmp$geo.check[tmp$dist_m<10000 & tmp$resolution.coord%in%c("minutes_only","degrees_only") & tmp$geo.check%in%"ok_state"] = "ok_county_close"
#     tmp$geo.check[tmp$dist_m<10000 & tmp$resolution.coord%in%c("seconds","seconds?") & tmp$geo.check%in%"ok_state"] = "ok_county_not_too_far"
#     tmp$geo.check[tmp$dist_m<20000 & tmp$resolution.coord%in%c("minutes_only","degrees_only") & tmp$geo.check%in%"ok_state"] = "ok_county_not_too_far"
#     tmp$geo.check[tmp$dist_m>=10000 & tmp$dist_m<20000 & tmp$resolution.coord%in%c("seconds","seconds?") & tmp$geo.check%in%"ok_state"] = "ok_county_name_mistaken?"
#     tmp$geo.check[tmp$dist_m>=20000 & tmp$dist_m<100000 & tmp$resolution.coord%in%c("seconds","seconds?") & tmp$geo.check%in%"ok_state"] = "no_too_far"
#     tmp$geo.check[tmp$dist_m>=40000 & tmp$dist_m<100000 & tmp$resolution.coord%in%c("minutes_only","degrees_only") & tmp$geo.check%in%"ok_state"] = "no_too_far"
#     herb.data1$geo.check[herb.data1$geo.check%in%"ok_state"&!is.na(herb.data1$dist_m)&!is.na(herb.data1$county)&!is.na(herb.data1$municipio)] = tmp$geo.check
#   } else { 	}
#   ##4 - Replacing the original coordinates by the gazetter coordinates for selected verification classes
#   #4.1 Coordinates falling into the sea ("ok_county_coord_corrected")
#   herb.data1[herb.data1$geo.check%in%"ok_county_coord_corrected",c("latitude.work","longitude.work")] =
#     herb.data1[herb.data1$geo.check%in%"ok_county_coord_corrected",c("latitude.gazetteer","longitude.gazetteer")]
#   herb.data1$origin.coord[herb.data1$geo.check%in%"ok_county_coord_corrected"] = "correction_coord_original"
#   herb.data1$resolution.coord[herb.data1$geo.check%in%"ok_county_coord_corrected"] = "seconds_centroid"
#   #4.2 Problematic coordinates ("problem","problem_in_water","problem_too_far")
#   herb.data1[grepl("no_problem",herb.data1$geo.check)&!is.na(herb.data1$country),c("latitude.work","longitude.work")] =
#     herb.data1[grepl("no_problem",herb.data1$geo.check)&!is.na(herb.data1$country),c("latitude.gazetteer","longitude.gazetteer")]
#   herb.data1$origin.coord[grepl("no_problem",herb.data1$geo.check)&!is.na(herb.data1$country)] = "correction_coord_original"
#   herb.data1$resolution.coord[grepl("no_problem",herb.data1$geo.check)&!is.na(herb.data1$country)] = "seconds_centroid"
#   herb.data1$geo.check[grepl("no_problem",herb.data1$geo.check)&!is.na(herb.data1$country)] = "ok_county_problem_coord_corrected"
#   #4.3 Original coodinates between 40-100km of the centroid of the county ("ok_county_far")
#   #Correct only in the case of coordinates at "degrees_only" resolution
#   herb.data1[herb.data1$geo.check%in%"no_too_far"&!is.na(herb.data1$county)&herb.data1$resolution.coord%in%"degrees_only",c("latitude.work","longitude.work")] =
#     herb.data1[herb.data1$geo.check%in%"no_too_far"&!is.na(herb.data1$county)&herb.data1$resolution.coord%in%"degrees_only",c("latitude.gazetteer","longitude.gazetteer")]
#   ids =  herb.data1$order[herb.data1$geo.check%in%"no_too_far"&!is.na(herb.data1$county)&herb.data1$resolution.coord%in%"degrees_only"]
#   herb.data1$geo.check[herb.data1$order%in%ids] = "ok_county_coord_corrected"
#   herb.data1$origin.coord[herb.data1$order%in%ids] = "correction_coord_original"
#   herb.data1$resolution.coord[herb.data1$order%in%ids] = "seconds_centroid"
#   #4.4 Coordinates with county info but with problematic original corodinates ("ok_state")
#   #herb.data1[herb.data1$geo.check%in%"ok_state" & herb.data1$origin.coord%in%"coord_gazet_state",c("latitude.work","longitude.work")] =
#   #	herb.data1[herb.data1$geo.check%in%"ok_state" & herb.data1$origin.coord%in%"coord_gazet_state",c("latitude.gazetteer","longitude.gazetteer")]
#   #Nothing to correct; but the gazetteer can include more variants and localities to avoid this problems
#
#   ### SAVING THE VALIDATION RESULTS ###
#   #Inpecting if the order of both objects match
#   #table(herb.data$order == herb.data1$order)
#   #Updating the existing columns
#   herb.data[,c("latitude.work","longitude.work","origin.coord","resolution.coord")] = herb.data1[,c("latitude.work","longitude.work","origin.coord","resolution.coord")]
#   #Saving the new columns of the geo-validation process and assigning the classes/precision to the coordinates from the gazetteer
#   herb.data$geo.check = herb.data1$geo.check
#   herb.data$resolution.coord[herb.data$origin.coord%in%"ok_country_gazet"] = "seconds_centroid"
#   herb.data$resolution.coord[herb.data$origin.coord%in%"ok_state_gazet"] = "seconds_centroid"
#   herb.data$resolution.coord[herb.data$origin.coord%in%"ok_county_gazet"] = "seconds_centroid"
#   herb.data$resolution.coord[herb.data$origin.coord%in%"ok_locality_gazet"] = "seconds_point"
#   #table(herb.data$geo.check,herb.data$origin.coord)
#   #Saving the new columns with the location and situation from the geographical coordinates
#   dic1 = dic[!duplicated(dic$loc)&!is.na(dic$loc.correct) & !dic$resolution.gazetteer%in%c("localidade","localidade|sublocalidade","distrito|vila","distrito|bairro","cachoeira","mina","vila","serra"),]
#   loc = herb.data1[,c("order","loc.coord")]
#   names(loc)[2] = "loc"
#   if(any(!is.na(loc$loc))) {
#     tmp = left_join(loc,dic1[,c("loc","loc.correct","AtlanticForest_Lei11428")],by="loc")
#     #table(tmp$order==herb.data$order)
#     herb.data$loc.coord = tmp$loc.correct
#     herb.data$AtlanticForest_Lei11428.coord = tmp$AtlanticForest_Lei11428
#   } else {
#     herb.data$loc.coord = NA
#     herb.data$AtlanticForest_Lei11428.coord = NA
#   }
#   #Creating the new file path and saving in the local folder
#   path.csv = gsub("coord-tax.csv","coord-tax-geo.csv",myfiles0)
#   write.csv(x=herb.data,file=path.csv, row.names = FALSE)
#   cat(i,"\n")
# }
