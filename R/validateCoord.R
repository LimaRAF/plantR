#' @title Validate Geographical Coordinates
#'
#' @description This function...
#'
#' @param x Data.frame with coordinates in decimal degrees.
#' @param parallel Logical. Parallelize or not.
#'
#' @importFrom dplyr select one_of rename mutate if_else
#' @importFrom tidyr separate
#' @importFrom sp coordinates
#' @importFrom sf st_crs st_as_sf st_join st_intersects st_set_crs
#x <- occs
#no need to use the gazetteer
validateCoord <- function(x) {
  # ##Getting the file paths for the herbarium data
  #   ### PRE-VALIDATION STEPS ###
  #   ##Loading the occurrence data
  #   ##Removing unwanted columns


  cols <- c("loc.correct",
            "latitude.gazetteer",
            "longitude.gazetteer",
            "origin.coord",
            "resolution.coord",
            "decimalLatitude.new",
            "decimalLongitude.new")
  #cls <-   unique(cols[cols %in% names(x)])
  #x1 <- x [,cls]
  x1 <- dplyr::select(x, one_of(cols))
  #x1$order <- 1:nrow(x1) #putting the data back on its original order

#   ##Defining the country, state and county columns
   x1 <- tidyr::separate(data = x1,
                         col = loc.correct,
                         sep = "_",
                         into = c("country", "state", "county", "locality", "sublocality"),
                          remove = FALSE)

   ##Creating the geo.check columns
   x1$geo.check <- NA
   x1$geo.check[x1$origin.coord %in% c("coord_original")] <-  "coord_original"
   x1$geo.check[x1$origin.coord %in% c("coord_gazet")] <-  "coord_gazet"
   x1$geo.check[x1$origin.coord %in% "no_coord"] <- "no_coord"
   x1$geo.check[is.na(x1$decimalLatitude.new) & is.na(x1$latitude.gazetteer)] <- "no_coord"

   #tmp0 <- x1[!x1$origin.coord %in% "coord_original", ]
   #tmp <- x1[x1$origin.coord %in% "coord_original", ]
   tmp <- x1
   tmp <- tmp[!is.na(tmp$decimalLongitude.new),]
   tmp <- tmp[!is.na(tmp$decimalLatitude.new),]

   ##Getting data frame with spatial coordinates (points) and standardizing the projection
   sp::coordinates(tmp) <- c("decimalLongitude.new", "decimalLatitude.new")  # set spatial coordinates
   tmp <- sf::st_as_sf(tmp, remove = FALSE)

   #@ast: assumindo que tudo é WGS84 msa isto tem que ser revisto desde prepCoord
   prj <- st_crs(worldMap)
   tmp <- st_set_crs(tmp, prj)  # define projection system of our data
   over_res <- st_join(tmp, worldMap, join = st_intersects)
   #names(over_res) #este objeto não tem as colunas decimalLatitude/longitude.new porque são as coordinates
   over_res <- rename(over_res, pais = NAME_0) #country from world - the output is already in the desired format <3

   ##Comparing the spatial data frame with the selected country shapefiles
   latam_all <- bind_rows(latamMap) #there is no need to keep them apart actually
   x2 <- st_join(over_res, latam_all, join = st_intersects)

   #ambos vetores de paises sao praticamente iguais mas a melhor resolução permite que alguns NA no mundo sejam países em latam
   x2 <- rename(x2, pais_latam = NAME_0, estado = NAME_1, municipio = NAME_2)
   #checa diferencas paises e preenche com latam se faltar no mundo
   x2 <- mutate(x2, pais = if_else(is.na(pais) & !is.na(pais_latam), pais_latam, pais))
   #cria o vetor para checar
   x2$loc.coord <- paste(x2$pais, x2$estado, x2$municipio, sep = "_")
   x2$loc.coord[x2$loc.coord %in% "NA_NA_NA"] <- NA
   # recupera todas as linhas
   x3 <- left_join(x1, x2) #it works now :shrugs:
   #st_coordinates(x2)#x3 is a dataframe, it does not have coordinates anymore
   names(x3) #her şey güzel

   ### GEO-VALIDATION STEPS ###
   ##1- Validating the coordinates at different levels - exact matchs
   #1.1 Cases with original coordinates but without country, state or county information (cannot check)
   count(x3, geo.check)
   x3$cannot.check[is.na(x3$geo.check) & !is.na(x3$decimalLatitude.new) & !is.na(x3$pais) & is.na(x3$loc.correct)] <- "cannot_check"
   #there are no NAs so this does nothing
   count(x3,cannot.check, geo.check) #6, 76, 16460
   #   #1.2 Country-level: good country? All countries

   x3 <- dplyr::mutate(x3, country.check = if_else(#is.na(x3$geo.check) &               #not evaluated yet - this could be commented too perhaps but I keep the unevaluated just in case
                                      #!is.na(x3$latitude.gazetteer) &   # with gazetteer data - can be commented
                                      #!is.na(x3$decimalLatitude.new) &  # with obtained from gazetteer - can be commented
                                      #!is.na(x3$country) &              # with original country data
                                      #!is.na(x3$pais) &                 # with gazetter country data
                                      x3$country == x3$pais,          # with or without differences between country and pais (from gazetteer)
                                    "country_ok", "country_bad")) #if OK or not
   count(x3, geo.check, country.check) #6, country_ok, no_coord 76, NA 328

   #1.3 State-level: good state? All countries
   x3 <- dplyr::mutate(x3, state.check = if_else(
     x3$state == x3$estado,          # with or without differences between state and estado (from gazetteer)
     "estado_ok", "estado_bad")) #if OK or not
   count(x3, state.check)
   count(x3, geo.check, country.check, state.check) #6, country_ok, no_coord 76, NA 328
#   #1.4 County-level. All countries
   x3 <- dplyr::mutate(x3, county.check = if_else(
     x3$county == x3$municipio,          # with or without differences between county and mpo (from gazetteer)
     "county_ok", "county_bad")) #if OK or not
   count(x3, county.check) #6, country_ok, no_coord 76, NA 328
   count(x3, geo.check, country.check, state.check, county.check) #6, country_ok, no_coord 76, NA 328
}
######fiquei aqui


#longitude.gazetteer latitude.gazetter e new
#   #1.5 Calculating the distance between the original coordinates and the gazetter
#           dist1[w] = distm(c(tmp1.1$longitude.gazetteer[w],tmp1.1$latitude.gazetteer[w]),c(tmp1.1$decimalLongitude.new[w],tmp1.1$decimalLatitude.new[w]), fun = distHaversine)
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
#       for (w in 1:length(dist)) { dist[w] = distm(c(tmp$longitude.gazetteer[w],tmp$latitude.gazetteer[w]),c(tmp$decimalLongitude.new[w],tmp$decimalLatitude.new[w]), fun = distHaversine)	}
#     }
#     x1$dist_m[x1$geo.check%in%c("ok_country","ok_state","no_problem",NA)&!is.na(x1$decimalLatitude.new)&!is.na(x1$latitude.gazetteer)] = dist
#   } else { 	}
#   #1.6 Cases with original coordinates but probably with a projection/precision issue (e.g. coordinates falling into the sea or bays)
#   tmp = x1[is.na(x1$geo.check)&!is.na(x1$country)&is.na(x1$pais)&!is.na(x1$dist_m)&x1$origin.coord%in%"coord_original",]
#   if(dim(tmp)[1]>0) {
#     tmp2 = tmp$geo.check
#     tmp2[tmp$dist_m<40000 & tmp$resolution.coord%in%c("degrees_only")] = "ok_county_coord_corrected"
#     tmp2[tmp$dist_m<20000] = "ok_county_coord_corrected"
#     tmp2[is.na(tmp2)] = "no_problem_in_water"
#     x1$geo.check[is.na(x1$geo.check)&!is.na(x1$country)&is.na(x1$pais)&!is.na(x1$dist_m)&x1$origin.coord%in%"coord_original"] = as.character(tmp2)
#   } else { 	}
#   ##2- Verification of coordinate problems: inversions and swaps
#   #2.1 Finding original coordinates too far away from their
#   x1$geo.check[x1$geo.check%in%c("ok_country","ok_state")&x1$dist_m>=100000&!is.na(x1$county)&!is.na(x1$dist_m)] = "no_problem_too_far"
#   x1$geo.check[is.na(x1$geo.check)&is.na(x1$pais)&!is.na(x1$decimalLatitude.new)] = "no_problem_in_water"
#   #2.2 Creating the data frame with all possible variations of coordinates
#   tmp = x1[x1$geo.check%in%c("no_problem","no_problem_in_water","no_problem_too_far")&!is.na(x1$decimalLatitude.new)&!is.na(x1$decimalLongitude.new),]
#   if(dim(tmp)[1]>0) {
#     tmp1 = tmp
#     tmp1[,"decimalLatitude.new"] = -tmp1[,"decimalLatitude.new"] #inverting latitude
#     tmp2 = tmp1
#     tmp1 = tmp; tmp1[,"decimalLongitude.new"] = -tmp1[,"decimalLongitude.new"] #inverting longitude
#     tmp2 = rbind.data.frame(tmp2,tmp1)
#     tmp1 = tmp; tmp1[,c("decimalLongitude.new","decimalLatitude.new")] = -tmp1[,c("decimalLongitude.new","decimalLatitude.new")] #inverting both coordinates
#     tmp2 = rbind.data.frame(tmp2,tmp1)
#     tmp1 = tmp; tmp1[,c("decimalLongitude.new","decimalLatitude.new")] = tmp1[,c("decimalLatitude.new","decimalLongitude.new")] #swapping coordinates
#     tmp2 = rbind.data.frame(tmp2,tmp1)
#     tmp2$type.problem = c(rep("invert.lat",dim(tmp)[1]),rep("invert.long",dim(tmp)[1]),rep("invert.both",dim(tmp)[1]),rep("swap",dim(tmp)[1]))
#     #2.3 Transforming points into spatial coordinates (points) and standardizing the projection
#     coordinates(tmp2) <- c("decimalLongitude.new", "decimalLatitude.new")  # set spatial coordinates
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
#         coordinates(tmp4) <- c("decimalLongitude.new", "decimalLatitude.new")  # set spatial coordinates
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
#         for (w in 1:length(dist)) { dist[w] = distm(c(tmp3$longitude.gazetteer[w],tmp3$latitude.gazetteer[w]),c(tmp3$decimalLongitude.new[w],tmp3$decimalLatitude.new[w]), fun = distHaversine)	}
#         tmp3$dist_m1 = dist
#       } else {
#         if(dim(tmp3)[1]>0) {
#           tmp3$dist_m1 = NA
#           #2.8 Saving the results
#           tmp5 = left_join(x1,tmp3[,c("order","decimalLatitude.new","decimalLongitude.new","geo.check1","pais1","estado1","municipio1","dist_m1")],by="order")
#           x1[!is.na(tmp5$pais1),c("decimalLatitude.new","decimalLongitude.new","geo.check","pais","estado","municipio","dist_m")] =
#             tmp5[!is.na(tmp5$pais1),c("decimalLatitude.new.y","decimalLongitude.new.y","geo.check1","pais1","estado1","municipio1","dist_m1")]
#         } else { }
#       }
#     } else { 	}
#   } else { 	}
#   ##3- Final decisions and classification of particular cases based on coordinates resolution and distance to the reference/gazetteer coordinate
#   #3.1 Cases with original coordinates but probably low precision (seconds not explicitly defined)
#   #max. distance due to minutes rounding = 2.624km (equator) to 2.199km (latitude 50)
#   #max. distance due to degrees rounding = 78.714km (equator) to 66.065km (latitude 50)
#   #quantiles of the distribution od dist_m for "ok_county"
#   #tmp = x1[x1$geo.check %in% c("ok_county") & x1$state%in%"bahia",]
#   #dist = rep(NA,dim(tmp)[1])
#   #for (w in 1:length(dist)) { dist[w] = distm(c(tmp$longitude.gazetteer[w],tmp$latitude.gazetteer[w]),c(tmp$decimalLongitude.new[w],tmp$decimalLatitude.new[w]), fun = distHaversine)	}
#   #quantile(dist)
#
#   #for Bahia (large county-size AF state - max. limits), up to:
#   #2.5km (same), 5km (very close), 10km (close), 20km (not_too_far), 40km (far), >100km (problem)
#   tmp = x1[x1$geo.check%in%"ok_state"&!is.na(x1$dist_m)&!is.na(x1$county)&!is.na(x1$municipio),]
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
#     x1$geo.check[x1$geo.check%in%"ok_state"&!is.na(x1$dist_m)&!is.na(x1$county)&!is.na(x1$municipio)] = tmp$geo.check
#   } else { 	}
#   ##4 - Replacing the original coordinates by the gazetter coordinates for selected verification classes
#   #4.1 Coordinates falling into the sea ("ok_county_coord_corrected")
#   x1[x1$geo.check%in%"ok_county_coord_corrected",c("decimalLatitude.new","decimalLongitude.new")] =
#     x1[x1$geo.check%in%"ok_county_coord_corrected",c("latitude.gazetteer","longitude.gazetteer")]
#   x1$origin.coord[x1$geo.check%in%"ok_county_coord_corrected"] = "correction_coord_original"
#   x1$resolution.coord[x1$geo.check%in%"ok_county_coord_corrected"] = "seconds_centroid"
#   #4.2 Problematic coordinates ("problem","problem_in_water","problem_too_far")
#   x1[grepl("no_problem",x1$geo.check)&!is.na(x1$country),c("decimalLatitude.new","decimalLongitude.new")] =
#     x1[grepl("no_problem",x1$geo.check)&!is.na(x1$country),c("latitude.gazetteer","longitude.gazetteer")]
#   x1$origin.coord[grepl("no_problem",x1$geo.check)&!is.na(x1$country)] = "correction_coord_original"
#   x1$resolution.coord[grepl("no_problem",x1$geo.check)&!is.na(x1$country)] = "seconds_centroid"
#   x1$geo.check[grepl("no_problem",x1$geo.check)&!is.na(x1$country)] = "ok_county_problem_coord_corrected"
#   #4.3 Original coodinates between 40-100km of the centroid of the county ("ok_county_far")
#   #Correct only in the case of coordinates at "degrees_only" resolution
#   x1[x1$geo.check%in%"no_too_far"&!is.na(x1$county)&x1$resolution.coord%in%"degrees_only",c("decimalLatitude.new","decimalLongitude.new")] =
#     x1[x1$geo.check%in%"no_too_far"&!is.na(x1$county)&x1$resolution.coord%in%"degrees_only",c("latitude.gazetteer","longitude.gazetteer")]
#   ids =  x1$order[x1$geo.check%in%"no_too_far"&!is.na(x1$county)&x1$resolution.coord%in%"degrees_only"]
#   x1$geo.check[x1$order%in%ids] = "ok_county_coord_corrected"
#   x1$origin.coord[x1$order%in%ids] = "correction_coord_original"
#   x1$resolution.coord[x1$order%in%ids] = "seconds_centroid"
#   #4.4 Coordinates with county info but with problematic original corodinates ("ok_state")
#   #x1[x1$geo.check%in%"ok_state" & x1$origin.coord%in%"coord_gazet_state",c("decimalLatitude.new","decimalLongitude.new")] =
#   #	x1[x1$geo.check%in%"ok_state" & x1$origin.coord%in%"coord_gazet_state",c("latitude.gazetteer","longitude.gazetteer")]
#   #Nothing to correct; but the gazetteer can include more variants and localities to avoid this problems
#
#   ### SAVING THE VALIDATION RESULTS ###
#   #Inpecting if the order of both objects match
#   #table(x$order == x1$order)
#   #Updating the existing columns
#   x[,c("decimalLatitude.new","decimalLongitude.new","origin.coord","resolution.coord")] = x1[,c("decimalLatitude.new","decimalLongitude.new","origin.coord","resolution.coord")]
#   #Saving the new columns of the geo-validation process and assigning the classes/precision to the coordinates from the gazetteer
#   x$geo.check = x1$geo.check
#   x$resolution.coord[x$origin.coord%in%"ok_country_gazet"] = "seconds_centroid"
#   x$resolution.coord[x$origin.coord%in%"ok_state_gazet"] = "seconds_centroid"
#   x$resolution.coord[x$origin.coord%in%"ok_county_gazet"] = "seconds_centroid"
#   x$resolution.coord[x$origin.coord%in%"ok_locality_gazet"] = "seconds_point"
#   #table(x$geo.check,x$origin.coord)
#   #Saving the new columns with the location and situation from the geographical coordinates
#   dic1 = dic[!duplicated(dic$loc)&!is.na(dic$loc.correct) & !dic$resolution.gazetteer%in%c("localidade","localidade|sublocalidade","distrito|vila","distrito|bairro","cachoeira","mina","vila","serra"),]
#   loc = x1[,c("order","loc.coord")]
#   names(loc)[2] = "loc"
#   if(any(!is.na(loc$loc))) {
#     tmp = left_join(loc,dic1[,c("loc","loc.correct","AtlanticForest_Lei11428")],by="loc")
#     #table(tmp$order==x$order)
#     x$loc.coord = tmp$loc.correct
#     x$AtlanticForest_Lei11428.coord = tmp$AtlanticForest_Lei11428
#   } else {
#     x$loc.coord = NA
#     x$AtlanticForest_Lei11428.coord = NA
#   }
#   #Creating the new file path and saving in the local folder
#   path.csv = gsub("coord-tax.csv","coord-tax-geo.csv",myfiles0)
#   write.csv(x = x,file = path.csv, row.names = FALSE)
#   cat(i,"\n")
# }
#
# ##Studying possible distance due to coordinate precision
# #Degrees: maximum distance of 157km (equator) to 132km (at 50latS)
# distm(c(-39,-50),c(-40,-51), fun = distHaversine)
# distm(c(-39,0),c(-40,-1), fun = distHaversine)
# #Minutes: maximum distance of 2.62km (equator) to 2.21km (at 50latS)
# distm(c(-40.01666667,0.00000),c(-40.00,-0.01666667), fun = distHaversine)
# distm(c(-40.01666667,-50.00000),c(-40.00,-50.01666667), fun = distHaversine)
# #Seconds
# distm(c(-40.0002777778,0.00000),c(-40.00,-0.0002777778), fun = distHaversine)
# distm(c(-40.0002777778,-50.00000),c(-40.00,-50.0002777778), fun = distHaversine)
#

#  }
