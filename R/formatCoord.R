#' @title Format Geographical Coordinates
#'
#' @description This function...
#'
#' @param x data.frame
#'
formatCoord <- function(x) {
  loc = x
  return(loc)
}
#### 1.1 Editing coordinates data and defining the working coordinates  - from speciesLink, speciesLink_new and JABOT ####
##########################################################################################################################
# #Path to the edited files from speciesLink and JABOT
# paths = c(dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink",full.names=TRUE),
#           dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink_new",full.names=TRUE),
#           dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//jabot",full.names=TRUE))
# paths = list.files(paths, full.names = TRUE)
# paths = paths[grep("edited-cleaned-loc.csv",paths)]
#
# #Looping for each collection
# #tmp = vector("list",length(paths))
# for(i in 1:length(paths)) {
#   myfiles0 = paths[i]
#   herb.data = read.csv(myfiles0,as.is=TRUE,na.strings=c(""," ","NA"))
#   if(dim(herb.data)[1]==0) next
#   ##Removing unwanted columns and re-naming
#   cols = c("decimalLatitude","decimalLongitude", 		#speciesLink
#            #"verbatimLatitude","verbatimLongitude", 	    #speciesLink
#            "latitude","longitude",					              #speciesLink_new + JABOT
#            "decimallatitude","decimallongitude")		      #GBIF
#   cls = unique(cols[cols %in% names(herb.data)])
#   herb.data1 = herb.data[,cls]
#   ##Obtaining the columns with coordinates
#   lat = herb.data1[,1]; long = herb.data1[,2]
#   ##Preliminary editing
#   #coordinates without numbers
#   lat[!grepl('\\d',lat)|!grepl('\\d',long)] = NA
#   long[!grepl('\\d',lat)|!grepl('\\d',long)] = NA
#   #zero coordinates as missing
#   lat[lat%in%0 & long%in%0] = NA
#   long[(is.na(lat)|lat%in%0) & long%in%0] = NA
#   #one of the coordinates missing as missing
#   lat[lat%in%0 & is.na(long)] = NA; long[long%in%0 & is.na(lat)] = NA
#   #blocked coordinates
#   lat[lat %in% c("bloqueada","Bloqueada")] = NA
#   long[long %in% c("bloqueada","Bloqueada")] = NA
#   #possible problems with decimal division
#   lat = gsub(',','\\.',lat); long = gsub(',','\\.',long)
#   ##Any coordinate on non-decimal degrees format?
#   toto = sum(table(lat)) - sum(table(as.double(lat)))
#   if(toto>0) {
#     lat.ref = grepl("s|S",lat)&!grepl("n|N",lat); long.ref = grepl("w|W|o|O",long)&!grepl("e|E|l|L",long)
#     lat = gsub('\'|\"|?|?|\\*|\\|'," ",lat); long = gsub('\'|\"|?|?|\\*|\\|'," ",long)
#     lat = gsub('[a-z]'," ",lat, ignore.case=T); long = gsub('[a-z]'," ",long, ignore.case=T)
#     lat = gsub('   ',' ',lat); long = gsub('   ',' ',long)
#     lat = gsub('  ',' ',lat); long = gsub('  ',' ',long)
#     lat = str_trim(lat); long = str_trim(long)
#     lat[!is.na(lat)&lat%in%c("0 0 0","0 0")] = NA; long[!is.na(long)&long%in%c("0 0 0","0 0")] = NA
#     lat[!is.na(lat)&lat%in%""] = NA; long[!is.na(long)&long%in%""] = NA
#     #unique(lat)
#     lat1 = cbind.data.frame(grau = as.double(sapply(strsplit(lat," "),function(x)x[1])),
#                             min = as.double(sapply(strsplit(lat," "),function(x)x[2]))/60,
#                             sec = as.double(sapply(strsplit(lat," "),function(x)x[3]))/3600)
#     lat2 = round(apply(lat1,1,sum,na.rm=TRUE),8)
#     long1 = cbind.data.frame(grau = as.double(sapply(strsplit(long," "),function(x)x[1])),
#                              min = as.double(sapply(strsplit(long," "),function(x)x[2]))/60,
#                              sec = as.double(sapply(strsplit(long," "),function(x)x[3]))/3600)
#     long2 = round(apply(long1,1,sum,na.rm=TRUE),8)
#     lat2[lat.ref] = -lat2[lat.ref]; long2[long.ref] = -long2[long.ref]
#     lat2[!is.na(lat2)&lat2==0] = NA; long2[!is.na(long2)&long2==0] = NA
#   } else {
#     lat2 = lat
#     long2 = long
#   }
#   ##Creating the working coodinates and defining the origin of the information
#   if(all(is.na(lat))|all(is.na(long))) {
#     herb.data$latitude.work = NA
#     herb.data$longitude.work = NA
#     herb.data$origin.coord = NA
#     herb.data$resolution.coord = NA
#   }	else {
#     herb.data$latitude.work = lat2
#     herb.data$longitude.work = long2
#     herb.data$origin.coord = NA
#     herb.data$origin.coord[!is.na(herb.data$latitude.work)] = "coord_original"
#     ##Assessing the resolution of the original coordinates
#     herb.data$resolution.coord = NA
#     lat3 = herb.data$latitude.work[!is.na(herb.data$latitude.work)]
#     long3 = herb.data$latitude.work[!is.na(herb.data$latitude.work)]
#     #No minutes
#     tmp1 = strsplit(lat3,"\\.")
#     lat3[sapply(tmp1,length)==1] = "degrees_only"
#     #if(sapply(tmp1,length)==1) lat3 = "degrees_only"
#     tmp2 = strsplit(long3,"\\.")
#     long3[sapply(tmp2,length)==1] = "degrees_only"
#     #if(sapply(tmp2,length)==1) long3 = "degrees_only"
#
#     #No seconds
#     mins= sapply(strsplit(as.character(round(1:59/60,6)),"\\."),function(x)x[2])
#     #lat3[!lat3%in%"degrees_only" & sapply(tmp1,function(x)x[2])%in%c(mins,0)] = "minutes_only"
#     #long3[!long3%in%"degrees_only" & sapply(tmp2,function(x)x[2])%in%c(mins,0)] = "minutes_only"
#     mins1 = c(sapply(strsplit(as.character(round(1:59/60,6)),"\\."),function(x)x[2]),
#               sapply(strsplit(as.character(round(1:59/60,4)),"\\."),function(x)x[2]),
#               sapply(strsplit(as.character(round(1:59/60,2)),"\\."),function(x)x[2]))
#     lat3[!lat3%in%"degrees_only" & sapply(tmp1,function(x)x[2])%in%c(mins1,0)] = "minutes_only"
#     long3[!long3%in%"degrees_only" & sapply(tmp2,function(x)x[2])%in%c(mins1,0)] = "minutes_only"
#     #Minutes
#     mins = as.double(mins)/10^(nchar(mins))
#     secs = as.character(apply(cbind.data.frame(as.double(rep(c(mins,0),each=60)),as.double(rep(round(1:60/3600,6),60))),1,sum))
#     secs = sapply(strsplit(secs,"\\."),function(x)x[2])
#     secs = secs[!is.na(secs)]
#     lat3[!lat3%in%c("degrees_only","minutes_only") & sapply(tmp1,function(x)x[2])%in%secs] = "seconds"
#     long3[!lat3%in%c("degrees_only","minutes_only") & sapply(tmp2,function(x)x[2])%in%secs] = "seconds"
#     #Rounding issues
#     lat3[!lat3%in%c("degrees_only","minutes_only","seconds")] = "seconds?"
#     long3[!long3%in%c("degrees_only","minutes_only","seconds")] = "seconds?"
#     #Saving the resolution
#     tmp = herb.data$resolution.coord[!is.na(herb.data$latitude.work)]
#     tmp[lat3%in%"degrees_only" & long3%in%"degrees_only"] = "degrees_only"
#     tmp[lat3%in%"minutes_only" & long3%in%"minutes_only"] = "minutes_only"
#     tmp[lat3%in%"seconds" & long3%in%"seconds"] = "seconds"
#     tmp[lat3%in%"degrees_only" & long3%in%"minutes_only"] = "minutes_only"
#     tmp[long3%in%"degrees_only" & lat3%in%"minutes_only"] = "minutes_only"
#     tmp[lat3%in%"degrees_only" & long3%in%"seconds"] = "seconds"
#     tmp[long3%in%"degrees_only" & lat3%in%"seconds"] = "seconds"
#     tmp[lat3%in%"minutes_only" & long3%in%"seconds"] = "seconds"
#     tmp[long3%in%"minutes_only" & lat3%in%"seconds"] = "seconds"
#     tmp[lat3%in%"seconds" & long3%in%"seconds?"] = "seconds?"
#     tmp[long3%in%"seconds" & lat3%in%"seconds?"] = "seconds?"
#     tmp[lat3%in%"seconds?" & long3%in%"seconds?"] = "seconds?"
#     tmp[long3%in%"seconds?" & lat3%in%"seconds?"] = "seconds?"
#     herb.data$resolution.coord[!is.na(herb.data$latitude.work)] = tmp
#     ##Removing problematic coordinates still in the mix
#     herb.data$latitude.work[!is.na(herb.data$latitude.work)&abs(as.double(herb.data$latitude.work))>90] = NA
#     herb.data$longitude.work[!is.na(herb.data$latitude.work)&abs(as.double(herb.data$latitude.work))>90] = NA
#     herb.data$latitude.work[!is.na(herb.data$longitude.work)&abs(as.double(herb.data$longitude.work))>180] = NA
#     herb.data$longitude.work[!is.na(herb.data$longitude.work)&abs(as.double(herb.data$longitude.work))>180] = NA
#   }
#   ##Replacing missing coordinates by the county coordinates from the gazetter
#   herb.data$latitude.work[is.na(herb.data$latitude.work)] = herb.data$latitude.gazetteer[is.na(herb.data$latitude.work)]
#   herb.data$longitude.work[is.na(herb.data$longitude.work)] = herb.data$longitude.gazetteer[is.na(herb.data$longitude.work)]
#   herb.data$origin.coord[!is.na(herb.data$latitude.work)&is.na(herb.data$origin.coord)] = "coord_gazet"
#   herb.data$origin.coord[is.na(herb.data$origin.coord)] = "no_coord"
#   herb.data$origin.coord[herb.data$origin.coord %in% "coord_gazet" & herb.data$resolution.gazetteer %in% "country"] = "ok_country_gazet"
#   herb.data$origin.coord[herb.data$origin.coord %in% "coord_gazet" & herb.data$resolution.gazetteer %in% "state"] = "ok_state_gazet"
#   herb.data$origin.coord[herb.data$origin.coord %in% "coord_gazet" & herb.data$resolution.gazetteer %in% "county"] = "ok_county_gazet"
#   herb.data$origin.coord[herb.data$origin.coord %in% "coord_gazet" & herb.data$resolution.gazetteer %in% "locality"] = "ok_locality_gazet"
#   ##Saving the filtered data
#   path.csv = gsub("cleaned-loc.csv","cleaned-loc-coord.csv",myfiles0)
#   write.csv(x=herb.data,file=path.csv, row.names = FALSE)
#   cat(i,"\n")
# }

