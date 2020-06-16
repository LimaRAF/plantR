#' @title Validate Specimen Taxonomy
#'
#' @description This function ...
#'
#' @param x Character. Species name
#'
validateTax = function(x) {
  loc = x
  return(loc)
}
### LOADING PACKAGES ###
# require(flora)
# require(taxize)
# require(stringr)
# require(dplyr)
# require(raster)
# require(rgeos)
# require(sp)
# require(parallel)
# require(doParallel)
# require(rgdal)
# require(geosphere)
# #require(ModelR)
# source("functions.R")
#
#
# #### TAXONOMIC VALIDATION ####
#
# ### Reading and editing the validated species name and synonyms list ###
# spp = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//DomainsKnownTreesNeotropics.csv", as.is=TRUE, na.string=c(NA,""," "))
# #removing unecessary columns
# spp = spp[,c("TreeCo_status","SpeciesKTSA","family.reflora","genus.reflora","species.reflora","taxon.rank.reflora","name.status.reflora","notes.reflora",
#              "Accepted_name_family","Accepted_name_genus","Accepted_name","Accepted_name_rank","Taxonomic_status","DataSource",
#              "Family.TPL","New.Genus.TPL","NewTaxon.TPL","Taxonomic.status.TPL")]
#
# #replacing names not found in ReFlora by The Plant List (previous to TNRS)
# spp[is.na(spp$species.reflora) & spp$TreeCo_status%in%"ok_use_TPL",c("family.reflora","genus.reflora","species.reflora")] =
#   spp[is.na(spp$species.reflora) & spp$TreeCo_status%in%"ok_use_TPL",c("Family.TPL","New.Genus.TPL","NewTaxon.TPL")]
# #replacing names not found in ReFlora by the TNRS/Hans taxonomy
# spp[is.na(spp$species.reflora),c("family.reflora","genus.reflora","species.reflora","name.status.reflora")] =
#   spp[is.na(spp$species.reflora),c("Accepted_name_family","Accepted_name_genus","Accepted_name","Taxonomic_status")]
# #replacing final names not found in ReFlora and TNRS by the Plant List
# spp[is.na(spp$species.reflora) & grepl("ok",spp$TreeCo_status),c("family.reflora","genus.reflora","species.reflora")] =
#   spp[is.na(spp$species.reflora) & grepl("ok",spp$TreeCo_status),c("Family.TPL","New.Genus.TPL","NewTaxon.TPL")]
#
# #flagging problematic names (and putting them down the list)
# table(spp[is.na(spp$species.reflora),]$Taxonomic_status)
# table(spp[is.na(spp$species.reflora),]$TreeCo_status)
# tmp = spp[is.na(spp$species.reflora),]
# tmp$species.reflora = tmp$SpeciesKTSA
# tmp$name.status.reflora = "problematic"
# spp = spp[!is.na(spp$species.reflora),]
# table(spp$Taxonomic_status)
# table(spp$TreeCo_status)
# spp = rbind.data.frame(spp,tmp) #recombining valid and problematic names into a single data frame
# rm(tmp)
#
# #filtering columns
# spp = spp[,c("SpeciesKTSA","family.reflora","genus.reflora","species.reflora","taxon.rank.reflora","name.status.reflora","notes.reflora","DataSource","TreeCo_status"),]
# names(spp) = c("name","family.correct","genus.correct","species.correct","taxon.rank","name.status","notes","source","status")
#
# #checking possible problems with family names
# tmp = aggregate(spp$family.correct,list(spp$genus.correct),function(x)as.character(unique(x)))
# tmp[is.na(tmp[,2]),]
# tmp[is.na(tmp[,1]),]
# tmp[grep(" ",tmp[,1]),]
# tmp[grep("-",tmp[,1]),]
# tmp[grep("-",tmp[,2]),]
#
# ##Considering names only at species level
# #getting the names with any of the infra-specific notation
# tmp = spp[grepl(" var\\.| subsp\\.| f\\.| fo\\.| ssp\\.| subf\\.| subfo\\.| subvar\\.",spp$name),]
# #removing the infra-specific notation from the input names
# spp$name = gsub(" var\\.| subsp\\.| f\\.| fo\\.| ssp\\.| subf\\.| subfo\\.| subvar\\.","",spp$name)
# spp = spp[!grepl("\\.",spp$name),]
# #Re-adding the infra-specific taxa in their original notation to make sure all names are found, and thus all family names from the occurrences are corrected
# spp = rbind.data.frame(spp,tmp)
#
# ### Reading the specialists' name dictionary ###
# autores = read.csv("autores.csv", as.is=TRUE,na.string=c(NA,""," "))
# autores = autores[!is.na(autores$tdwg.name),]
# autores = autores[!is.na(autores$family),]
# autores = autores[!grepl('\\?',autores$family),]
# autores = autores[!grepl('Floristics/Generalist (all families)|Wood anatomist, autores$family'),]
#
# #Standardizing family names
# families.apg = read.csv("families_synonyms.csv", as.is=TRUE,na.string=c(NA,""," "))
# autores = merge(autores,families.apg[,c("name","name.correct")],by.x="family",by.y="name",all.x=TRUE)
# autores = autores[order(autores$order),]
# #Getting misspelled family names
# tmp = autores[is.na(autores$name.correct),]
# head(tmp) #only fungi and cyano bacteria are not in the dictionary of name synonyms
# #tmp1 = get.taxa (unique(tmp$family), life.form = FALSE, habitat = FALSE, states = FALSE,
# #	suggestion.distance = 0.92, drop = c("id","accepted.name","search.str","scientific.name","specific.epiteth","infra.epiteth","authorship","threat.status"))
# #tmp1.1 = tmp1[!is.na(tmp1$taxon.rank) & tmp1$taxon.rank %in% "family",]
# #tmp1.1 = tmp1.1[order(tmp1.1$order),]
# #autores[is.na(autores$name.correct),c("name.correct")] = tmp1.1[is.na(autores$name.correct),c("family.y")]
# #Getting names not found
# #tmp1.2 = tmp1[tmp1$notes %in% c("not found"),]
# #sort(tmp1.2$original.search)
#
# #Getting the unique family-specialist combinations (3 options: tdwg, last.name, finger.print)
# combo = unique(paste(autores$family,autores$tdwg.name,sep="_"))
# tmp = unique(paste(autores$name.correct[!is.na(autores$name.correct)],autores$tdwg.name[!is.na(autores$name.correct)],sep="_"))
# tmp = tmp [!tmp %in% combo]
# combo = c(combo,tmp)
# combo = str_trim(combo)
# table(duplicated(combo))
#
# #combo1 = c(paste(autores$family,autores$last.name,sep="_")) #,paste(autores$family.y,autores$last.name,sep="_")))
# #combo2 = c(paste(autores$family,autores$finger.print,sep="_")) #,paste(autores$family.y,autores$finger.print,sep="_")))
#
# ### Getting the file paths for the herbarium data
# splink = read.csv("speciesLink.csv", as.is=T)
# splink.new = read.csv("speciesLink_new.csv", as.is=T)
# paths = dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink",full.names=TRUE)
# paths1 = "C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink_new"
# paths2 = "C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//jabot"
# #paths1 = dir(paths1,full.names=TRUE)[!dir(paths1) %in% splink$acronym]
# #paths2 = dir(paths2,full.names=TRUE)[!dir(paths2) %in% splink$acronym]
# paths1 = dir(paths1,full.names=TRUE)
# paths2 = dir(paths2,full.names=TRUE)
# #putting paths for all collections together
# paths = c(paths,paths1,paths2)
# paths = list.files(paths,full.names=TRUE)
# paths = c(paths, list.files("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//gbif",full.names=TRUE))
# paths = paths[grep("-edited-cleaned-loc-coord.csv",paths)]
#
# ### Perfoming the taxonomical validation for all collections ###
# for(i in 1:length(paths)) {
#   #for(i in 1:199) {
#   myfiles0 = paths[i]
#   herb.data = read.csv(myfiles0,as.is=TRUE,header=TRUE,na.strings=c(""," ","NA"))
#   ##Removing unwanted columns
#   cols = c("family","familia",
#            "scientificName","scientificname",
#            "determinador.name","coletor.name",
#            "typeStatus","typestatus","nat_typus")
#   #"dateIdentified","yearidentified,"anodeterm") #columns not present in GBIF
#   cls =  c(unique(cols[cols %in% names(herb.data)]),"order")
#   herb.data$order = as.vector(1:dim(herb.data)[1]) #column to put the data back on its original order
#   herb.data1 = herb.data[,cls]
#   names(herb.data1)[1] = "family.original"
#   names(herb.data1)[2] = "scientificName"
#   names(herb.data1)[length(names(herb.data1))-1] = "typeStatus"
#   ##Standardizing species notation for JABOT - Not needed anymore, the species dictionary now has infra-specific taxons with and without the notations below
#   #if(grepl("/jabot/",myfiles0)) {
#   #	spp1 = spp
#   #	spp1$name = gsub(' var\\.| subsp\\.| f\\.| fo\\.| ssp\\.| subf\\.| subfo\\.','',spp1$name)
#   #} else {
#   #
#   #	spp1 = spp
#   #}
#   ##Obtainig the valid species name and family for each occurrence
#   herb.data1 = merge(herb.data1, spp[,c("name","family.correct","species.correct","name.status","notes","source","status")],
#                      by.x="scientificName",by.y="name",all.x=TRUE)
#   herb.data1$family.correct[is.na(herb.data1$family.correct)] = herb.data1$family.original[is.na(herb.data1$family.correct)]
#   herb.data1$species.correct[is.na(herb.data1$species.correct)] = herb.data1$scientificName[is.na(herb.data1$species.correct)]
#   herb.data1 = herb.data1[order(herb.data1$order),]
#   ##Putting all infrageneric identifications at species level
#   herb.data1$species.correct = sapply(strsplit(herb.data1$species.correct, " var\\."),function(x) x[1])
#   herb.data1$species.correct = sapply(strsplit(herb.data1$species.correct, " subsp\\."),function(x) x[1])
#   herb.data1$species.correct = sapply(strsplit(herb.data1$species.correct, " f\\."),function(x) x[1])
#   herb.data1$species.correct = sapply(strsplit(herb.data1$species.correct, " fo\\."),function(x) x[1])
#   herb.data1$species.correct = sapply(strsplit(herb.data1$species.correct, " ssp\\."),function(x) x[1])
#   herb.data1$species.correct = sapply(strsplit(herb.data1$species.correct, " subf\\."),function(x) x[1])
#   herb.data1$species.correct = sapply(strsplit(herb.data1$species.correct, " subfo\\."),function(x) x[1])
#   herb.data1$species.correct = sapply(strsplit(herb.data1$species.correct, " subvar\\."),function(x) x[1])
#   ##Obtainig the family-specialist combinations for each occurrence
#   herb.data1$combo = paste(herb.data1$family.correct,herb.data1$determinador.name,sep="_")
#   herb.data1$combo = str_trim(herb.data1$combo)
#   ##Crossing the occurrence and reference family-specialist combinations
#   herb.data1$tax.check = herb.data1$combo %in% combo
#   ##Finding possible missing determinations due to small typos (max.2 letters of difference)
#   #tmp = unique(herb.data1[!herb.data1$tax.check & !herb.data1$determinador.name %in% c("SemDeterminador","Semdeterminador"),]$combo)
#   #names(tmp) = unique(herb.data1[!herb.data1$tax.check & !herb.data1$determinador.name %in% c("SemDeterminador","Semdeterminador"),]$combo)
#   #if(length(tmp)==0) { herb.data1$tax.check.new = FALSE
#   #} else {
#   #	cl <- makeCluster(detectCores()-1)
#   #	clusterExport(cl, list("tmp","combo"),envir=environment())
#   #	tmp2 <- parLapply(cl,1:length(tmp), fun= function(j) {
#   #					tmp1 = agrep(tmp[j], combo, value=TRUE, max.distance=2)
#   #					if(length(tmp1)==0) {
#   #						tmp[j] = tmp[j]
#   #					} else {
#   #						dst = adist(tmp[j],tmp1)
#   #						id = which(dst==min(dst))
#   #						tmp[j] = head(tmp1[id],1)
#   #					} } )
#   #	stopImplicitCluster()
#   #	stopCluster(cl)
#   #	tmp2 = do.call(c, tmp2)
#   #	tmp1 = data.frame(combo=names(tmp),combo.new=tmp2, stringsAsFactors=FALSE)
#   #	tmp2 = herb.data1[!herb.data1$tax.check & !herb.data1$determinador.name %in% c("SemDeterminador","Semdeterminador"),c("order","combo")]
#   #	tmp3 = merge(tmp2,tmp1,by="combo",all.x=TRUE)
#   #	tmp3 = tmp3[order(tmp3$order),] #putting the data back on its original order
#   #	herb.data1[!herb.data1$tax.check & !herb.data1$determinador.name %in% c("SemDeterminador","Semdeterminador"),]$combo = as.character(tmp3$combo.new)
#   #	herb.data1$tax.check.new = herb.data1$combo %in% combo
#   #}
#   ##Validating all type specimens (isotype, paratypes, etc) but not the "not a type"
#     herb.data1$tax.check[!is.na(herb.data1$typeStatus)&!grepl("not a type|notatype|probable type|tipo provavel|tipo prov?vel",herb.data1$typeStatus,ignore.case = TRUE)] = TRUE
#   ##Specifying why taxonomy was not validated
#    herb.data1$tax.check[herb.data1$tax.check == FALSE &
#                          herb.data1$determinador.name %in% c("Semdeterminador","SemDeterminador","Anonymus","Anonymous","Anonimo","Incognito","Unknown","s.d.")] = "cannot_check"
#   ##Validating all specimens collected by the family specialist but with the determiner field empty
#    herb.data1$combo1 = paste(herb.data1$family.correct,herb.data1$coletor.name,sep="_")
#    herb.data1$combo1 = str_trim(herb.data1$combo1)
#    #Crossing the occurrence and reference family-specialist combinations
#    herb.data1$tax.check1 = herb.data1$combo1 %in% combo
#    #Replacing the specimens that could by validated
#    herb.data1$tax.check[herb.data1$tax.check %in% c("cannot_check") & herb.data1$tax.check1 %in% TRUE] = TRUE
#   ##Saving the filtered data
#     herb.data = merge(herb.data,herb.data1[,c("order","family.correct","species.correct","name.status","notes","source","status","tax.check")],by="order",all.x=T)
#     herb.data = herb.data[!duplicated(herb.data$order),]
#     path.csv = gsub("coord.csv","coord-tax.csv",myfiles0)
#     write.csv(x=herb.data,file=path.csv, row.names = FALSE)
#     cat(i,"\n")
# }
