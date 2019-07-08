#' @title Format Names, Numbers and Dates
#'
#' @description
#'
formatOcc = function(x) {
  loc = x
  return(loc)
}
#
# #### Finding and standardizing collector's and deteminer's names and collection number ####
#
# ### Loading packages and functions  ###
# #require(RCurl)
# require(stringr)
# source("functions.R")
# #require(rgdal)
# #require(utils)
# #require(finch)
# #require(devtools)
# #require(dplyr)
# require(countrycode)
#
# #### 1- Loading and editing speciesLink, speciesLink_new and JABOT data ####
#
# #Finding the data - speciesLink, speciesLink_new and JABOT
# paths = c(dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink",full.names=TRUE),
#           dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink_new",full.names=TRUE),
#           dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//jabot",full.names=TRUE))
#
# #Looping for each collection - speciesLink, speciesLink_new and JABOT
# for(i in 1:length(paths)) {
#   myfiles0 = list.files(paths[i], full.names = TRUE)
#   myfiles0 = myfiles0[grep("-edited.csv",myfiles0)]
#   herb.data = read.csv(myfiles0,as.is=TRUE,na.strings=c(""," ","NA"))
#   ##Removing unwanted columns
#   cols = c("collectionCode","collectioncode","siglacolecao","catalogNumber","catalognumber","numtombo",
#            "recordNumber","collectornumber","codespecime","numcoleta",
#            "recordedBy","recordedBy1","collector","coletor","coletor1",
#            "year","month","day","yearcollected","monthcollected","daycollected","diacoleta","mescoleta","anocoleta",
#            "country","pais","stateProvince","stateprovince","estado_prov","municipality","county","cidade",
#            "locality","descrlocal",
#            "decimalLatitude","decimalLongitude","latitude","longitude",
#            "identifiedBy","dateIdentified","typeStatus","identifiedby","yearidentified","typestatus","determinador","diadeterm","mesdeterm","anodeterm","nat_typus",
#            "family","familia","genus","genero","scientificName","scientificname",
#            "datasetkey","datelastmodified","occurrenceRemarks","notes","notes.x","notas")
#   cls =  cols[cols %in% names(herb.data)]
#   herb.data = herb.data[,cls]
#   ##Getting the vector with authors' names
#   coluna = names(herb.data)[names(herb.data) %in% c("recordedBy","recordedby","coletor","collector")]
#   nomes = herb.data[,coluna]
#   ##Editing collectors names
#   #Separation between multiple authors
#   nomes = gsub("  "," ",nomes)
#   nomes = gsub(' & | e | \\| ',"; ",nomes) #### INCLUDE HERE: ' et '
#   nomes = gsub("&|\\|",";",nomes)
#   nomes = gsub("; ;",";",nomes)
#   nomes = gsub(" ;",";",nomes)
#   nomes = gsub('^\\;','',nomes)
#   nomes = gsub('\\( ','\\(',nomes)
#   nomes = gsub(' \\)','\\)',nomes)
#   nomes = gsub(' <U+','; ',nomes)
#   nomes = gsub(" s.n. ",";",nomes)
#   nomes = gsub("Collector\\(s\\):","",nomes)
#   nomes = gsub("\\(Coll.",";",nomes)
#   nomes = gsub(" \\(\\?\\) ","; ",nomes)
#   nomes = as.character(unlist(sapply(strsplit(nomes,";"), head, n=1)))
#   #Removing unwanted character
#   nomes = gsub("[0-9]","",nomes)
#   nomes = gsub('\\(\\)|\\( \\)',"",nomes)
#   nomes = gsub('\\[|\\]',"",nomes)
#   nomes = gsub(' - | -|- ',"-",nomes)
#   nomes = gsub('^-- ',"",nomes)
#   nomes = gsub(' --$',"",nomes)
#   nomes = gsub('^-\\.$','SemColetor',nomes)
#   nomes = gsub('^-\\. ','',nomes)
#   nomes[grepl('^<',nomes)&grepl('>$',nomes)] = "EncodingError"
#   nomes = gsub('!','',nomes)
#   nomes = gsub('^\\* ','',nomes)
#   #Removing "et al." expression
#   nomes = gsub(" et alii| Et alii| Et Alii| et alli| Et alli| et all$|et alii$","",nomes)
#   nomes = gsub(" et\\. al\\.$| et\\.al\\.$| et al\\.$","",nomes)
#   nomes = gsub(" at al\\.$| etal\\.$| et,al\\.$| et, al\\.$|et. al\\.$","",nomes)
#   nomes = gsub(" et\\. al$| et\\.al$| et\\. al\\.\\.$","",nomes)
#   nomes = gsub(" et\\. al,\\.$| et\\.a l\\.$| et\\. al,\\.$| et\\. a\\.$","",nomes)
#   nomes = gsub(" et\\.al,\\.$| et\\.al\\.,$","",nomes)
#   nomes = gsub(" et al $| et al\\. $","",nomes)
#   nomes = gsub(" el al\\.$| Et al\\.$| Et Al\\.$| Et\\. Al\\.$| at al\\.$| et Al\\.$","",nomes)
#   nomes = gsub(" et al$|et al$","",nomes)
#   #Compound names
#   nomes = gsub("Jr\\.|jr\\.| jr$| Jr$"," Junior",nomes)
#   nomes = gsub(" - J?nior| - Junior"," Junior",nomes)
#   nomes = gsub(", J?nior,|, Junior,"," Junior,",nomes)
#   nomes = gsub("-J?nior|-Junior"," Junior",nomes)
#   nomes = gsub("JA?nior|jA?nior","Junior",nomes)
#   nomes = gsub(" F?| F?"," Filho",nomes)
#   nomes = gsub(' f\\.,'," Filho",nomes)
#   nomes = gsub(" - Filho"," Filho",nomes)
#   nomes = gsub("-Filho"," Filho",nomes)
#   nomes = gsub("- Filho"," Filho",nomes)
#   nomes = gsub(" Filho\\.,"," Filho,",nomes)
#   nomes = gsub(" Sobr?| Sobr?"," Sobrinho",nomes)
#   nomes = gsub(" Sobrinho\\.,| Sobr\\.,"," Sobrinho,",nomes)
#   #Formatting names without abbreviation
#   nomes = gsub("  "," ",nomes)
#   nomes = str_trim(nomes)
#   nomes = gsub('^\\.|^,','',nomes)
#   nomes = gsub(',$','',nomes)
#   nomes[!grepl("\\.$",nomes)&!grepl(",",nomes)] = as.character(sapply(nomes[!grepl("\\.$",nomes)&!grepl(",",nomes)],format.name))
#   #Editing prefixs or prepositions
#   nomes = gsub(' de$| do$| dos$| da$| van den$| van der$| van$| den$| von$| ter$',"",nomes, ignore.case=TRUE)
#   nomes = gsub('\\.de$|\\.do$|\\.dos$|\\.da$|\\.van den$|\\.van der$|\\.von$|\\.ter$',"",nomes, ignore.case=TRUE)
#   nomes = gsub(' de la |\\. de |\\. do |\\. dos |\\. da |\\. van den |\\. van der |\\. von |\\. ter ',"",nomes)
#   nomes = gsub(', de |, do |, dos |, da |, van den |, van der |, von |, ter ',"",nomes)
#   #Standardizing division between last and initials
#   nomes = gsub("  "," ",nomes)
#   nomes = gsub(",",", ",nomes)
#   nomes = gsub("  "," ",nomes)
#   nomes = gsub(" ,",",",nomes)
#   nomes = gsub("  "," ",nomes)
#   #Standardizing separation within initials
#   nomes[grep("[A-Z]\\. [A-Z]\\.",nomes)] = gsub("\\. ","\\.",nomes[grep("[A-Z]\\. [A-Z]\\.",nomes)])
#   #Encoding problems
#   nomes = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), nomes)
#   nomes = as.character(iconv(nomes, from="UTF-8", to="windows-1252//TRANSLIT"))
#   nomes = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), nomes)
#   nomes = str_trim(nomes)
#   #No collector information
#   nomes[is.na(nomes)|nomes %in% ""] = "SemColetor"
#   nomes[nomes %in% c("s/col.","s/col","s/c","s/coletor"," sem col.","s.col.")] = "SemColetor"
#   nomes[nomes %in% c("Collector unspecified","Collector unknown")] = "SemColetor"
#   nomes[nomes %in% c("?")] = "SemColetor"
#   nomes = gsub("NANA","SemColetor",nomes)
#   #Last stardardization
#   nomes[!is.na(nomes)&!grepl('\\.',nomes)&str_count(nomes, ", ")==1] = as.character(unlist(sapply(nomes[!is.na(nomes)&!grepl('\\.',nomes)&str_count(nomes, ", ")==1],format.name1)))
#   #Final corrections
#   nomes = gsub('\\. \\.',".",nomes)
#   nomes = gsub('\\(\\.\\)',".",nomes)
#   nomes = gsub("\\.\\'\\.",".",nomes)
#   nomes = gsub('\\.\\.\\.',".",nomes)
#   nomes = gsub('\\.\\.',".",nomes)
#   nomes = gsub('\\/,',",",nomes)
#   nomes = gsub('\\/-,',',',nomes)
#   nomes = gsub(',,',",",nomes)
#   nomes = str_trim(nomes)
#   #final edits
#   nomes = gsub('a?',"o",nomes)
#   nomes = gsub('a?|a?',"e",nomes)
#   nomes = gsub('a?',"o",nomes)
#   nomes = gsub("a?","a",nomes)
#   nomes = gsub('a?|a?|a?',"u",nomes)
#   nomes = gsub("a?","c",nomes)
#   nomes = gsub("a?","o",nomes)
#   nomes = gsub("a?|a?|a?|a?","a",nomes)
#   #Saving the editions and new name columns
#   herb.data$coletor.name = nomes
#   herb.data$coletor.last.name = as.vector(unlist(sapply(nomes,lastname)))
#   herb.data$coletor.fingerprint = as.vector(sapply(nomes,fingerprint))
#   #Replacing names to decrease file size
#   herb.data$coletor.last.name[herb.data$coletor.name %in% "SemColetor"] = NA
#   herb.data$coletor.fingerprint[herb.data$coletor.name %in% "SemColetor"] = NA
#
#   ##Getting the vector with determiners' names
#   coluna = names(herb.data)[names(herb.data) %in% c("identifiedBy","identifiedby","determinador")]
#   nomes = herb.data[,coluna]
#   #Separation between multiple authors
#   nomes = gsub("  "," ",nomes)
#   nomes = gsub(' & | e | \\| ',"; ",nomes)
#   nomes = gsub("&|\\|",";",nomes)
#   nomes = gsub("; ;",";",nomes)
#   nomes = gsub(" ;",";",nomes)
#   nomes = gsub('^\\;','',nomes)
#   nomes = gsub('\\( ','\\(',nomes)
#   nomes = gsub(' \\)','\\)',nomes)
#   nomes = gsub(' <U+','; ',nomes)
#   nomes = gsub(" s.n. ",";",nomes)
#   nomes = gsub('Data:|Date:',";",nomes, ignore.case=T)
#   #preparing to remove dates of determination
#   nomes = gsub(', [0-9][0-9]|\\. [0-9][0-9]',";",nomes, ignore.case=T)
#   nomes = gsub(', [0-9]/|, [0-9]-|\\. [0-9]/',";",nomes, ignore.case=T)
#   nomes = gsub(" [0-9][0-9]| \\'[0-9][0-9]",";",nomes, ignore.case=T)
#   #preparing to remove months of determination
#   meses = paste(paste(", ",month.name,"/",sep=""),paste(", ",month.abb,"/",sep=""),collapse="|")
#   nomes = gsub(meses,";",nomes)
#   meses = paste(paste(", ",month.name,"-",sep=""),paste(", ",month.abb,"-",sep=""),collapse="|")
#   nomes = gsub(meses,";",nomes)
#   meses = paste(paste(", ",month.name," ",sep=""),paste(", ",month.abb," ",sep=""),collapse="|")
#   nomes = gsub(meses,";",nomes)
#   nomes[grep("\\([A-Z]|[A-Z]\\)",nomes)] = gsub(" \\(","; ",nomes[grep("\\([A-Z]|[A-Z]\\)",nomes)])
#   nomes = gsub(" \\(\\?\\) ","; ",nomes)
#   nomes[nomes==""] = "SemDeterminador"
#   nomes = as.character(unlist(sapply(strsplit(nomes,";"), head, n=1)))
#
#   #Removing unwanted character/expressions
#   nomes = gsub("[0-9]","",nomes)
#   nomes = gsub('\\(\\)|\\( \\)',"",nomes)
#   nomes = gsub(' - | -|- ',"-",nomes)
#   nomes = gsub('\\[|\\]',"",nomes)
#   nomes = gsub(', date unknown',"",nomes, ignore.case=T)
#   nomes = gsub('Determiner unknown|Determiner unspecified','SemDeterminador',nomes,ignore.case=T)
#   nomes = gsub('^det\\. by|',"",nomes, ignore.case=T)
#   nomes = gsub('^det\\.:|^det:',"",nomes, ignore.case=T)
#   nomes = gsub('^det\\.|^det\\. ',"",nomes, ignore.case=T)
#   nomes = gsub('^-- ',"",nomes)
#   nomes = gsub(' --$',"",nomes)
#   nomes = gsub('^-\\.$','SemDeterminador',nomes)
#   nomes = gsub('^-\\. ','',nomes)
#   nomes[grepl('^<',nomes)&grepl('>$',nomes)] = "EncodingError"
#   nomes = gsub('!','',nomes)
#   nomes = gsub('^\\* ','',nomes)
#   #Removing the type indication
#   nomes = gsub("-Paratype|, Paratype|, paratype|-TYPE|-Lectotype| (Lectotype)| (Paratype)| ,(Isotype:MO)|, Syntype|-Syntype","",nomes)
#   nomes = gsub("Isotype: MO\\/ |Isotype:MO| Isolectotype|Isotype|, US - Syntype","",nomes)
#   #Removing "et al." expression
#   nomes = gsub(" et alii| Et alii| Et Alii| et alli| Et alli| et all$|et alii$","",nomes)
#   nomes = gsub(" et\\. al\\.$| et\\.al\\.$| et al\\.$","",nomes)
#   nomes = gsub(" at al\\.$| etal\\.$| et,al\\.$| et, al\\.$|et. al\\.$","",nomes)
#   nomes = gsub(" et\\. al$| et\\.al$| et\\. al\\.\\.$","",nomes)
#   nomes = gsub(" et\\. al,\\.$| et\\.a l\\.$| et\\. al,\\.$| et\\. a\\.$","",nomes)
#   nomes = gsub(" et\\.al,\\.$| et\\.al\\.,$","",nomes)
#   nomes = gsub(" et al $| et al\\. $","",nomes)
#   nomes = gsub(" el al\\.$| Et al\\.$| Et Al\\.$| Et\\. Al\\.$| at al\\.$| et Al\\.$","",nomes)
#   nomes = gsub(" et al$|et al$","",nomes)
#   #Compound names
#   nomes = gsub("Jr\\.|jr\\.| jr$| Jr$"," Junior",nomes)
#   nomes = gsub(" - J?nior| - Junior"," Junior",nomes)
#   nomes = gsub(", J?nior,|, Junior,"," Junior,",nomes)
#   nomes = gsub("-J?nior|-Junior"," Junior",nomes)
#   nomes = gsub("JA?nior|jA?nior","Junior",nomes)
#   nomes = gsub(" F?| F?"," Filho",nomes)
#   nomes = gsub(' f\\.,'," Filho",nomes)
#   nomes = gsub(" - Filho"," Filho",nomes)
#   nomes = gsub("-Filho"," Filho",nomes)
#   nomes = gsub("- Filho"," Filho",nomes)
#   nomes = gsub(" Filho\\.,"," Filho,",nomes)
#   nomes = gsub(" Sobr?| Sobr?"," Sobrinho",nomes)
#   nomes = gsub(" Sobrinho\\.,| Sobr\\.,"," Sobrinho,",nomes)
#   #Formatting names without abbreviation
#   nomes = gsub("  "," ",nomes)
#   nomes = str_trim(nomes)
#   nomes = gsub('^\\.|^,','',nomes)
#   nomes = gsub(',$','',nomes)
#   nomes[!is.na(nomes)&!grepl("\\.$",nomes)&!grepl(",",nomes)] = as.character(sapply(nomes[!is.na(nomes)&!grepl("\\.$",nomes)&!grepl(",",nomes)],format.name))
#   #Editing prefixs or prepositions
#   nomes = gsub(' de$| do$| dos$| da$| van den$| van der$| van$| den$| von$| ter$',"",nomes, ignore.case=TRUE)
#   nomes = gsub('\\.de$|\\.do$|\\.dos$|\\.da$|\\.van den$|\\.van der$|\\.von$|\\.ter$',"",nomes, ignore.case=TRUE)
#   nomes = gsub(' de la |\\. de |\\. do |\\. dos |\\. da |\\. van den |\\. van der |\\. von |\\. ter ',"",nomes)
#   nomes = gsub(', de |, do |, dos |, da |, van den |, van der |, von |, ter ',"",nomes)
#   #Standardizing division between last and initials
#   nomes = gsub("  "," ",nomes)
#   nomes = gsub(",",", ",nomes)
#   nomes = gsub("  "," ",nomes)
#   nomes = gsub(" ,",",",nomes)
#   nomes = gsub("  "," ",nomes)
#   #Standardizing separation within initials
#   nomes[grep("[A-Z]\\. [A-Z]\\.",nomes)] = gsub("\\. ","\\.",nomes[grep("[A-Z]\\. [A-Z]\\.",nomes)])
#   #Encoding problems
#   nomes = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), nomes)
#   nomes = as.character(iconv(nomes, from="UTF-8", to="windows-1252//TRANSLIT"))
#   nomes = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), nomes)
#   nomes = str_trim(nomes)
#   #Last stardardization
#   nomes[!is.na(nomes)&!grepl('\\.',nomes)&str_count(nomes, ", ")==1] = as.character(unlist(sapply(nomes[!is.na(nomes)&!grepl('\\.',nomes)&str_count(nomes, ", ")==1],format.name1)))
#   #No determiner information
#   nomes[is.na(nomes)|nomes %in% ""] = "SemDeterminador"
#   nomes[nomes %in% c("s/det.","s/det","s/d","s/determinador"," sem det.","s.det.")] = "SemDeterminador"
#   nomes[nomes %in% c("Determiner unspecified","Determiner unknown")] = "SemDeterminador"
#   nomes[nomes %in% c("?")] = "SemDeterminador"
#   nomes = gsub("NANA","SemDeterminador",nomes)
#   if(any(grepl('\\(\\?\\)',nomes))) nomes[grep('\\(\\?\\)',nomes)] = 	sapply(strsplit(nomes[grep('\\(\\?\\)',nomes)],"\\(\\?\\)"), function(x) format.name(x[1]))
#   #Final edits
#   nomes = gsub(", --$","",nomes)
#   nomes = gsub(', //$','',nomes)
#   nomes = gsub(', /$','',nomes)
#   nomes = gsub(', \'$','',nomes)
#   nomes = gsub('\\.\\.$','\\.',nomes)
#   nomes = gsub('\\.\\.$','\\.',nomes)
#   nomes = gsub('\\. /$','\\.',nomes)
#   nomes = gsub('^Disponible, N\\.$','SemDeterminador',nomes)
#   nomes = gsub('^Sin$','SemDeterminador',nomes)
#   nomes = gsub('^Pjm, M\\.$','Maas, P.J.M.',nomes)
#   nomes = gsub('AraA?jo|Araa?jo','Araujo',nomes)
#   nomes = gsub('a?o|a?o','ao',nomes)
#   nomes = gsub('ra?es','raes',nomes)
#   #Super final starndardization
#   nomes[!is.na(nomes)&!grepl("\\.$",nomes)&!grepl(",",nomes)] = as.character(sapply(nomes[!is.na(nomes)&!grepl("\\.$",nomes)&!grepl(",",nomes)],format.name))
#   nomes[!is.na(nomes)&!grepl(",",nomes)] = as.character(sapply(nomes[!is.na(nomes)&!grepl(",",nomes)],format.name))
#   #Super extra final corrections
#   nomes = gsub('\\. \\.',".",nomes)
#   nomes = gsub('\\(\\.\\)',".",nomes)
#   nomes = gsub("\\.\\'\\.",".",nomes)
#   nomes = gsub('\\.\\.\\.',".",nomes)
#   nomes = gsub('\\.\\.',".",nomes)
#   nomes = gsub('\\/,',",",nomes)
#   nomes = gsub('\\/-,',',',nomes)
#   nomes = gsub(',,',",",nomes)
#   nomes = str_trim(nomes)
#   #Uber Super extra final corrections
#   nomes = gsub('a?',"o",nomes)
#   nomes = gsub('a?|a?',"e",nomes)
#   nomes = gsub('a?',"o",nomes)
#   nomes = gsub("a?","a",nomes)
#   nomes = gsub('a?|a?|a?',"u",nomes)
#   nomes = gsub("a?","c",nomes)
#   nomes = gsub("a?","o",nomes)
#   nomes = gsub("a?|a?|a?|a?","a",nomes)
#   #Saving the editions and new name columns
#   herb.data$determinador.name = nomes
#   herb.data$determinador.last.name = as.vector(unlist(sapply(nomes,lastname)))
#   herb.data$determinador.fingerprint = as.vector(sapply(nomes,fingerprint))
#   #Replacing names to descrease file size
#   herb.data$determinador.last.name[herb.data$determinador.name %in% "Semdeterminador"] = NA
#   herb.data$determinador.fingerprint[herb.data$determinador.name %in% "Semdeterminador"] = NA
#
#   ##Getting the vector with collectors numbers
#   coluna = names(herb.data)[names(herb.data) %in% c("recordNumber","recordnumber","numcoleta","collectornumber")]
#   numbs = herb.data[,coluna]
#   #Editing collectors numbers
#   numbs = gsub('s\\.n\\.|s\\.n|s/n?|S/N|S\\.N\\.|s/n?|s/n?|^s/n$|^s/n\\.$',"SemNumero",numbs)
#   numbs[numbs %in% "sn"] = "SemNumero"
#   numbs[is.na(numbs)|numbs %in% c("")] = "SemNumero"
#   numbs[numbs %in% c("Number unspecified")] = "SemNumero"
#   numbs[is.na(numbs)&grepl(" s.n. ",numbs)] = "SemNumero"
#   #Removing names of collectors from the numbers
#   numbs[!is.na(numbs)&grepl("[a-z][a-z][a-z] ",numbs,ignore.case = TRUE)] = as.character(sapply(strsplit(numbs[!is.na(numbs)&grepl("[a-z][a-z][a-z] ",numbs,ignore.case = TRUE)], " "),function(x) x[grepl('[0-9]|SemNumero',x)]))
#   numbs[!is.na(numbs)&grepl("SemNumero",numbs)] = "SemNumero"
#   numbs[!is.na(numbs)&grepl("character\\(0\\)",numbs)] = "SemNumero"
#   #Removing unwanted characters
#   numbs = gsub(' - ',"-",numbs)
#   numbs = gsub(' \\(',"\\(",numbs)
#   numbs = gsub(' ',"-",numbs)
#   numbs = gsub(' e ',", ",numbs)
#   numbs = gsub('#|\\?|\\!|\\.',"",numbs)
#   numbs = gsub(", ",",",numbs)
#   numbs = gsub("Collector Number:","",numbs)
#   numbs = gsub("NANA","SemNumero",numbs)
#   numbs = gsub('^--$',"SemNumero",numbs)
#   numbs = gsub('^-',"",numbs)
#   numbs = gsub('-$',"",numbs)
#   numbs = gsub("SemNumero","s.n.",numbs)
#   herb.data$coletor.number = str_trim(numbs)
#
#   ##Editing the determination date
#   ##TAKE CODED FROM THE: CREATING COLLECTION AND SPECIMEN UNIQUE IDENTIFIERS AND PREPARING DATA FOR MERGE (03_data_validation.R)
#
#   ##Saving the edited/filtered tree records
#   path.csv = gsub("-edited.csv","-edited-cleaned.csv",myfiles0)
#   write.csv(x=herb.data,file=path.csv, row.names = FALSE)
#   cat(i,"\n")
# }
