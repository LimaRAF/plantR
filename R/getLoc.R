#' @title Get Locality and Coordinates
#'
#' @description Create the locality string used to search for coordinates in a gazetteer. This information is
#' used to obtain missing coordinates based on locality descriptions and in the process of validation of the
#' geographical coordinates.
#'
#' @param x a data.frame containig the strings for locality search
#' @param gazet a data.frame containing the gazetteer; the default is "plantR", the default gazetteer (biased
#' towards Latin America)
#'
#' @return the data frame \code{x} with new columns obtained from the gazetteer.
#'
#' @details The function ...
#'
#' If the column 'locality.scrap' is present in the input data frame, it is also used to produce
#' alternative locality strings, which are crossed against the gazetteers, to potentialize
#' the retrieval of locality information from the gazetteer.
#'
#' @author Lima, R.A.F.
#'
#' @importFrom dplyr left_join
#' @importFrom stringr str_count
#'
#' @export getLoc
#'
getLoc = function(x, gazet = "plantR") {
  require(dplyr)

  ## check input:
    if (!class(x) == "data.frame") { stop("input object needs to be a data frame!") }
    if (!any(grepl("loc.string", names(x)))) { stop("input object needs to have at least one locality string: loc.string, loc.string1, loc.string2") }

  ## putting the input data in the right order
    x1 = x[which(colnames(x) %in%
                 c("resol.orig","loc.string","loc.string1","loc.string2"))]

  ## Getting coordinates from the gazetteer - county level or lower (provided in the occuurrence labels)
    if(all(gazet %in% c("plantR","plantr"))) {
      load("./R/sysdata.rda")
      dic <- gazetteer
    } else {
      dic <- gazet
      if(!"resolution.gazetteer" %in% names(dic)) dic$resolution.gazetteer = NA
    }

  # merging the occurrence data with the gazetteer information
  tmp = left_join(data.frame(loc = x1[,2], stringsAsFactors= FALSE),
                  dic[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")],
                  by = "loc")
  # Downgrading the locality resolution for the localities not found in the gazetteer
  tmp1 = tmp$loc[!is.na(tmp$loc) & is.na(tmp$loc.correct)]
  if(length(tmp1)>0) {
    if(any(str_count(tmp1,"_")==1)) tmp1[str_count(tmp1,"_")==1] = sapply(strsplit(tmp1[str_count(tmp1,"_")==1],"_"), function(x) x[1])
    if(any(str_count(tmp1,"_")==2)) tmp1[str_count(tmp1,"_")==2] = sapply(strsplit(tmp1[str_count(tmp1,"_")==2],"_"), function(x) paste(x[1],x[2],sep="_"))
    tmp2 = left_join(data.frame(loc=tmp1, stringsAsFactors= FALSE),
                     dic[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")],
                     by = "loc")
    #if nothing is found in the gazetteer, we finally try at country level:
    tmp3 = tmp2$loc[is.na(tmp2$loc.correct)]
    tmp3 = sapply(strsplit(tmp3,"_"), function(x) x[1])
    tmp3 = left_join(data.frame(loc=tmp3, stringsAsFactors= FALSE),
                     dic[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")],
                     by = "loc")
    tmp2[is.na(tmp2$loc.correct), c("loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")] =
      tmp3[ ,c("loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")]
    #saving all dowgraded locality infor from the gazetteer
    tmp[!is.na(tmp$loc) & is.na(tmp$loc.correct), c("loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")] =
      tmp2[ ,c("loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")]
  }

  ## Getting coordinates from the gazetteer - locality level
    dic1 = dic[dic$resolution.gazetteer %in% c("localidade","localidade|sublocalidade","sublocalidade","distrito|vila","distrito","distrito|bairro","bairro","cachoeira","mina","vila","serra"),]
    tmp3 = left_join(data.frame(loc=x1[,3], stringsAsFactors= FALSE),
                     dic1[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")],
                     by = "loc")
    # Replacing coordinates at county level by those found at locality level
    tmp[tmp$resolution.gazetteer %in% "county" & !is.na(tmp3$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")] =
      tmp3[tmp$resolution.gazetteer %in% "county" & !is.na(tmp3$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")]

    # Replacing coordinates at state level by those found at locality level (rare but can happen due to missing counties)
    tmp[tmp$resolution.gazetteer %in% "state" & !is.na(tmp3$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")] =
      tmp3[tmp$resolution.gazetteer %in% "state" & !is.na(tmp3$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")]
    # Replacing coordinates at country level by those found at locality level due to missing state and counties
    tmp[tmp$resolution.gazetteer %in% "country" & !is.na(tmp3$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")] =
      tmp3[tmp$resolution.gazetteer %in% "country" & !is.na(tmp3$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")]
    tmp$resolution.gazetteer[tmp$resolution.gazetteer %in% c("localidade","localidade|sublocalidade","sublocalidade","distrito|vila","distrito","distrito|bairro","bairro","cachoeira","mina","vila","serra")] = "locality"

  ## getting coordinates from the gazetteer - county level extracted from the locality (not 100% sure? needs validation...)
    if("loc.string2" %in% names(x1)) {
      tmp4 = left_join(data.frame(loc = x1[,4], stringsAsFactors= FALSE),
                       dic[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")],
                       by = "loc")
      # replacing coordinates at state level by those found at county level
      tmp[tmp$resolution.gazetteer %in% "state" & !is.na(tmp4$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")] =
        tmp4[tmp$resolution.gazetteer %in% "state" & !is.na(tmp4$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")]
      # Replacing coordinates at country level by those found at locality level due to missing state and counties
      tmp[tmp$resolution.gazetteer %in% "country" & !is.na(tmp4$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")] =
        tmp4[tmp$resolution.gazetteer %in% "country" & !is.na(tmp4$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")]
      tmp$resolution.gazetteer[tmp$resolution.gazetteer %in% c("localidade","localidade|sublocalidade","sublocalidade","distrito|vila","distrito","distrito|bairro","bairro","cachoeira","mina","vila","serra")] = "locality"
    }

  ## Assigning the "no_info" category for localities not found at the gazetteer
    tmp$resolution.gazetteer[is.na(tmp$resolution.gazetteer)] = "no_info"

  ## Merging the entry and output data
    result = cbind.data.frame(x, tmp)
    return(result)
}
