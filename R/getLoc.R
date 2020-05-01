#' @title Get Locality and Coordinates
#'
#' @description Create the locality string used to search for coordinates in a gazetteer. This information is
#' used to obtain missing coordinates based on locality descriptions and in the process of validation of the
#' geographical coordinates.
#'
#' @param x data frame with the columns: "country.new","stateProvince.new","municipality.new","locality.new"
#'
getLoc = function(x, gazet = "plantR") {
  require(dplyr)

  ## check input:
    if (!class(x) == "data.frame") { stop("input object needs to be a data frame!") }
  if (!any(c("country.new","stateProvince.new","municipality.new") %in% colnames(x))) { stop("input object needs to have at least the following fields: country.new, stateProvince.new, municipality.new") }

  ## putting the input data in the right order
  x1 = x[which(colnames(x) %in% c("country.new","stateProvince.new","municipality.new","locality.new","locality.scrap","resol.orig"))]

  ## Defining a unique code for each county, state/province or county/commune ##
  loc = rep(NA, dim(x1)[1])
  # county-level
  loc[!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3])] =
    paste(
      x1[1][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3])],
      x1[2][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3])],
      x1[3][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3])],
      sep="_")
  # state-level
  loc[is.na(loc)&!is.na(x1[1])&!is.na(x1[2])] =
    paste(
      x1[1][is.na(loc) & !is.na(x1[1]) & !is.na(x1[2])],
      x1[2][is.na(loc) & !is.na(x1[1]) & !is.na(x1[2])],
      sep="_")
  # country-level
  loc[is.na(loc) & !is.na(x1[1])] =
    x1[1][is.na(loc) & !is.na(x1[1])]

  ## Definig a unique code for each locality (if available)
  if("locality.new" %in% names(x1)) {
    loc1 = rep(NA, dim(x1)[1])
    #locality-level
    loc1[!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[3]) & !is.na(x1[4])] =
      paste(x1[1][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[4])],
            x1[2][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[4])],
            x1[3][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[4])],
            x1[4][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[4])],
            sep="_")

    # county-level, but missing stateProvince
    loc1[is.na(loc1) & !is.na(x1[1]) & is.na(x1[2]) & !is.na(x1[3])] =
      paste(
        x1[1][is.na(loc1) & !is.na(x1[1]) & is.na(x1[2]) & !is.na(x1[3])],
        x1[2][is.na(loc1) & !is.na(x1[1]) & is.na(x1[2]) & !is.na(x1[3])],
        x1[3][is.na(loc1) & !is.na(x1[1]) & is.na(x1[2]) & !is.na(x1[3])],
        sep="_")
  }

  ## Definig a unique code for an alternative way of getting missing counties from the locality field
  if("locality.scrap" %in% names(x1)) {
    loc2 = rep(NA, dim(x1)[1])
    #county-level when county is not given
    loc2[!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[5])] =
      paste(
        x1[1][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[5])],
        x1[2][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[5])],
        x1[5][!is.na(x1[1]) & !is.na(x1[2]) & !is.na(x1[3]) & !is.na(x1[5])],
        sep="_")
    #locality-level, but missing stateProvince and county
    loc2[is.na(loc2) & !is.na(x1[4]) & x1$resol.orig %in% "country"] =
      paste(
        x1[1][is.na(loc2) & !is.na(x1[4]) & x1$resol.orig %in% "country"],
        NA, NA,
        x1[4][is.na(loc2) & !is.na(x1[4]) & x1$resol.orig %in% "country"],
        sep="_")
  }

  ## Final edits to simplify the coordinate cross-checking process
  loc  = prepLoc(loc)
  if("locality.new" %in% names(x1)) loc1 = prepLoc(loc1)
  if("locality.scrap" %in% names(x1)) loc2 = prepLoc(loc2)

  ## Getting coordinates from the gazetteer - county level or lower (provided in the occuurrence labels)
  if(all(gazet %in% c("plantR","plantr"))) {
    load("./R/sysdata.rda")
    dic = gazetteer
  } else {
    dic = gazet
    if(!"resolution.gazetteer" %in% names(dic)) dic$resolution.gazetteer = NA
  }

  # merging the occurrence data with the gazetteer information
  tmp = left_join(data.frame(loc=loc, stringsAsFactors= FALSE),
                  dic[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")],
                  by="loc")
  # Downgrading the locality resolution for the localities not found in the gazetteer
  tmp1 = tmp$loc[!is.na(tmp$loc) & is.na(tmp$loc.correct)]
  if(length(tmp1)>0) {
    if(any(str_count(tmp1,"_")==1)) tmp1[str_count(tmp1,"_")==1] = sapply(strsplit(tmp1[str_count(tmp1,"_")==1],"_"), function(x) x[1])
    if(any(str_count(tmp1,"_")==2)) tmp1[str_count(tmp1,"_")==2] = sapply(strsplit(tmp1[str_count(tmp1,"_")==2],"_"), function(x) paste(x[1],x[2],sep="_"))
    tmp2 = left_join(data.frame(loc=tmp1, stringsAsFactors= FALSE),
                     dic[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")],
                     by="loc")
    #if nothing is found in the gazetteer, we finally try at country level:
    tmp3 = tmp2$loc[is.na(tmp2$loc.correct)]
    tmp3 = sapply(strsplit(tmp3,"_"), function(x) x[1])
    tmp3 = left_join(data.frame(loc=tmp3, stringsAsFactors= FALSE),
                     dic[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")],
                     by="loc")
    tmp2[is.na(tmp2$loc.correct), c("loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")] =
      tmp3[ ,c("loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")]
    #saving all dowgraded locality infor from the gazetteer
    tmp[!is.na(tmp$loc) & is.na(tmp$loc.correct), c("loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")] =
      tmp2[ ,c("loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")]
  } else { 	}

  ## Getting coordinates from the gazetteer - locality level
    dic1 = dic[dic$resolution.gazetteer %in% c("localidade","localidade|sublocalidade","sublocalidade","distrito|vila","distrito","distrito|bairro","bairro","cachoeira","mina","vila","serra"),]
    tmp3 = left_join(data.frame(loc=loc1, stringsAsFactors= FALSE),
                     dic1[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")],
                     by="loc")
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
    tmp4 = left_join(data.frame(loc=loc2, stringsAsFactors= FALSE),
                     dic[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")],
                     by="loc")
    # replacing coordinates at state level by those found at county level
    tmp[tmp$resolution.gazetteer %in% "state" & !is.na(tmp4$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")] =
      tmp4[tmp$resolution.gazetteer %in% "state" & !is.na(tmp4$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")]
    # Replacing coordinates at country level by those found at locality level due to missing state and counties
    tmp[tmp$resolution.gazetteer %in% "country" & !is.na(tmp4$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")] =
      tmp4[tmp$resolution.gazetteer %in% "country" & !is.na(tmp4$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")]
    tmp$resolution.gazetteer[tmp$resolution.gazetteer %in% c("localidade","localidade|sublocalidade","sublocalidade","distrito|vila","distrito","distrito|bairro","bairro","cachoeira","mina","vila","serra")] = "locality"

  ## Merging the entry and output data
    x2 = cbind.data.frame(x1, tmp[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")])

  ## Flagging localities with changes in their resolution
    resol.gazet = gsub("state", "stateProvince", x2$resolution.gazetteer)
    resol.gazet = gsub("county", "municipality", resol.gazet)
    resol.gazet[is.na(resol.gazet)] = "no_info"
    x2$loc.check = resol.gazet == x2$resol.orig
    # OK: change to a better resolution
    x2$loc.check[x2$loc.check == FALSE &
                   resol.gazet %in% "locality" &
                   x2$resol.orig %in% "country"] = "ok_country2locality"
    x2$loc.check[x2$loc.check == FALSE &
                   resol.gazet %in% "municipality" &
                   x2$resol.orig %in% "country"] = "ok_country2municip."
    x2$loc.check[x2$loc.check == FALSE &
                   resol.gazet %in% "stateProvince" &
                   x2$resol.orig %in% "country"] = "ok_country2state"
    x2$loc.check[x2$loc.check == FALSE &
                   resol.gazet %in% "locality" &
                   x2$resol.orig %in% "stateProvince"] = "ok_state2locality"
    x2$loc.check[x2$loc.check == FALSE &
                   resol.gazet %in% "municipality" &
                   x2$resol.orig %in% "stateProvince"] = "ok_state2municip."
    x2$loc.check[x2$loc.check == FALSE &
                   resol.gazet %in% "locality" &
                   x2$resol.orig %in% "municipality"] = "ok_municip.2locality"
    # Problems (check): locality info is given but was not found in the gazetteer
    x2$loc.check[x2$loc.check == FALSE &
                  resol.gazet %in% "no_info" &
                  x2$resol.orig %in% "country"] = "check_country2no.info"
    x2$loc.check[x2$loc.check == FALSE &
                   resol.gazet %in% "no_info" &
                   x2$resol.orig %in% "stateProvince"] = "check_state2no.info"
    x2$loc.check[x2$loc.check == FALSE &
                   resol.gazet %in% "no_info" &
                   x2$resol.orig %in% "municipality"] = "check_municip.2no.info"
    x2$loc.check[x2$loc.check == FALSE &
                   resol.gazet %in% "no_info" &
                   x2$resol.orig %in% "locality"] = "check_local.2no.info"
    x2$loc.check[x2$loc.check == FALSE &
                   resol.gazet %in% "country" &
                   x2$resol.orig %in% "locality"] = "check_local.2country"
    x2$loc.check[x2$loc.check == FALSE &
                   resol.gazet %in% "stateProvince" &
                   x2$resol.orig %in% "locality"] = "check_local.2state"
    x2$loc.check[x2$loc.check == FALSE &
                   resol.gazet %in% "municipality" &
                   x2$resol.orig %in% "locality"] = "check_local.2municip."
    x2$loc.check[x2$loc.check == FALSE &
                   resol.gazet %in% "country" &
                   x2$resol.orig %in% "municipality"] = "check_municip.2country"
    x2$loc.check[x2$loc.check == FALSE &
                   resol.gazet %in% "stateProvince" &
                   x2$resol.orig %in% "municipality"] = "check_municip.2state"
    x2$loc.check[x2$loc.check == FALSE &
                   resol.gazet %in% "country" &
                   x2$resol.orig %in% "stateProvince"] = "check_state2country"
    # No changes (ok_same) and inofrmation not found even at country level (no_info)
    x2$loc.check[x2$loc.check == TRUE] = "ok_same"
    x2$loc.check[is.na(x2$loc.check)] = "no_info"


    ## Reporting and returning
    print("Resolution of the localities in the original data vs. the edited data:")
    print(table(edited = resol.gazet, original = x2$resol.orig))
    return(x2)
}
