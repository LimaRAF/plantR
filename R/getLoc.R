#' @title Get Locality and Coordinates
#'
#' @description This function uses locality strings to search for existing
#'   localities and their respective coordinates in a \href{https://en.wikipedia.org/wiki/Gazetteer}{gazetteer}, which can be
#'   used to replace missing coordinates and in the validation process of the
#'   locality information and geographical coordinates provided.
#'
#' @param x a data.frame containing the strings for locality search. See details
#'   for the specifications of this data frame.
#' @param gazet a data.frame containing the gazetteer. The default is "plantR",
#'   the internal `plantR` gazetteer (biased towards Latin America)
#'
#' @return The data frame \code{x}, with the new columns retrieved from the
#'   gazetteer. More specifically, it returns the string used for the search in
#'   the gazetteer (column 'loc'), the string retrieved (if any, column
#'   'loc.correct'), the geographical coordinates (in decimal degrees) and the
#'   resolution associated with the string retrieved (columns
#'   'latitude.gazetteer', 'longitude.gazetteer', and 'resolution.gazetteer',
#'   respectively). and the associated resolution.
#'
#' @details The function was initially designed as part of a larger routine to
#'   edit and validate locality information from plant occurrence data. It is
#'   possible to use it separately, but it may be easier to use it under the
#'   workflow presented in `plantR` manual. If used separately, users must
#'   provide a data frame with at least two columns ('resol.orig' and
#'   'loc.string'). Other locality strings ('loc.string1' and 'loc.string2') may
#'   also be provided and in this case, these additional strings are used to
#'   search for information below the municipality/county level, that is, to
#'   retrieve from the gazetteer information at the locality level or below. See
#'   the examples below.
#'
#'   A different gazetteer than the `plantR` default can be used. This gazetteer
#'   must be provided using the argument `gazet` and it must contain the
#'   columns 'loc' (search string), 'loc.correct' (correct string),
#'   'latitude.gazetteer', 'longitude.gazetteer' (in decimal degrees) and
#'   'resolution.gazetteer' (e.g. country, state, etc).
#'
#'   It is important to stress that the retrieval of locality information
#'   depends on the completeness of the gazetteer itself. So, if a query does
#'   not find a "valid" locality, it does not necessarily mean that the locality
#'   does not exist or that its notation is wrong. It can simply mean that the
#'   gazetteer is incomplete for the region you are working with. The gazetteer
#'   is permanently being improved. If you find an error or if you want to
#'   contribute with region-specific gazetteers, please send an email to
#'   <renato.lima@naturalis.nl>.
#'
#' @author Renato A. F. de Lima
#'
#' @importFrom dplyr left_join
#' @importFrom stringr str_count
#'
#' @export getLoc
#'
#' @examples
#'
#' ## Using the function separately (need to provide a data in a specific format)
#' # Creating a data frame with locality information
#' (loc.str <- data.frame(resol.orig = c("municipality","locality"),
#'                   loc.string = c("brazil_rio janeiro_parati","brazil_rio janeiro_paraty"),
#'                   loc.string1 = c(NA, "brazil_rio janeiro_paraty_paraty mirim"),
#'                   stringsAsFactors = FALSE))
#' # Making the query of the strings in the gazetter
#' getLoc(loc.str)
#'
#'
#' ## Using the function under the `plantR` cleaning workflow (may be easier
#' # than using the function separatedely)
#'
#' # Creating a data frame with locality information
#' (occs <- data.frame(country = c("BR", "Brazil", "Brasil", "USA"),
#'                      stateProvince = c("RJ", "Rio de Janeiro", "Rio de Janeiro","Florida"),
#'                      municipality = c("Paraty", "Paraty", "Parati", NA),
#'                      locality = c(NA,"Paraty-Mirim", NA, NA),
#'                      stringsAsFactors = FALSE
#'                      ))
#'
#' # Formating the locality information
#' occs.fix <- fixLoc(occs)
#'
#' # Creating locality strings used to query the gazetteer
#' occs.locs <- strLoc(occs.fix)
#'
#' # Final editing the locality strings (reduces variation in locality notation)
#' occs.locs$loc.string <- prepLoc(occs.locs$loc.string)
#' occs.locs$loc.string1 <- prepLoc(occs.locs$loc.string1)
#' occs.locs$loc.string2 <- prepLoc(occs.locs$loc.string2)
#'
#' # Making the query of the edited strings in the gazetter
#' getLoc(occs.locs)
#'
#'
getLoc <- function(x, gazet = "plantR", ...) {

  ## check input:
  if (!class(x) == "data.frame")
    stop("input object needs to be a data frame!")

  if (!any(grepl("loc.string", names(x))))
    stop("input object needs to have at least one locality string: loc.string, loc.string1, loc.string2")

  ## putting the input data in the right order
  cls.nms <- c("resol.orig","loc.string","loc.string1","loc.string2")
  cls.nms <- cls.nms[cls.nms %in% colnames(x)]
  x1 <- x[which(colnames(x) %in% cls.nms)]

  ## Getting coordinates from the gazetteer - county level or lower (provided in the occuurrence labels)
  if(all(gazet %in% c("plantR","plantr"))) {
     dic <- gazetteer
  } else {

    if(!class(gazet) == "data.frame")
      stop("The gazetteer must be provided as a data frame")
    if(!all(c("family", "tdwg.name") %in% names(gazet)))
      stop("The gazetteer must contain at least the following columns: loc', 'loc.correct',
          'latitude.gazetteer' and 'longitude.gazetteer'")

    dic <- gazet
    if(!"resolution.gazetteer" %in% names(dic))
      dic$resolution.gazetteer = NA
  }

  # merging the occurrence data with the gazetteer information
  tmp <- dplyr::left_join(data.frame(loc = x1[,2], stringsAsFactors= FALSE),
                          dic[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")],
                          by = "loc")
  # Downgrading the locality resolution for the localities not found in the gazetteer
  tmp1 <- tmp$loc[!is.na(tmp$loc) & is.na(tmp$loc.correct)]
  if(length(tmp1)>0) {
    if (any(stringr::str_count(tmp1, "_") == 1))
      tmp1[stringr::str_count(tmp1, "_") == 1] <-
        sapply(strsplit(tmp1[stringr::str_count(tmp1, "_") == 1], "_"), function(x) x[1])
    if (any(stringr::str_count(tmp1, "_") == 2))
      tmp1[stringr::str_count(tmp1, "_") == 2] <-
        sapply(strsplit(tmp1[stringr::str_count(tmp1, "_") == 2], "_"), function(x) paste(x[1], x[2], sep = "_"))

    tmp2 <- dplyr::left_join(data.frame(loc=tmp1, stringsAsFactors= FALSE),
                             dic[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")],
                             by = "loc")

    #if nothing is found in the gazetteer, we finally try at country level:
    tmp3 <- tmp2$loc[is.na(tmp2$loc.correct)]
    if (length(tmp3) > 0) {
      tmp3 <- sapply(strsplit(tmp3,"_"), function(x) x[1])
      tmp3 <- dplyr::left_join(data.frame(loc=tmp3, stringsAsFactors= FALSE),
                               dic[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")],
                               by = "loc")
      tmp2[is.na(tmp2$loc.correct), c("loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")] =
        tmp3[ ,c("loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")]
    }
    #saving all dowgraded locality infor from the gazetteer
    tmp[!is.na(tmp$loc) & is.na(tmp$loc.correct), c("loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")] =
      tmp2[ ,c("loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")]
  }

  ## Getting coordinates from the gazetteer - locality level
  if("loc.string1" %in% names(x1)) {
    dic1 <- dic[dic$resolution.gazetteer %in% c("localidade","localidade|sublocalidade","sublocalidade","distrito|vila","distrito","distrito|bairro","bairro","cachoeira","mina","vila","serra"),]
    tmp3 <- dplyr::left_join(data.frame(loc=x1[,3], stringsAsFactors= FALSE),
                             dic1[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")],
                             by = "loc")
    # Replacing coordinates at county level by those found at locality level
    tmp[tmp$resolution.gazetteer %in% "county" & !is.na(tmp3$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")] <-
      tmp3[tmp$resolution.gazetteer %in% "county" & !is.na(tmp3$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")]

    # Replacing coordinates at state level by those found at locality level (rare but can happen due to missing counties)
    tmp[tmp$resolution.gazetteer %in% "state" & !is.na(tmp3$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")] <-
      tmp3[tmp$resolution.gazetteer %in% "state" & !is.na(tmp3$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")]
    # Replacing coordinates at country level by those found at locality level due to missing state and counties
    tmp[tmp$resolution.gazetteer %in% "country" & !is.na(tmp3$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")] <-
      tmp3[tmp$resolution.gazetteer %in% "country" & !is.na(tmp3$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")]
    tmp$resolution.gazetteer[tmp$resolution.gazetteer %in% c("localidade","localidade|sublocalidade","sublocalidade","distrito|vila","distrito","distrito|bairro","bairro","cachoeira","mina","vila","serra")] = "locality"
  }

  ## getting coordinates from the gazetteer - county level extracted from the locality (not 100% sure? needs validation...)
  if("loc.string2" %in% names(x1)) {
    tmp4 <- dplyr::left_join(data.frame(loc = x1[,4], stringsAsFactors= FALSE),
                             dic[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")],
                             by = "loc")
    # replacing coordinates at state level by those found at county level
    tmp[tmp$resolution.gazetteer %in% "state" & !is.na(tmp4$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")] <-
      tmp4[tmp$resolution.gazetteer %in% "state" & !is.na(tmp4$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")]
    # Replacing coordinates at country level by those found at locality level due to missing state and counties
    tmp[tmp$resolution.gazetteer %in% "country" & !is.na(tmp4$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")] <-
      tmp4[tmp$resolution.gazetteer %in% "country" & !is.na(tmp4$resolution.gazetteer), c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")]
    tmp$resolution.gazetteer[tmp$resolution.gazetteer %in% c("localidade","localidade|sublocalidade","sublocalidade","distrito|vila","distrito","distrito|bairro","bairro","cachoeira","mina","vila","serra")] = "locality"
  }

  ## Assigning the "no_info" category for localities not found at the gazetteer
  tmp$resolution.gazetteer[is.na(tmp$resolution.gazetteer)] <- "no_info"

  ## Merging the entry and output data
  result <- cbind.data.frame(x, tmp)
  return(result)
}
