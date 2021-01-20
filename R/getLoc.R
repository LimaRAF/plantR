#' @title Get Locality and Coordinates
#'
#' @description This function uses the __plantR__ locality strings to search for
#'   existing localities and their respective coordinates in a
#'   \href{https://en.wikipedia.org/wiki/Gazetteer}{gazetteer}, which can be
#'   used to replace missing coordinates and in the validation process of the
#'   locality information and geographical coordinates provided.
#'
#' @param x a data.frame containing the strings for locality search. See details
#'   for the specifications of this data frame.
#' @param str.names a vector of at least two columns names containing the
#'   locality resolution and search string(s), in that order. Defaults to
#'   'resol.orig', 'loc.string', 'loc.string1' and 'loc.string2'.
#' @param gazet a data.frame containing the gazetteer. The default is "plantR",
#'   the internal __plantR__ gazetteer (biased towards Latin America).
#' @param gazet.names a vector of at least four columns names containing the
#'   locality search string, latitude and longitude, in that order. If available,
#'   the resolution of the gazetteer can be provided as a fifth name. Defaults
#'   to columns names of the __plantR__ gazetteer: 'loc', 'loc.correct',
#'   'latitude.gazetteer', 'longitude.gazetteer' and 'resolution.gazetteer'.
#' @param orig.names logical. Should the original columns names of the gazetteer
#'   be preserved. Default to FALSE.
#'
#' @return The data frame \code{x}, with the new columns retrieved from the
#'   gazetteer. More specifically, it returns the string used for the search in
#'   the gazetteer (column 'loc'), the string retrieved (if any, column
#'   'loc.correct'), the geographical coordinates (in decimal degrees) and the
#'   resolution associated with the string retrieved (columns
#'   'latitude.gazetteer', 'longitude.gazetteer', and 'resolution.gazetteer',
#'   respectively) and the associated resolution.
#'
#' @details The function was initially designed as part of a larger routine to
#'   edit and validate locality information from plant occurrence data. It is
#'   possible to use it separately, but it may be easier to use it under the
#'   workflow presented in the __plantR__ manual. If used separately, users must
#'   provide a data frame with at least two columns ('resol.orig' and
#'   'loc.string'). Other locality strings ('loc.string1' and 'loc.string2') may
#'   also be provided and in this case, these additional strings are used to
#'   search for information below the municipality/county level, that is, to
#'   retrieve from the gazetteer information at the locality level or below. If
#'   these columns have different names in \code{x}, these names can be supplied using,
#'   the argument `str.names`. See the examples below.
#'
#'   The default __plantR__ gazetteer includes information for all countries at
#'   the country level (i.e. administrative level 0) and at the lowest
#'   administrative level available for all Latin at GDAM
#'   (\url{https://gadm.org}) for 51 Latin American countries. For Brazil, the
#'   gazetteer also contains information at the locality level (e.g. farms,
#'   forest fragments, parks), obtained from [IBGE](https://www.ibge.gov.br/),
#'   [CNCFlora](http://cncflora.jbrj.gov.br) and
#'   [TreeCo](http://labtrop.ib.usp.br/doku.php?id=projetos:treeco:start)
#'   databases. It also includes common spelling variants and historical changes
#'   to locality names (currently biased for Brazil) and more common notation
#'   variants of locality names found in the lcoality description of records
#'   from GBIF, speciesLink and JABOT databases (include few type localities).
#'   In total the gazetteer has nearly 25,000 locality names associated with a
#'   valid geographical coordinates.
#'
#'   A different gazetteer than the __plantR__ default can be used. This gazetteer
#'   must be provided using the argument `gazet` and it must contain the
#'   columns 'loc' (search string), 'loc.correct' (correct string),
#'   'latitude.gazetteer', 'longitude.gazetteer' (in decimal degrees) and
#'   'resolution.gazetteer' (e.g. country, state, etc). If the names for these
#'   columns are different, they can be supplied using argument ``
#'
#'   It is important to stress that the retrieval of locality information
#'   depends on the completeness of the gazetteer itself. So, if a query does
#'   not find a "valid" locality, it does not necessarily mean that the locality
#'   does not exist or that its notation is wrong. It can simply mean that the
#'   gazetteer is incomplete for the region you are working with. The gazetteer
#'   is permanently being improved. If you find an error or if you want to
#'   contribute with region-specific gazetteers, please send an email to
#'   <raflima@usp.br>.
#'
#' @author Renato A. F. de Lima
#'
#' @importFrom dplyr left_join
#' @importFrom stringr str_count
#'
#' @export getLoc
#'
#' @seealso
#'  \link[plantR]{fixLoc}, \link[plantR]{strLoc} and
#'  \link[plantR]{prepLoc}.
#'
#' @examples
#'
#' ## Using the function separately (need to provide column names and
#' #strings in an specific format)
#' (df <- data.frame(resol = c("municipality","locality"),
#'                   loc = c("brazil_rio janeiro_parati","brazil_rio janeiro_paraty"),
#'                   loc1 = c(NA, "brazil_rio janeiro_paraty_paraty mirim"),
#'                   stringsAsFactors = FALSE))
#' getLoc(df, str.names = c("resol", "loc", "loc1"))
#'
#' ## Using the function under the __plantR__ workflow
#' (df <- data.frame(country = c("BR", "Brazil", "Brasil", "USA"),
#'                      stateProvince = c("RJ", "Rio de Janeiro", "Rio de Janeiro","Florida"),
#'                      municipality = c("Paraty", "Paraty", "Parati", NA),
#'                      locality = c(NA,"Paraty-Mirim", NA, NA),
#'                      stringsAsFactors = FALSE))
#'
#' # Formating the locality information
#' occs.fix <- fixLoc(df)
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
getLoc <- function(x, str.names = c("resol.orig", "loc.string", "loc.string1", "loc.string2"),
                   gazet = "plantR", gazet.names = c("loc", "loc.correct", "latitude.gazetteer",
                                                     "longitude.gazetteer", "resolution.gazetteer"),
                   orig.names = FALSE, ...) {

  ## check input:
  if (!class(x) == "data.frame")
    stop("Input object needs to be a data frame!")

  ## Checking input data and columns names
  str.names <- str.names[str.names %in% colnames(x)]
  if (length(str.names) == 0)
    stop("Please provide valid names for the locality resolution and search string(s) in the input object")

  x1 <- x[match(str.names, colnames(x))]
  cls.nms <- c("resol.orig", "loc.string", "loc.string1", "loc.string2")
  for(i in 1:length(x1)) colnames(x1)[i] <- cls.nms[i]

  if (!any(grepl("loc.string", names(x1))))
    stop("Input object needs to have at least one locality string (e.g. loc.string)")


  ## Getting coordinates from the gazetteer - county level or lower (provided in the occuurrence labels)
  cols.gazet <- c("loc", "loc.correct", "latitude.gazetteer",
                  "longitude.gazetteer", "resolution.gazetteer")
  class.gazet <- class(gazet)
  dic <- NULL
  if (class.gazet == "character") {
    if (any(gazet %in% c("plantR", "plantr"))) {
     dic <- gazetteer
     dic <- dic[, cols.gazet]
    } else {
      stop("Please chose between the default gazetteer or provide one as a data frame")
    }
  }

  if(class.gazet == "data.frame") {
    if (all(gazet.names %in% colnames(gazet))) {
      dic <- gazet[match(gazet.names, colnames(gazet))]
      for(i in 1:length(dic)) colnames(dic)[i] <- cols.gazet[i]

      if (!"resolution.gazetteer" %in% names(dic))
        dic$resolution.gazetteer <- "no_res"

    } else {
      stop("The gazetteer must contain the columns speciefied in the argument `gazet.names`")
    }
  }

  if (is.null(dic))
    stop("Please chose between the default 'plantR' gazetteer or provide one as a data frame")

  # List of columns and categories to be used in the manipulation
  cols.repl <- c("loc.correct", "latitude.gazetteer",
                 "longitude.gazetteer", "resolution.gazetteer")
  resol.gazet <- c("localidade", "localidade|sublocalidade", "sublocalidade",
                   "distrito|vila", "distrito", "distrito|bairro", "bairro",
                   "cachoeira", "mina", "vila", "serra")

  # merging the occurrence data with the gazetteer information
  tmp <- dplyr::left_join(data.frame(loc = x1[,2], stringsAsFactors= FALSE),
                          dic, by = "loc")
  # Downgrading the locality resolution for the localities not found in the gazetteer
  tmp1 <- tmp$loc[!is.na(tmp$loc) & is.na(tmp$loc.correct)]
  if (length(tmp1) > 0) {
    if (any(stringr::str_count(tmp1, "_") == 1))
      tmp1[stringr::str_count(tmp1, "_") == 1] <-
        sapply(strsplit(tmp1[stringr::str_count(tmp1, "_") == 1], "_"), function(x) x[1])
    if (any(stringr::str_count(tmp1, "_") == 2))
      tmp1[stringr::str_count(tmp1, "_") == 2] <-
        sapply(strsplit(tmp1[stringr::str_count(tmp1, "_") == 2], "_"), function(x) paste(x[1], x[2], sep = "_"))

    tmp2 <- dplyr::left_join(data.frame(loc=tmp1, stringsAsFactors= FALSE),
                             dic, by = "loc")

    #if nothing is found in the gazetteer, we finally try at country level:
    tmp3 <- tmp2$loc[is.na(tmp2$loc.correct)]
    if (length(tmp3) > 0) {
      tmp3 <- sapply(strsplit(tmp3,"_"), function(x) x[1])
      tmp3 <- dplyr::left_join(data.frame(loc=tmp3, stringsAsFactors= FALSE),
                               dic, by = "loc")
      tmp2[is.na(tmp2$loc.correct), cols.repl] <-
        tmp3[ ,cols.repl]
    }
    #saving all dowgraded locality infor from the gazetteer
    tmp[!is.na(tmp$loc) & is.na(tmp$loc.correct), cols.repl] <-
      tmp2[ ,cols.repl]
  }

  ## Getting coordinates from the gazetteer - locality level
  if ("loc.string1" %in% names(x1)) {
    dic1 <- dic[dic$resolution.gazetteer %in% resol.gazet,]
    tmp3 <- dplyr::left_join(data.frame(loc=x1[,3], stringsAsFactors= FALSE),
                             dic1, by = "loc")
    # Replacing coordinates at county level by those found at locality level
    tmp[tmp$resolution.gazetteer %in% "county" & !is.na(tmp3$resolution.gazetteer), cols.gazet] <-
      tmp3[tmp$resolution.gazetteer %in% "county" & !is.na(tmp3$resolution.gazetteer), cols.gazet]

    # Replacing coordinates at state level by those found at locality level (rare but can happen due to missing counties)
    tmp[tmp$resolution.gazetteer %in% "state" & !is.na(tmp3$resolution.gazetteer), cols.gazet] <-
      tmp3[tmp$resolution.gazetteer %in% "state" & !is.na(tmp3$resolution.gazetteer), cols.gazet]
    # Replacing coordinates at country level by those found at locality level due to missing state and counties
    tmp[tmp$resolution.gazetteer %in% "country" & !is.na(tmp3$resolution.gazetteer), cols.gazet] <-
      tmp3[tmp$resolution.gazetteer %in% "country" & !is.na(tmp3$resolution.gazetteer), cols.gazet]
    tmp$resolution.gazetteer[tmp$resolution.gazetteer %in% resol.gazet] <- "locality"
  }

  ## getting coordinates from the gazetteer - county level extracted from the locality (not 100% sure? needs validation...)
  if ("loc.string2" %in% names(x1)) {
    tmp4 <- dplyr::left_join(data.frame(loc = x1[,4], stringsAsFactors = FALSE),
                             dic, by = "loc")
    # replacing coordinates at state level by those found at county level
    tmp[tmp$resolution.gazetteer %in% "state" & !is.na(tmp4$resolution.gazetteer), cols.gazet] <-
      tmp4[tmp$resolution.gazetteer %in% "state" & !is.na(tmp4$resolution.gazetteer), cols.gazet]
    # Replacing coordinates at country level by those found at locality level due to missing state and counties
    tmp[tmp$resolution.gazetteer %in% "country" & !is.na(tmp4$resolution.gazetteer), cols.gazet] <-
      tmp4[tmp$resolution.gazetteer %in% "country" & !is.na(tmp4$resolution.gazetteer), cols.gazet]
    tmp$resolution.gazetteer[tmp$resolution.gazetteer %in% resol.gazet] <- "locality"
  }

  ## Assigning the "no_info" category for localities not found at the gazetteer
  tmp$resolution.gazetteer[is.na(tmp$resolution.gazetteer)] <- "no_info"

  ## Should the original gazetteer names be preserved?
  if (orig.names)
    colnames(tmp) <- gazet.names

  ## Merging the entry and output data
  result <- cbind.data.frame(x, tmp, stringsAsFactors = FALSE)
  return(result)
}
