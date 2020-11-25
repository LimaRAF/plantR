#' @title Merge Information Within Duplicates
#'
#' @description This function homogenize the information of different groups of
#'   columns (taxonomic, geographic or locality) for groups of duplicate specimens.
#'
#' @param dups the input data frame.
#' @param prop numerical. The threshold value of proportion of duplicated values
#'   retrieved (i.e. dup.prop) to enter the merging routine. Default to 0.75.
#' @param info2merge character. The groups of information (i.e. columns) to be merged.
#'   Currently, only taxonomic ('tax'), geographic ('geo') and/or locality ('loc')
#'   columns can be merged. Default to all of them.
#' @param tax.level character. A vector with the confidence level of the
#'   identification that should be considered in the merge of taxonomic
#'   information. Default to "high".
#' @param overwrite logical. Should the merged information be overwritten or
#'   stored in separate, new columns. Default to FALSE (new columns are
#'   created).
#'
#' @author Renato A. F. de Lima
#'
#' @details The input data frame \code{dups} must contain the typicall columns
#'   resulting from __plantR__ workflow and functions. The characters that
#'   aggregate the occurrences into duplicates must be named 'dup.ID'. Depending
#'   on the type of merge desired (argument `info2merge`), a different set o
#'   clumns names are needed.
#'
#'  For the merge of taxonomic, geographic and locality information, the merging
#'  process consists in ordering the the information available for each group of
#'  duplicates from the best to the worst quality/resolution available.
#'
#'  For the merge of taxonomical information, the specimen(s) with the highest
#'  confidence level of the identification is(are) used as the standard(s), from
#'  which the taxonomic information is expanded to other specimens within the
#'  same group of duplicates. By default, `mergeDup` uses specimens flagged as
#'  having a 'high' taxonomic confidence level.
#'
#'  In the case of conflicting species identification among specialists for the
#'  same group of duplicates, the most recent identification is assumed as the
#'  most up-to-date one. Note that if the year of identification is missing from
#'  one or more duplicate labels, the corresponding identifications are not
#'  taken into account while trying to assign the most up-to-date identification
#'  for a group of duplicates.
#'
#'  For the merge of geographical information, specimens are ordered according
#'  to the result of their geographical validation (i.e. column 'geo.check') and
#'  the resolutions of the original geographical coordinates. Thus, for each
#'  group of duplicates the specimen whose coordinates were validated at the
#'  highest level (e.g. at county level) is used as the standard to expand
#'  the information for the specimens validate at lower levels (e.g. state or
#'  country levels).
#'
#'  A similar procedure is performed to expand the information regarding the locality
#'  description. Specimens are ordered according to the result of their locality
#'  validation (i.e. column 'loc.check'), and the one ranked best within the
#'  group of duplicates is the one used as the standard.
#'
#'  The columns necessary for the merging processes are the typical outputs from
#'  the __plantR__ workflow. They are different for the merge of each group of
#'  information:
#'
#'  + Taxonomic: family.new, scientificName/scientificName.new, identifiedBy.new,
#'  yearIdentified.new, tax.check
#'
#'  + Geographic: decimalLatitude.new, decimalLongitude.new, origin.coord,
#'  resolution.coord, geo.check
#'
#'  + Locality: loc.correct, loc.check
#'
#'  The merge of collector information (i.e. collector name, number and year) are
#'  predicted but not yet implemented in the current version.
#'
#' @return If `overwrite == FALSE`, the function returns the input data frame
#'   \code{dups} and the new columns containing the homogeneized information. The
#'   names of thse columns are the same of the previous one but with an added
#'   suffix '1'. If `overwrite == TRUE`, the homogeneized information is saved
#'   on the same columns of the input data and the names of the columns ramin
#'   the same.
#'
#' @import data.table
#'
#' @export mergeDup
#'
mergeDup <- function(dups, prop = 0.75, info2merge = c("tax", "geo", "loc"),
                     tax.name = "scientificName.new",
                     tax.level = "high", overwrite = FALSE, ...) {

  ## check input
  if (!class(dups) == "data.frame")
    stop("input object needs to be a data frame!")

  #Escaping R CMD check notes from using data.table syntax
  dup.prop <- dup.ID <- loc.correct <- NULL
  family.new <- identifiedBy.new <- yearIdentified.new <- NULL
  scientificName.new <- identifiedBy.new1 <- yearIdentified.new1 <- NULL
  tax.check <- tax.check1 <- geo.check <- NULL
  decimalLatitude.new <- decimalLongitude.new <- NULL
  resolution.coord <- resol.orig <- origin.coord <- NULL
  dup.merge <- work.tax.name <- same_spp <- loc.check <- NULL
  valor <- valor1 <- i.valor <- valor2 <- prioridade <- NULL

  #Checking essential columns
  if(!"dup.ID" %in% names(dups))
    stop(paste0("Merge is only possible if the input data frame contain a columns named 'dup.ID'"))

  tax.names <- c("family.new", tax.name, "identifiedBy.new", "yearIdentified.new", "tax.check")
  if("tax" %in% info2merge & !all(tax.names %in% names(dups)))
    stop(paste0("To perform the merge of taxonomic info, the input data frame must contain the following columns: ",
                paste0(tax.names, collapse = ", ")))

  geo.names <- c("decimalLatitude.new", "decimalLongitude.new", "origin.coord", "resolution.coord", "geo.check")
  if("geo" %in% info2merge & !all(geo.names %in% names(dups)))
    stop(paste0("To perform the merge of geographic info, the input data frame must contain the following columns: ",
                paste0(geo.names, collapse = ", ")))

  loc.names <- c('loc.correct','loc.check')
  if("loc" %in% info2merge & !all(loc.names %in% names(dups)))
    stop(paste0("To perform the merge of locality info, the input data frame must contain the following columns: ",
                paste0(loc.names, collapse = ", ")))

  #Vector to keep the original data order
  dups$ordem <- 1:dim(dups)[1]

  #Subsetting the data with any indication of duplicates
  dups1 <- dups[!is.na(dups$dup.ID),]
  dups1$dup.ID <- as.factor(dups1$dup.ID)
  dt <- data.table::data.table(dups1) # transforming the data frame into a data.table
  data.table::setkeyv(dt, "dup.ID") # setting 'dup.ID' as key to the data.table (makes computations faster)

  #Creating the duplicate categories
  dt[, dup.merge := dup.prop > prop] # creating the new columns for taxonomic check

  # if ("col" %in% info2merge) {
  #### PENSAR EM COMO IMPLEMENTAR ####
  # }

  if ("tax" %in% info2merge) {
    #Defining the column with the taxa names
    a <- which(names(dt) %in% tax.name)
    dt[, work.tax.name := dt[, a, with = FALSE], ]

    ### FINDING THE MOST RECENT TAXONOMICALLY VALIDATE NAME FOR EACH DUPLICATED ID ###
    ## Are all taxonomically-validated species names equal within a group?
    dt[, same_spp := "vazio"] # creating the new columns for taxonomic check
    #All names for taxonomically-validated specimens are the same? Then, mark as 'yes'
    dt[dup.merge & tax.check %in% tax.level,
       same_spp := if (uniqueN(work.tax.name) == 1) "sim"
       else same_spp, by = dup.ID]
    dt[, same_spp := if (any(same_spp == "sim") &
                         uniqueN(work.tax.name) == 1) "sim"
       else same_spp, by = dup.ID]
    dt[, same_spp := if (any(same_spp == "sim") &
                         uniqueN(work.tax.name) > 1) "no"
       else same_spp, by = dup.ID]
    #Not all names for taxonomically-validated specimens are the same? Then, mark as 'no'
    dt[dup.merge & tax.check %in% tax.level,
       same_spp := if (uniqueN(work.tax.name) > 1) "no"
       else same_spp, by = dup.ID]
    dt[, same_spp := if (any(same_spp == "no")) "no"
       else same_spp, by = dup.ID]
    # table(dt$same_spp,dt$tax.check)

    ## Defining the most up-to-date species name for the case of >=1 taxonomically validated names (new column species.correct1)
    # creating the new columns for taxonomic check
    dt[, c("family.new1", "scientificName.new1", "identifiedBy.new1", "yearIdentified.new1", "tax.check1") :=
         list(family.new, work.tax.name, identifiedBy.new, yearIdentified.new, tax.check)]
    #Saving for each duplicate group ID, the most recent identification, in the case of disagreement between the identification of taxonomically-validated specimens
    dt[same_spp == "no",
       "family.new1" := family.new[tax.check %in% tax.level][which.max(yearIdentified.new[tax.check %in% tax.level])], by = dup.ID]
    dt[same_spp == "no",
       "scientificName.new1" := work.tax.name[tax.check %in% tax.level][which.max(yearIdentified.new[tax.check %in% tax.level])], by = dup.ID]
    dt[same_spp == "no",
       "identifiedBy.new1" := identifiedBy.new[tax.check %in% tax.level][which.max(yearIdentified.new[tax.check %in% tax.level])], by = dup.ID]
    dt[same_spp == "no",
       "yearIdentified.new1" := yearIdentified.new[tax.check %in% tax.level][which.max(yearIdentified.new[tax.check %in% tax.level])], by = dup.ID]
    dt[same_spp == "no",
       "tax.check1" := tax.check[tax.check %in% tax.level][which.max(yearIdentified.new[tax.check %in% tax.level])], by = dup.ID]
    dt[same_spp == "sim",
       identifiedBy.new1 := identifiedBy.new[tax.check %in% tax.level &
                                               which.max(yearIdentified.new)][1], by = dup.ID]
    dt[same_spp == "sim",
       yearIdentified.new1 := yearIdentified.new[tax.check %in% tax.level &
                                                   which.max(yearIdentified.new)][1], by = dup.ID]
    dt[same_spp == "sim",
       tax.check1 := if (any(tax.check1 %in% tax.level)) tax.level
       else tax.check1, by = dup.ID]
    # table(dt$tax.check); table(dt$tax.check1)

    #Removing unecessary columns
    dt[, c("same_spp","work.tax.name") := NULL]

  }

  if ("geo" %in% info2merge) {

    # replacing NAs
    dt[, geo.check := ifelse(is.na(geo.check), "no_cannot_check", geo.check), ]
    cols <- c('resolution.coord','decimalLatitude.new','decimalLongitude.new')
    dt[, (cols) := replace(.SD, is.na(.SD), "no_coord"), .SDcols = cols]

    # creating the new columns for the taxonomic check and merge
    cols <- c("decimalLatitude.new1","decimalLongitude.new1","origin.coord1","resolution.coord1","geo.check1")
    dt[, (cols) :=
         list(decimalLatitude.new,decimalLongitude.new,origin.coord,resolution.coord,geo.check)]

    # creating the order/priority for geo info replacement
    geo.check.lvs <- data.table::data.table(
      geo.check = c("ok_county", "ok_county_close","ok_locality_gazet","ok_county_gazet",
                    "ok_state", "ok_state_gazet", "ok_country", "ok_country_gazet", "no_cannot_check"),
      valor = c(1,3,4,5,8,9,10,11,20))
    coord.res.lvs <- data.table::data.table(
      resolution.coord = c("seconds", "seconds?","seconds_centroid",
                           "minutes_only", "degrees_only", "no_coord"),
      valor = c(0,0,0,1,6,20))

    # add values of geo.check and coord. resolution to the original data
    data.table::setDT(dt)[geo.check.lvs, valor1 := i.valor, on = c(geo.check = "geo.check")]
    data.table::setDT(dt)[coord.res.lvs, valor2 := i.valor, on = c(resolution.coord = "resolution.coord")]

    # creating the priority index and re-organizing the original data
    dt[,prioridade:= valor1 + valor2]
    dt[, c("valor1", "valor2") := NULL]
    data.table::setkey(dt, dup.ID, prioridade)

    #Replacing the columns by the first row by groups of duplicate
    dt[dup.merge == TRUE, (cols) := .SD[1L], by = dup.ID, .SDcols = cols]

    #Removing unecessary columns
    dt[, "prioridade" := NULL]
  }

  if ("loc" %in% info2merge) {

    # removing NAs and creating the new columns for locality and geographical check
    cols <- c('loc.correct','loc.check')
    dt[, (cols) := replace(.SD, is.na(.SD), "no_loc"), .SDcols = cols]
    cols <- paste0(cols, "1")
    dt[, (cols) := list(loc.correct, loc.check)]

    # creating the order/priority for geo info replacement
    loc.check.lvs <- data.table::data.table(
      loc.check = c("check_local.2no.info","check_local.2country","check_local.2state","check_local.2municip.",
                    "check_municip.2no.info","check_municip.2country","check_municip.2state",
                    "check_state2no.info","check_state2country",
                    "check_country2no.info",
                    "ok_country2state","ok_country2municip.","ok_country2locality",
                    "ok_state2municip.","ok_state2locality",
                    "ok_municip.2locality",
                    "ok_same_resolution"),
      valor = c(5,4,3,2,5,4,3,5,4,5,3,2,1,2,1,1,NA_integer_))

    # add values of geo.check and coord. resolution to the original data
    data.table::setDT(dt)[loc.check.lvs, valor := i.valor, on = c(loc.check = "loc.check")]
    dt[is.na(valor) & resol.orig %in% "no_info", valor := 5]
    dt[is.na(valor) & resol.orig %in% "country", valor := 4]
    dt[is.na(valor) & resol.orig %in% "stateProvince", valor := 3]
    dt[is.na(valor) & resol.orig %in% "municipality", valor := 2]
    dt[is.na(valor) & resol.orig %in% "locality", valor := 1]

    # re-organizing the original data
    data.table::setkey(dt, dup.ID, valor)

    #Replacing the columns by the first row by groups of duplicate
    ## include an extra step: paste(unique(loc.correct[which.min(valor)][1]), collapse="|")??
    dt[dup.merge == TRUE, (cols) := .SD[1L], by = dup.ID, .SDcols = cols]

    #Removing unecessary columns
    dt[, "valor" := NULL]
  }

  if (overwrite) {
    dt[, c("dup.merge") := NULL]
    setkeyv(dt, c("ordem"))

    ## Detecting the target columns to overwrite
    colunas <- names(dt)[!names(dt) %in% names(dups)]
    colunas0 <- gsub("1", "", colunas)

    # subsetting and editing the new columns
    dt <- data.table::data.table(dt[, .SD, .SDcols = colunas])
    names(dt) <- gsub("1", "", names(dt))
    dt[, decimalLatitude.new := as.double(decimalLatitude.new)]
    dt[, decimalLongitude.new := as.double(decimalLongitude.new)]

    # transforming the original data frame and overwritting the new columns
    dt1 <- data.table::data.table(dups)
    setkeyv(dt1, c("ordem"))
    dt1[!is.na(dup.ID), (colunas0) := dt]

  } else {

    dt[, c("dup.merge") := NULL]
    ## Creating the missing new columns in the unicate subset of the data
    colunas <- names(dt)[!names(dt) %in% names(dups)]
    colunas0 <- unique(gsub("1", "", colunas))
    if(tax.name != colunas0[grepl("scientificName", colunas0)])
      colunas0[grepl("scientificName", colunas0)] <- tax.name

    # data with no indication of duplicates
    dt0 <- data.table::data.table(dups[is.na(dups$dup.ID), ])
    dt0[, (colunas) := .SD, .SDcols = colunas0]
    # merging the unicate and duplicated data tables
    dt1 <- rbindlist(list(dt0, dt))
    # re-ordering the resulting data.table to the input order
    setkeyv(dt1, c("ordem"))
  }

  #Removing unecessary columns: ordem
  dt1[, c("ordem") := NULL]
  dups1 <- data.frame(dt1)
  return(dups1)
}
