#' @title Merge Duplicate Information
#'
#' @description This function homogenize the information of different groups of
#'   fields (e.g. taxonomic, geographic or locality) for groups of duplicate specimens.
#'
#' @param dups the input data frame.
#' @param dup.name character. The name of column in the input data frame with
#'   the duplicate group ID. Default to the __plantR__ output 'dup.ID'.
#' @param prop.name character. The name of column in the input data frame with
#'   the proportion of duplicates found within the group ID. Default to the
#'   __plantR__ output 'dup.prop'.
#' @param prop numerical. The threshold value of proportion of duplicated values
#'   retrieved (i.e. dup.prop) to enter the merging routine. Should be between
#'   zero and one. Default to 0.75.
#' @param info2merge Vector. The groups of information (i.e. fields) to be
#'   merged. Currently, only taxonomic ('tax'), geographic ('geo') and/or
#'   locality ('loc') information can be merged. Default to merge all of them.
#' @param tax.names Vector. A named vector containing the names of columns in
#'   the input data frame with the taxonomic information to be merged.
#' @param geo.names Vector. A named vector containing the names of columns in
#'   the input data frame with the geographical information to be merged.
#' @param loc.names Vector. A named vector containing the names of columns in
#'   the input data frame with the locality information to be merged.
#' @param tax.level character. A vector with the confidence level of the
#'   identification that should be considered in the merge of taxonomic
#'   information. Default to "high".
#' @param overwrite logical. Should the merged information be overwritten or
#'   stored in separate, new columns. Default to FALSE (new columns are
#'   created).
#'
#' @author Renato A. F. de Lima
#'
#' @details
#'   The homogenization of the information within groups of duplicates depends
#'   on the existence of some fields in the input data frame, which result from
#'   the __plantR__ workflow. The first essential field is the duplicate group
#'   identifiers, which is used to aggregate the records (see functions
#'   `prepDup()` and `getDup()`). The name of the column with these identifiers
#'   must be provided to the argument `dup.name` (default to 'dup.ID'). Other
#'   essential fields depend on the type of merge desired (argument
#'   `info2merge`), a different set of columns names are needed. These names
#'   should be provided to the arguments `tax.names`, `geo.names`, and `loc.names`.
#'
#'   For the merge of taxonomical information, the fields required by
#'   `tax.names` are:
#'   - 'family': the botanical family (default: 'family.new')
#'   - 'species': the scientific name (default: 'scientificName.new')
#'   - 'det.name': the indentifier name (default: 'identifiedBy.new')
#'   - 'det.year': the identification year (default: 'yearIdentified.new')
#'   - 'tax.check': the confidence level of the taxonomic identification (default: 'tax.check')
#'
#'   For the merge of geographical information, the fields required by
#'   `geo.names` are:
#'   - 'lat': latitude in decimal degrees (default: 'decimalLatitude.new')
#'   - 'lon': longitude in decimal degrees (default: 'decimalLongitude.new')
#'   - 'org.coord': the origin of the coordinates (default: 'origin.coord')
#'   - 'prec.coord': the precision of the coordinates (default: 'precision.coord')
#'   - 'geo.check': the result of the geo. coordinate validation (default: 'geo.check')
#'
#'   For the merge of locality information, the fields required by `loc.names`
#'   are:
#'   - 'loc.str': the locality search string (default: 'loc.correct')
#'   - 'res.gazet': the resolution of the gazetteer coordinates (default: 'resolution.gazetteer')
#'   - 'res.orig': the resolution of the source coordinates (default: 'resol.orig')
#'   - 'loc.check': the result of the locality validation (default: 'loc.check')
#'
#'  For all groups of information (i.e. taxonomic, geographic and locality), the
#'  merging process consists in ordering the the information available for each
#'  group of duplicates from the best to the worst quality/resolution available.
#'  The best information available is then expanded for all records of the group
#'  of duplicates. The argument `prop` defines the duplicated proportion (given
#'  by `prop.name`) that should be used as a threshold. Only records with
#'  duplicated proportions above this threshold will be merged. For all other
#'  records, the output will be the same as the input. If no column `prop.name` is
#'  found in the input data, merge is performed for all records, with a warning.
#'
#'  For the merge of taxonomical information, the specimen(s) with the highest
#'  confidence level of the identification is used as the standard, from
#'  which the taxonomic information is expanded to other specimens within the
#'  same group of duplicates. By default, `mergeDup()` uses specimens flagged as
#'  having a 'high' confidence level.
#'
#'  In the case of conflicting species identification among specialists for the
#'  same group of duplicates, the most recent identification is assumed as the
#'  most up-to-date one. Note that if the year of identification is missing from
#'  one or more records, the corresponding identifications of these records are not
#'  taken into account while trying to assign the most up-to-date identification
#'  for a group of duplicates.
#'
#'  For the merge of geographical information, specimens are ordered according
#'  to the result of their geographical validation (i.e. field 'geo.check') and
#'  the resolutions of the original geographical coordinates. Thus, for each
#'  group of duplicates the specimen whose coordinates were validated at the
#'  best level (e.g. 'ok_county') is used to expand the information for the
#'  specimens validate at lower levels (e.g. state or country levels).
#'
#'  A similar procedure is performed to expand the information regarding the
#'  locality description. Specimens are ordered according to the result of their
#'  locality validation (i.e. field 'loc.check'), and the one ranked best
#'  within the group of duplicates (e.g. 'ok_municip.2locality') is the one used
#'  as the standard.
#'
#'  The merge of collector information (i.e. collector name, number and year) are
#'  predicted, but not yet implemented in the current version.
#'
#' @return If `overwrite == FALSE`, the function returns the input data frame
#'   \code{dups} and the new columns containing the homogeneized information. The
#'   names of these columns are the same of the previous one but with an added
#'   suffix '1'. If `overwrite == TRUE`, the homogeneized information is saved
#'   on the same columns of the input data and the names of the columns remain
#'   the same.
#'
#' @examples
#' #An example for the merg of taxonomic information only
#' (df = data.frame(
#'   ID = c("a7","b2","c4","d1","e9","f3","g2","h8","i6","j5"),
#'   dup.ID = c("a7|b2","a7|b2","c4|d1|e9","c4|d1|e9","c4|d1|e9",
#'              "f3|f2","f3|f2","h8|i6|j5","h8|i6|j5","h8|i6|j5"),
#'   fam = c("AA","AA","BB","BB","Bb","CC","DD","EE","Ee","ee"),
#'   sp = c("a a","a b","c c","c d","c d","e e","f f","h h","h h","h h"),
#'   det = c("spec","n_spec","spec1","spec2","n_spec1",
#'           "spec3","spec4","n_spec2","n_spec3","n_spec4"),
#'   yr = c("2010","2019","2019","2000","2020",NA,"1812","2020","2020","2020"),
#'   check = c("high","low","high","high","low","high","high","low","low","low")))
#'
#' mergeDup(df, info2merge = "tax",
#'         tax.names = c(family = "fam",
#'                       species = "sp",
#'                       det.name = "det",
#'                       det.year = "yr",
#'                       tax.check = "check"))
#'
#'
#' @seealso
#'  \link[plantR]{prepDup} and \link[plantR]{getDup}.
#'
#' @import data.table
#'
#' @export mergeDup
#'
#'
mergeDup <- function(dups, dup.name = "dup.ID", prop.name = "dup.prop", prop = 0.75,
                     info2merge = c("tax", "geo", "loc"),
                     tax.names = c(family = "family.new",
                                   species = "scientificName.new",
                                   det.name = "identifiedBy.new",
                                   det.year = "yearIdentified.new",
                                   tax.check = "tax.check"),
                     geo.names = c(lat = "decimalLatitude.new",
                                   lon = "decimalLongitude.new",
                                   org.coord = "origin.coord",
                                   prec.coord = "precision.coord",
                                   geo.check = "geo.check"),
                     loc.names = c(loc.str = "loc.correct",
                                   res.gazet = "resolution.gazetteer",
                                   res.orig = "resol.orig",
                                   loc.check = "loc.check"),
                     tax.level = "high", overwrite = FALSE) {

  ## check input
  if (!class(dups) == "data.frame")
    stop("Input object needs to be a data frame!")

  #Escaping R CMD check notes from using data.table syntax
  dup.IDs <- dup.merge <- dup.prop <- same_spp <- tax.check.wk <- NULL
  species.wk <- det.year.wk <- tax.check.wk1 <- geo.check.wk <- NULL
  valor1 <- i.valor <- valor2 <- prioridade <- valor <- res.orig.wk <- NULL
  decimalLatitude.new <- decimalLongitude.new <- NULL


  #Checking essential columns
  if(!dup.name %in% names(dups))
    stop(paste0("Merge is only possible if the input data contain a column with the duplicate IDs"))

  if("tax" %in% info2merge & !all(tax.names %in% names(dups))) {
    warning(paste0("To merge taxonomic info, the input data must contain the following column(s): ",
                   paste0(tax.names[!tax.names %in% names(dups)], collapse = ", "),
                   ". Skipping 'tax' from info to merge."), call. = FALSE)
    info2merge <- info2merge[!info2merge %in% 'tax']
  }

  if("geo" %in% info2merge & !all(geo.names %in% names(dups))) {
    warning(paste0("To merge geographic info, the input data must contain the following column(s): ",
                   paste0(geo.names[!geo.names %in% names(dups)], collapse = ", "),
                   ". Skipping 'geo' from info to merge."), call. = FALSE)
    info2merge <- info2merge[!info2merge %in% 'geo']
  }

  if("loc" %in% info2merge & !all(loc.names %in% names(dups))) {
    warning(paste0("To merge locality info, the input data must contain the following column(s): ",
                   paste0(loc.names[loc.names %in% names(dups)], collapse = ", "),
                   ". Skipping 'loc' from info to merge."), call. = FALSE)
    info2merge <- info2merge[!info2merge %in% 'loc']
  }

  if (length(info2merge) == 0)
    stop(paste0("No information left to merge!"))

  #Making sure there are no factors
  for (i in which(sapply(dups, class) == "factor"))
    dups[[i]] = as.character(dups[[i]])

  #Vector to keep the original data order
  dups$tmp.ordem <- 1:dim(dups)[1]

  #Subsetting the data with any indication of duplicates
  dups1 <- dups[!is.na(dups[, dup.name]),]
  dt <- data.table::data.table(dups1) # transforming the data frame into a data.table
  dt[ , dup.IDs := as.factor(.SD), .SDcols = c(dup.name)]
  data.table::setkey(dt, dup.IDs) # setting 'dup.ID' as key to the data.table (makes computations faster)

  #Creating the duplicate categories
  if (!prop.name %in% names(dt)) {
    warning("The input data has no column with the proportion of duplicates. Assuming to be 1",
            call. = FALSE)
    dt[, dup.merge := TRUE]
  } else {
    dt[, dup.merge := dup.prop > prop]
  }

  if ("tax" %in% info2merge) {
    # creating the new columns for taxonomic check
    wk.cols <- paste0(names(tax.names),".wk")
    dt[, c(wk.cols) := .SD, .SDcols = tax.names]

    ## Finding the most recent, taxonomically valid name for each duplicate ID
    #Are all taxonomically-validated species names equal within a group?
    dt[, same_spp := "vazio"] # creating the new columns for taxonomic check
    #All names for taxonomically-validated specimens are the same? Then, mark as 'yes'
    dt[dup.merge & tax.check.wk %in% tax.level,
       same_spp := if (data.table::uniqueN(species.wk) == 1) "sim"
       else same_spp, by = dup.IDs]
    dt[, same_spp := if (any(same_spp == "sim") &
                         data.table::uniqueN(species.wk) == 1) "sim"
       else same_spp, by = dup.IDs]
    dt[, same_spp := if (any(same_spp == "sim") &
                         data.table::uniqueN(species.wk) > 1) "no"
       else same_spp, by = dup.IDs]

    #Not all names for taxonomically-validated specimens are the same? Then, mark as 'no'
    dt[dup.merge & tax.check.wk %in% tax.level,
       same_spp := if (data.table::uniqueN(species.wk) > 1) "no"
       else same_spp, by = dup.IDs]
    dt[, same_spp := if (any(same_spp == "no")) "no"
       else same_spp, by = dup.IDs]

    ## Defining the most up-to-date species name for the case of >=1 taxonomically validated names (new column species.correct1)
    # converting det.year to numerical
    data.table::setkey(dt, det.year.wk)
    dt[, det.year.wk := suppressWarnings(as.double(det.year.wk)),
       by = det.year.wk]

    # creating the new columns for taxonomic check
    new.cols <- paste0(wk.cols, "1")
    dt[, c(new.cols) := .SD, .SDcols = tax.names]

    #Saving for each duplicate group ID, the most recent identification, in the case of disagreement between the identification of taxonomically-validated specimens
    data.table::setkey(dt, dup.IDs)
    dt[same_spp == "no",
        c(new.cols) := .SD[tax.check.wk %in% tax.level][which.max(det.year.wk[tax.check.wk %in% tax.level])],
        by = dup.IDs, .SDcols = c(wk.cols)]
    dt[same_spp == "sim",
        c("det.name.wk1", "det.year.wk1") := .SD[tax.check.wk %in% tax.level &
                                                   which.max(det.year.wk)][1],
        by = dup.IDs, .SDcols = c("det.name.wk1", "det.year.wk1")]
    dt[same_spp == "sim",
        tax.check.wk1 := if (any(tax.check.wk1 %in% tax.level)) tax.level
        else tax.check.wk1, by = dup.IDs]
    # #table(dt$tax.check); table(dt$tax.check.wk1)

    # #Saving for each duplicate group ID, the most recent identification, in the case of disagreement between the identification of taxonomically-validated specimens
    # dt[same_spp == "no",
    #    "family.wk1" := family.wk[tax.check.wk %in% tax.level][which.max(det.year.wk[tax.check.wk %in% tax.level])],
    #    by = dup.IDs]
    # dt[same_spp == "no",
    #    "species.wk1" := species.wk[tax.check.wk %in% tax.level][which.max(det.year.wk[tax.check.wk %in% tax.level])],
    #    by = dup.IDs]
    # dt[same_spp == "no",
    #    "det.name.wk1" := det.name.wk[tax.check.wk %in% tax.level][which.max(det.year.wk[tax.check.wk %in% tax.level])],
    #    by = dup.IDs]
    # dt[same_spp == "no",
    #    "det.year.wk1" := det.year.wk[tax.check.wk %in% tax.level][which.max(det.year.wk[tax.check.wk %in% tax.level])],
    #    by = dup.IDs]
    # dt[same_spp == "no",
    #    "tax.check.wk1" := tax.check.wk[tax.check.wk %in% tax.level][which.max(det.year.wk[tax.check.wk %in% tax.level])],
    #    by = dup.IDs]
    # dt[same_spp == "sim",
    #    det.name.wk1 := det.name.wk[tax.check.wk %in% tax.level &
    #                                            which.max(det.year.wk)][1], by = dup.IDs]
    # dt[same_spp == "sim",
    #    det.year.wk1 := det.year.wk[tax.check.wk %in% tax.level &
    #                                                which.max(det.year.wk)][1], by = dup.IDs]
    # dt[same_spp == "sim",
    #    tax.check.wk1 := if (any(tax.check.wk1 %in% tax.level)) tax.level
    #    else tax.check.wk1, by = dup.IDs]

    #Removing unecessary columns
    dt[, c("same_spp", wk.cols) := NULL]

    #Renaming the new columns
    new.cols1 <- paste0(tax.names, "1")
    data.table::setnames(dt, c(new.cols), c(new.cols1))
  }

  if ("geo" %in% info2merge) {
    # creating the new columns for taxonomic check
    wk.cols <- paste0(names(geo.names),".wk")
    dt[, c(wk.cols) := .SD, .SDcols = geo.names]

    # replacing NAs
    dt[, geo.check.wk := ifelse(is.na(geo.check.wk), "no_cannot_check", geo.check.wk), ]
    cols <- c('prec.coord.wk',"lat.wk", "lon.wk")
    dt[, (cols) := replace(.SD, is.na(.SD), "no_coord"), .SDcols = cols]

    # creating the new columns for the geographical check and merge
    new.cols <- paste0(wk.cols, "1")
    dt[, c(new.cols) := .SD, .SDcols = geo.names]
    # cols <- c("decimalLatitude.new1","decimalLongitude.new1","origin.coord1","resolution.coord1","geo.check1")
    # dt[, (cols) :=
    #      list(decimalLatitude.new,decimalLongitude.new,origin.coord,resolution.coord,geo.check)]

    # creating the order/priority for geo info replacement
    patt <- c('invert_lon|invert_lat|invert_both|transposed|transp_inv_lon|transp_inv_lat|transp_inv_both')
    dt[, geo.check.wk := gsub(patt, "", geo.check.wk, perl = TRUE), ]
    dt[, geo.check.wk := gsub('\\[\\]', "", geo.check.wk, perl = TRUE), ]
    geo.check.lvs <- data.table::data.table(
      geo.check.wk = c("ok_county", "ok_county_close","ok_locality_gazet","ok_county_gazet",
                    "shore", "ok_state", "ok_state_gazet", "ok_country", "ok_country_gazet",
                    "ok_country[border]",
                    "no_cannot_check", "check_gazetteer", "bad_country", "open_sea"),
      valor = c(1,3,4,5,6,8,9,10,11,
                12,20,20,25,25))
    coord.prec.lvs <- data.table::data.table(
      prec.coord.wk = c("miliseconds", "seconds","seconds_centroid",
                           "minutes", "degrees", "no_coord"),
      valor = c(0,0,0,1,6,20))

    # add values of geo.check and coord. resolution to the original data
    data.table::setDT(dt)[geo.check.lvs, valor1 := i.valor, on = c(geo.check.wk = "geo.check.wk")]
    data.table::setDT(dt)[coord.prec.lvs, valor2 := i.valor, on = c(prec.coord.wk = "prec.coord.wk")]

    # creating the priority index and re-organizing the original data
    dt[ , prioridade := valor1 + valor2]
    dt[ , c("valor1", "valor2") := NULL]
    data.table::setkey(dt, dup.IDs, prioridade)

    #Replacing the columns by the first row by groups of duplicate
    dt[dup.merge == TRUE, (cols) := .SD[1L], by = dup.IDs, .SDcols = cols]

    #Removing unecessary columns
    dt[, c("prioridade", wk.cols) := NULL]

    #Renaming the new columns
    new.cols1 <- paste0(geo.names, "1")
    data.table::setnames(dt, c(new.cols), c(new.cols1))
  }


  if ("loc" %in% info2merge) {
    # creating the new columns for locality check
    wk.cols <- paste0(names(loc.names),".wk")
    dt[, c(wk.cols) := .SD, .SDcols = loc.names]

    # removing NAs and creating the new columns for locality and geographical check
    cols <- c('loc.str.wk', 'loc.check.wk')
    dt[, (cols) := replace(.SD, is.na(.SD), "no_loc"), .SDcols = cols]

    # creating the new columns for the locality check and merge
    new.cols <- paste0(wk.cols[!wk.cols %in% "res.orig.wk"], "1")
    dt[, c(new.cols) := .SD, .SDcols = c(loc.names[!names(loc.names) %in% "res.orig"])]
    # cols <- paste0(cols, "1")
    # dt[, (cols) := list(loc.correct, loc.check)]

    # creating the order/priority for locality info replacement
    loc.check.lvs <- data.table::data.table(
      loc.check.wk = c("check_local.2no.info","check_local.2country","check_local.2state","check_local.2municip.",
                    "check_municip.2no.info","check_municip.2country","check_municip.2state",
                    "check_state2no.info","check_state2country",
                    "check_country2no.info",
                    "ok_country2state","ok_country2municip.","ok_country2locality",
                    "ok_state2municip.","ok_state2locality",
                    "ok_municip.2locality",
                    "ok_same_resolution"),
      valor = c(5,4,3,2,5,4,3,5,4,5,3,2,1,2,1,1,NA_integer_))

    # add values of order/priority to the original data
    data.table::setDT(dt)[loc.check.lvs, valor := i.valor, on = c(loc.check.wk = "loc.check.wk")]

    # add values of order/priority to the original data without loc.check info
    dt[is.na(valor) & res.orig.wk %in% "no_info", valor := 5]
    dt[is.na(valor) & res.orig.wk %in% "country", valor := 4]
    dt[is.na(valor) & res.orig.wk %in% "stateProvince", valor := 3]
    dt[is.na(valor) & res.orig.wk %in% "municipality", valor := 2]
    dt[is.na(valor) & res.orig.wk %in% "locality", valor := 1]

    # re-organizing the original data
    data.table::setkey(dt, dup.IDs, valor)

    #Replacing the columns by the first row by groups of duplicate
    ## include an extra step: paste(unique(loc.correct[which.min(valor)][1]), collapse="|")??
    dt[dup.merge == TRUE, (new.cols) := .SD[1L], by = dup.IDs, .SDcols = new.cols]

    #Removing unecessary columns
    dt[, c("valor", wk.cols) := NULL]

    #Renaming the new columns
    new.cols1 <- paste0(loc.names[!names(loc.names) %in% "res.orig"], "1")
    data.table::setnames(dt, c(new.cols), c(new.cols1))
  }

  if (overwrite) {
    ## Removing columns that are no longer necessary
    dt[, c("dup.IDs", "dup.merge") := NULL]
    data.table::setkeyv(dt, c("tmp.ordem"))

    ## Detecting the target columns to overwrite
    colunas <- data.table::copy(names(dt))
    colunas <- colunas[!colunas %in% names(dups)]
    colunas0 <- gsub("1", "", colunas, perl = TRUE)

    # subsetting and editing the new columns
    tmp.dt <- data.table::data.table(dt[, .SD, .SDcols = colunas])
    names(tmp.dt) <- gsub("1", "", names(tmp.dt))
    tmp.dt[, decimalLatitude.new := as.double(decimalLatitude.new)]
    tmp.dt[, decimalLongitude.new := as.double(decimalLongitude.new)]

    # transforming the original data frame and overwritting the new columns
    dt1 <- data.table::data.table(dups)
    data.table::setkeyv(dt1, c("tmp.ordem"))
    dt1[!is.na(dup.IDs), (colunas0) := tmp.dt]

  } else {
    ## Removing columns that are no longer necessary
    dt[, c("dup.IDs", "dup.merge") := NULL]

    ## Creating the missing new columns in the unicate subset of the data
    colunas <- names(dt)[!names(dt) %in% names(dups)]
    colunas0 <- unique(gsub("1", "", colunas))
    # if(tax.name != colunas0[grepl("scientificName", colunas0)])
    #   colunas0[grepl("scientificName", colunas0)] <- tax.name

    # data with no indication of duplicates
    dt0 <- data.table::data.table(dups[is.na(dups[, dup.name]), ])
    dt0[, (colunas) := .SD, .SDcols = colunas0]

    # merging the unicate and duplicated data tables
    dt1 <- data.table::rbindlist(list(dt0, dt))

    # re-ordering the resulting data.table to the input order
    data.table::setkeyv(dt1, c("tmp.ordem"))
  }

  #Removing unecessary columns and returning
  dt1[, c("tmp.ordem") := NULL]

  dups1 <- data.frame(dt1)
  return(dups1)
}
