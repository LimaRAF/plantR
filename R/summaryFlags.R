#' @title Summary of Data Validation Flags
#'
#' @description This function provides the summary of the validation steps of
#'   the species occurrence data, including localities, geographical
#'   coordinates, taxonomy and other outputs from the __plantR__ validation
#'   workflow.
#'
#' @param x a data frame with the occurrence data and the columns containing the
#'   outputs of the __plantR__ validation functions.
#'
#' @details The summary output depends on the presence of some key columns in
#'   the input data frame \code{x}. Most of these columns are returned from
#'   __plantR__ functions and they generally are identified by the '.check'
#'   suffix.
#'
#'   The function prints summary tables related to __plantR__ validation
#'   workflow. However, the list of tables that generate those summaries can
#'   also be saved into an object.
#'
#' @import data.table
#' @importFrom knitr kable
#' @importFrom stats aggregate
#' @importFrom utils head
#'
#' @export summaryFlags
#'
summaryFlags <- function(x) {

  ## check input
  if (!class(x) == "data.frame")
    stop("Input object needs to be a data frame!")

  #Escaping R CMD check notes from using data.table syntax
  N <- NULL

  #Select which co-variables will be used in the summary (priority to the edited columns)
  covs <- list(locations = c("loc.check1", "loc.check"),
            coord.resol = c("resolution.coord1", "resolution.coord"),
            coordinates = c("geo.check1", "geo.check"),
            #outliers = c("spat.outlier1", "spat.outlier")
            #spatial.dups = c("spat.duplicate1", "spat.duplicate")
            cultivated = c("cult.check1", "cult.check"),
            taxonomy = c("tax.check1", "tax.check"))

  #Get only the columns of interest
  covs.present <- lapply(covs, function(z) utils::head(z[which(z %in% names(x))], n=1))
  # covs.present <- lapply(covs, function(z) my.head(z[which(z %in% names(x))]))
  covs.present[sapply(covs.present, identical, character(0))] <- NA
  if (all(sapply(covs.present, nchar)==0))
    stop("The input data frame does not contain at least one of the required columns")

  # Filtering the input data frame to
  covs.final <- c(unlist(covs.present), c("dup.ID", "dup.prop", "resol.orig", "resolution.gazetteer"))
  dt <- data.table::data.table(x[, names(x) %in% covs.final])


  ### Summary of flags and other issues

  #How many duplicates?
  if (all(c("dup.ID", "dup.prop") %in% names(dt))) {

    #dups <- table(is.na(dt$dup.ID), occs$dup.prop)
    #dups <- data.table::setDT(dt)[, .N, by = .(dup.ID, dup.prop)]
    dt[,order := 1:dim(dt)[1]]
    dups <- data.table::dcast.data.table(
      data.table::setDT(dt),
      dup.prop ~ 1, value.var = 'order', length)
    dt[,order := NULL]
    suppressWarnings(dups$dup.prop[!is.na(as.double(dups$dup.prop))] <-
                       paste0(100 * as.double(dups$dup.prop[!is.na(as.double(dups$dup.prop))]),"%"))
    dups$dup.prop[dups$dup.prop %in% "cc"] <- "Cannot check (no info)"
    names(dups)[2] <- "N"

    cat("==================", sep="\n")
    cat(" DUPLICATE SEARCH ", sep="\n")
    cat("==================", sep="\n")
    cat("Records per strength of duplicate indication:\n",
        #dups,
        knitr::kable(dups, col.names = c("Strenght", "Records")),
        sep="\n")
  } else { dups <- NULL}

  # How many valid localions?
  if (!is.na(covs.present[["locations"]])) {
    cat("\n=====================", sep="\n")
    cat(" LOCALITY VALIDATION ", sep="\n")
    cat("=====================", sep="\n")

    locs <- dt[ , .N, by = c(covs.present[["locations"]])]
    locs.clean <- data.frame(locs)
    locs.clean$check <- as.character(locs.clean[, 1])
    locs.clean$check[grep("ok_",locs.clean$check)] <- "ok (upgraded)"
    locs.clean$check[grep("check_",locs.clean$check)] <- "check (downgraded)"
    locs.clean$check[locs.clean[,1] == "ok_same_resolution"] <-
      "ok (same resolution)"
    locs.clean$check[locs.clean[,1] == "check_local.2municip."] <-
      "probably ok"
    locs.clean$check[grepl("2no\\.info", locs.clean[,1])] <-
      "check (not found)"

    locs.clean <- rowsum(locs.clean[2], locs.clean$check, na.rm = TRUE)
    locs.clean <- locs.clean[order(locs.clean$N, decreasing = TRUE), , drop = FALSE]
    locs.clean <- data.frame(cat = row.names(locs.clean),
                             N = locs.clean$N, stringsAsFactors = FALSE)

    if (all(c("resol.orig", "resolution.gazetteer") %in% names(dt))) {
      dt[,order := 1:dim(dt)[1]]
      locs1 <- data.table::dcast.data.table(
        data.table::setDT(dt),
        resol.orig ~ resolution.gazetteer, value.var = 'order', length)
      dt[,order := NULL]

      locs1 <- data.frame(locs1)
      levels <- c("no_info", "country", "stateProvince", "municipality", "locality")
      locs1 <- locs1[match(levels, locs1$resol.orig) ,]
      levels1 <- c("no_info", "country", "state", "county", "locality")
      locs1 <- cbind.data.frame(locs1[1],
                                locs1[,match(levels1, names(locs1))],
                                stringsAsFactors = FALSE)
      names(locs1)[1] <- "original.resolution"
      names(locs1) <- gsub("state", "stateProvince", names(locs1))
      names(locs1) <- gsub("county", "municipality", names(locs1))
      rownames(locs1) <- NULL

    } else { locs <- NULL }

    cat("Results of the locality validation:\n",
        #dups,
        knitr::kable(locs.clean, col.names = c("Validation", "Records")),
        sep="\n")
    if (!is.null(locs1))
      cat("\nDetails of the validation (original vs. validated localities):\n",
          knitr::kable(locs1),
          sep="\n")

  }

  # How many geographically validated coordinates?
  if (!is.na(covs.present[["coordinates"]])) {
    cat("\n=======================", sep="\n")
    cat(" COORDINATE VALIDATION ", sep="\n")
    cat("=======================", sep="\n")

    coords <- dt[ , .N, by = c(covs.present[["coordinates"]])]
    coords <- coords[order(N, decreasing = TRUE),]

    coords.clean <- data.frame(coords)
    coords.clean$check <- "original"
    coords.clean$check[grep("_gazet", coords.clean[, 1])] <- "gazetter"
    coords.clean$check[grepl("no_", coords.clean[, 1]) |
                         is.na(coords.clean[, 1])] <- "cannot_check"
    coords.clean$check1 <- "yes"
    coords.clean$check1[coords.clean$check == "cannot_check"] <- "no"
    coords.clean[, 1] <- gsub("_gazet$", "", coords.clean[, 1])
    coords.clean[, 1][is.na(coords.clean[, 1])] <- "no_cannot_check"

    patt <- c('invert_lon|invert_lat|invert_both|transposed|transp_inv_lon|transp_inv_lat|transp_inv_both')
    coords.clean[, 1] <- gsub(patt, "", coords.clean[, 1], perl = TRUE)
    coords.clean[, 1] <- gsub('\\[\\]', "", coords.clean[, 1], perl = TRUE)

    coords.clean0 <- stats::aggregate(coords.clean$N, list(coords.clean$check1, coords.clean$check), sum, na.rm = TRUE)
    names(coords.clean0) <- c("Validated", "Origin", "Records")
    coords.clean0 <- coords.clean0[order(coords.clean0$Records, decreasing = TRUE), , drop = FALSE]
    row.names(coords.clean0) <- NULL

    coords.clean1 <- stats::aggregate(coords.clean$N, list(coords.clean$check1, coords.clean[,1]), sum, na.rm = TRUE)
    names(coords.clean1) <- c("Validated", "Resolution", "Records")
    coords.clean1 <- coords.clean1[order(coords.clean1$Records, decreasing = TRUE), , drop = FALSE]
    row.names(coords.clean1) <- NULL

    cat("Valid coordinates per origin:\n",
        #dups,
        knitr::kable(coords.clean0),
        sep="\n")

    cat("\nValid coordinates per resolution:\n",
        #dups,
        knitr::kable(coords.clean1),
        sep="\n")

  } else { coords <- NULL }

  # spatial outliers and duplicates
  ### TO BE IMPLEMENTED ###

  # How many probably cultivated specimens?
  if (!is.na(covs.present[["cultivated"]])) {

    cat("\n======================", sep="\n")
    cat(" CULTIVATED SPECIMENS ", sep="\n")
    cat("======================", sep="\n")

    cults <- dt[ , .N, by = c(covs.present[["cultivated"]])]
    cults <- data.frame(cults[order(N, decreasing = TRUE),])
    cults[,1][is.na(cults[,1])] <- "probably not"
    cults[,1][cults[,1] == "prob_cultivated"] <- "probably yes"
    cults[,1][cults[,1] == "cultivated"] <- "yes"

    cat("Number of specimens from cultivated individuals:\n",
        knitr::kable(cults, col.names = c("Cultivated", "Records")),
        sep="\n")

  } else { cults <- NULL }

  # Confidence level on species taxonomic identifications
  if (!is.na(covs.present[["taxonomy"]])) {

    cat("\n======================", sep="\n")
    cat(" TAXONOMIC CONFIDENCE ", sep="\n")
    cat("======================", sep="\n")

    taxs <- dt[ , .N, by = c(covs.present[["taxonomy"]])]
    taxs <- data.frame(taxs[order(N, decreasing = TRUE),])
    taxs[,1][is.na(taxs[,1])] <- "unknown"

    cat("Confidence level of the taxonomic identifications:\n",
        knitr::kable(taxs, col.names = c("Confidence", "Records")),
        sep="\n")


  } else { taxs <- NULL }

  # Organizing the output and returning
  r <- list(duplicates = dups,
            locations = locs,
            coordinates = coords,
            cultivated = cults,
            taxonomy = taxs)


  invisible(r)
}
