#' @title Summary of Species Occurrence Data
#'
#' @description This function provides a general summary of the occurrence data,
#'   including the number of records, biological collections, taxa and
#'   countries.
#'
#' @param x a data frame with the occurrence data.
#' @param print logical. Should the first part of the table be printed? Default
#'   to TRUE.
#' @param top numerical. Number of groups (e.g. collections, families) to be
#'   printed. Default to 5.
#'
#' @details The summary output depends on the presence of some key columns in
#'   the input data frame \code{x}, which should follow the Darwin Core
#'   standards. If the edited columns from __plantR__ are given, they are given
#'   priority to report the data summaries.
#'
#'   The function prints the summary tables related to the occurrence data.
#'   However, the tables generating those summaries can be saved into an object.
#'
#' @import data.table
#' @importFrom stringr str_trim
#' @importFrom knitr kable
#' @importFrom stats quantile
#' @importFrom utils head tail
#'
#'
#' @export summaryData
#'
summaryData <- function(x, print = TRUE, top = 5) {

  ## check input
  if (!class(x) == "data.frame")
    stop("Input object needs to be a data frame!")

  if (dim(x)[1] == 0)
    stop("Input data frame is empty!")

  #Escaping R CMD check notes from using data.table syntax
  dup.ID <- dup.prop <- year.new <- country.new <- NULL
  N <- S <- genus.new <- NULL

  #Select which co-variables will be used in the summary (priority to the edited columns)
  covs <- list(collections = c("collectionCode.new", "collectionCode"),
            collectors = c("recordedBy.new", "recordedBy"),
            colYears = c("year.new", "year"),
            families = c("family.new", "family"),
            genera = c("genus.new", "genus"),
            species = c("scientificName.new", "scientificName"),
            countries = c("country.new", "country"))

  #Get only the columns of interest
  covs.present <- lapply(covs, function(z) utils::head(z[which(z %in% names(x))], n = 1))
  if (all(sapply(covs.present, nchar)==0))
    stop("The input data frame does not contain at least one of the required columns")

  # Filtering the input data frame to
  covs.final <- c(unlist(covs.present), c("dup.ID", "dup.prop"))
  dt <- data.table::data.table(x[, names(x) %in% covs.final])

  #How many records?
  if (all(c("dup.ID", "dup.prop") %in% names(dt))) {
    ccs <- dim(dt[is.na(dup.ID) & dup.prop == "cc",])[1]
    unicatas <- dim(dt[is.na(dup.ID) & dup.prop != "cc",])[1]
    duplicatas <- dt[!is.na(dup.ID), .N, by = "dup.ID"]
    total1 <- ccs + unicatas + dim(duplicatas)[1]
    total <- dim(dt)[1]
    if (print) {
      cat("=========", sep="\n")
      cat(" RECORDS ", sep="\n")
      cat("=========", sep="\n")
      cat(knitr::kable(data.frame(Type = c("Unicates", "Duplicates", "Unknown", "Total without duplicates", "Total with duplicates"),
                                  Records = c(unicatas, sum(duplicatas$N), ccs,  total1, total))), sep="\n")
    }
  } else {
    if (print) {
      cat("=========", sep="\n")
      cat(" RECORDS ", sep="\n")
      cat("=========", sep="\n")
      cat("Total number of records:", dim(dt)[1],"\n", sep=" ")
      warning("Columns 'dup.ID' and 'dup.prop' not found; cannot report number of duplicates and unicates")
    }
  }

  ## Collections
  if (any(nchar(covs.present[c("collections","collectors","colYears")]) > 0)) {
    if (print) {
      cat("\n=============", sep="\n")
      cat(" COLLECTIONS ", sep="\n")
      cat("=============", sep="\n")
    }

    # How many collections?
    if (nchar(covs.present[["collections"]]) > 0) {
      colls <- dt[ , .N, by = c(covs.present[["collections"]])]
      data.table::setkeyv(colls, covs.present[["collections"]])
      if (NA_character_ %in% colls[[1]]) {
        colls1 <- colls[!NA_character_]
        colls1 <- colls1[order(N, decreasing = TRUE),]
        colls <- rbind(colls1, colls[NA_character_])
        if (print)
          cat("Number of biological collections:", dim(colls1)[1],"\n", sep=" ")
      } else {
        colls <- colls[order(N, decreasing = TRUE),]
        if (print)
          cat("Number of biological collections:", dim(colls)[1],"\n", sep=" ")
      }
    } else { colls <- NULL }

    # Collectors
    if (nchar(covs.present[["collectors"]]) > 0) {
      cols <- dt[ , .N, by = c(covs.present[["collectors"]])]
      data.table::setkeyv(cols, covs.present[["collectors"]])
      if ("s.n." %in% cols[[1]]) {
        cols1 <- cols[!"s.n."]
        cols1 <- cols1[order(N, decreasing = TRUE),]
        cols <- rbind(cols1, cols["s.n."])
        if (print)
          cat("Number of collectors' names:", dim(cols1)[1],"\n", sep=" ")
      } else {
        cols <- cols[order(N, decreasing = TRUE),]
        if (print)
          cat("Number of collectors' names:", dim(cols)[1],"\n", sep=" ")
      }
    } else { cols <- NULL }

    # Collection years
    if (nchar(covs.present[["colYears"]]) > 0) {
      anos <- dt[ , .N, by = c(covs.present[["colYears"]])]
      ano.lim <- as.double(strsplit(date(), " ")[[1]][5])
      suppressWarnings(dt[, year.new := as.double(year.new)])
      anos.qt <- as.double(dt[year.new <= ano.lim,
                              stats::quantile(year.new, prob=c(0,0.1,0.25,0.5,0.75,0.9,1), na.rm = TRUE),])
      if (print)
        cat("Collection years: ", anos.qt[1],"-", utils::tail(anos.qt, n = 1)," (>90% and >50% after ",anos.qt[2]," and ",anos.qt[4],")","\n", sep="")
    } else { anos <- NULL }

    if (nchar(covs.present[["collections"]]) > 0)
      if (print)
        cat("\nTop collections in numbers of records:",
            knitr::kable(utils::head(colls, top), col.names = c("Collection", "Records")), sep="\n")

    if (nchar(covs.present[["collectors"]]) > 0)
      if (print)
        cat("\nTop collectors in numbers of records:",
            knitr::kable(utils::head(cols, top), col.names = c("Collector", "Records")), sep="\n")
  }

  ## Taxonomy
  if (any(nchar(covs.present[c("families","genera","species")]) > 0)) {
    if (print) {
      cat("\n==========", sep="\n")
      cat(" TAXONOMY ", sep="\n")
      cat("==========", sep="\n")
    }

    # How many families, genera and species?
    if (!"genus.new" %in% names(dt) & nchar(covs.present[["species"]]) > 0) {
      getGenus <- function(x) as.character(strsplit(x, " ")[[1]][1])
      dt[ , genus.new := lapply(.SD, getGenus),
          by = c(covs.present[["species"]]),
          .SDcols = c(covs.present[["species"]])]
    }

    tax.cols <- c(covs.present[["families"]],
                  "genus.new",
                  covs.present[["species"]])
    genus.only <- grepl(" ", dt[[covs.present[["species"]]]], fixed = TRUE)
    tax <- as.character(dt[genus.only, lapply(.SD, data.table::uniqueN),
                           .SDcols = c(tax.cols)])
    names(tax) <- tax.cols

    if (nchar(covs.present[["families"]]) > 0) {
      fams <- dt[ , .N, by = c(covs.present[["families"]])]
      fams[, S := dt[genus.only , data.table::uniqueN(.SD),
                     by = c(covs.present[["families"]]),
                     .SDcols = c(covs.present[["species"]])]$V1]
      data.table::setorderv(fams, c("S", "N"), c(-1,-1))
      if (print)
        cat("Number of families:", tax[covs.present[["families"]]],"\n", sep=" ")
    } else { fams <- NULL }

    if (nchar(tax[["genus.new"]]) > 0) {
      gens <- dt[ , .N, by = c("genus.new")]
      gens[, S := dt[ , data.table::uniqueN(.SD),
                      by = c("genus.new"),
                      .SDcols = c(covs.present[["species"]])]$V1]
      data.table::setorderv(gens, c("S", "N"), c(-1,-1))
      if (print)
        cat("Number of genera:", tax["genus.new"],"\n", sep=" ")
    } else { gens <- NULL }

    if (print)
      if (nchar(covs.present[["species"]]) > 0)
        cat("Number of species:", tax[covs.present[["species"]]],"\n", sep=" ")

    if (print)
      if (nchar(covs.present[["families"]]) > 0)
        cat("\nTop richest families:", knitr::kable(utils::head(fams, top)), sep="\n")

    if (print)
      cat("\nTop richest genera:", knitr::kable(utils::head(gens, top)), sep="\n")
  }

  ## Countries
  if (nchar(covs.present[["countries"]]) > 0) {
    paises <-  dt[ , .N, by = c(covs.present[["countries"]])]

    if (nchar(covs.present[["species"]]) > 0) {
      paises[, S := dt[ , data.table::uniqueN(.SD),
                        by = c(covs.present[["countries"]]),
                        .SDcols = c(covs.present[["species"]])]$V1]
    }
    paises <- paises[order(N, decreasing = TRUE),]
    tmp <- getAdmin(paises[[1]])$NAME_0
    tmp[is.na(tmp)] <- as.character(sapply(paises[[1]][is.na(tmp)], capName))
    tmp[tmp %in% ""] <- "[Unknown]"
    tmp <- gsub("NANA", NA, tmp)
    paises$country.new <- tmp

    if (print) {
      cat("\n===========", sep="\n")
      cat(" COUNTRIES ", sep="\n")
      cat("===========", sep="\n")
      cat("Number of countries:", dim(paises[!is.na(country.new) &
                                               !country.new %in% "[Unknown]"])[1],
          "\n", sep=" ")
      cat("\nTop countries in numbers of records:",
          knitr::kable(utils::head(paises, top),
                       # knitr::kable(my.head.df(paises, top),
                       col.names = c("Country", "Records", "Species")[1:dim(paises)[2]]), sep="\n")
    }
  } else { paises <- NULL }

  # Organizing the output and returning
  r <- list(collections = colls,
            collectors = cols,
            colYears = anos,
            families = fams,
            genera = gens,
            countries = paises)

  invisible(r)
}
