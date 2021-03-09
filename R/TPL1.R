#' @title Standardize plant names according to The Plant List
#'
#' @description Edited copy of the function `TPL()` from package Taxonstand to
#'  avoid an error.
#'
#' @param splist a vector.
#' @param n a index.
#' @param splist A character vector specifying the input taxa, each element
#'   including genus and specific epithet and, potentially, author name and
#'   infraspecific abbreviation and epithet.
#' @param genus	 A character vector containing the genera of plant taxon names.
#' @param species A character vector containing the specific epithets of plant
#'   taxon names.
#' @param infrasp A character vector containing the infraspecific epithets of
#'   plant taxon names.
#' @param infra Logical. If TRUE (default), infraspecific epithets are used to
#'   match taxon names in TPL.
#' @param corr Logical. If TRUE (default), spelling errors are corrected (only)
#'   in the specific and infraspecific epithets prior to taxonomic
#'   standardization.
#' @param diffchar A number indicating the maximum difference between the number
#'   of characters in corrected and original taxon names. Not used if corr =
#'   FALSE.
#' @param max.distance A number indicating the maximum distance allowed for a
#'   match in agrep when performing corrections of spelling errors in specific
#'   epithets. Not used if corr = FALSE.
#' @param version A character vector indicating whether to connect to the newest
#'   version of TPL (1.1) or to the older one (1.0). Defaults to "1.1".
#' @param encoding Encoding to be assumed for input strings from TPL website;
#'   defaults to "UTF-8" (see read.csv and Encoding).
#' @param author Logical. If TRUE (default), the function tries to extract
#'   author names from the input taxon (see Details).
#' @param drop.lower.level Logical. If TRUE, the variety is dropped from the
#'   input taxon if both subspecies and variety are given, and the forma is
#'   dropped from the input taxon if both subspecies or variety and forma are
#'   given. If specific and subspecific epithet are identical, the subspecies
#'   (variety) part is dropped instead, and variety (forma) is kept. Defaults to
#'   FALSE.
#' @param file Either a character string naming a file or a connection open for
#'   writing. "" (default) indicates output to the console.
#' @param silent Logical. If FALSE, the function prints the taxon name that is
#'   currently processed in the output. Defaults to TRUE.
#' @param repeats	number indicating how many times TPLck should be called if no
#'   connection to TPL website can be established (temporarily).
#'
#' @keywords internal
#'
#' @importFrom Taxonstand TPLck
#'
TPL1 <- function (splist, genus = NULL, species = NULL, infrasp = NULL,
          infra = TRUE, corr = TRUE, diffchar = 2, max.distance = 1,
          version = "1.1", encoding = "UTF-8", author = TRUE,
          drop.lower.level = FALSE, file = "", silent = TRUE,
          repeats = 6)
{
  # splist2 <- NULL
  # try(splist2 <- splist, silent = TRUE)
  # if (!is.null(splist2) && (!is.null(genus) || !is.null(species) ||
  #                           !is.null(infrasp))) {
  #   stop("Argument 'splist' incompatible with arguments 'genus' and 'species'")
  # }
  # else if (is.null(splist2) && ((is.null(genus) && !is.null(species)) ||
  #                               (!is.null(genus) && is.null(species)))) {
  #   stop("Arguments 'genus' and 'species' must be provided")
  # }
  # else if (is.null(splist2) && !is.null(genus) && !is.null(species)) {
  #   if (infra == TRUE && !is.null(infrasp)) {
  #     splist <- paste(genus, species, infrasp)
  #   }
  #   else if (infra == FALSE || is.null(infrasp)) {
  #     splist <- paste(genus, species)
  #   }
  # }
  TPLck2 <- function(d) {
    a <- NULL
    if (silent == FALSE) {
      print(paste("Checking", as.character(d), "in The Plant List"))
    }
    counter <- 0
    a <- try(Taxonstand::TPLck(sp = d, infra = infra, corr = corr, diffchar = diffchar,
                   max.distance = max.distance, version = version, encoding = encoding,
                   author = author, drop.lower.level = drop.lower.level),
             silent = TRUE)
    while (class(a) == "try-error" && counter < repeats) {
      a <- try(Taxonstand::TPLck(sp = d, infra = infra, corr = corr,
                     diffchar = diffchar, max.distance = max.distance,
                     version = version, encoding = encoding, author = author,
                     drop.lower.level = drop.lower.level), silent = TRUE)
      counter <- counter + 1
    }
    if (class(a) == "try-error") {
      a <- data.frame(Taxon = d, Genus = NA, Hybrid.marker = NA,
                      Species = NA, Abbrev = NA, Infraspecific.rank = NA,
                      Infraspecific = NA, Authority = NA, ID = NA,
                      Plant.Name.Index = NA, TPL.version = NA, Taxonomic.status = NA,
                      Family = NA, New.Genus = NA, New.Hybrid.marker = NA,
                      New.Species = NA, New.Infraspecific.rank = NA,
                      New.Infraspecific = NA, New.Authority = NA, New.ID = NA,
                      New.Taxonomic.status = NA, Typo = NA, WFormat = NA,
                      Higher.level = NA, Date = NA, Tax_res = NA, stringsAsFactors = FALSE)
    }
    invisible(a)
  }
  # if (length(splist) < 5) {
    results <- do.call("rbind", lapply(splist, TPLck2))
  # }
  # else {
  #   op <- pbapply::pboptions()
  #   pbapply::pboptions(type = "txt")
  #   results <- do.call("rbind", pbapply::pblapply(splist,
  #                                                 TPLck2))
  #   pbapply::pboptions(op)
  # }
  if (infra == FALSE) {
    results <- results[, -c(4, 13)]
  }
  if (nchar(file) > 0) {
    write.csv(results, file = file, row.names = FALSE)
  }
  return(results)
}
