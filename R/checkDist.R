#' @title Check Valid Distribution
#'
#' @description The function checks the location information obtained for each
#'   species record with the Flora do Brasil Online, the World Checklist of
#'   Vascular Plants (WCVP), or a user-defined data set.
#'
#' @param x a data.frame containing the taxon name, taxon authorship, and the
#'   record location.
#' @param tax.name character. The name of the column containing the taxon name.
#'   Default to "scientificName".
#' @param tax.author character. The name of the column containing the taxon name
#'   authorship. Default to "scientificNameAuthorship".
#' @param loc character. The name of the column containing the record location.
#'   Default to "loc.correct".
#' @param sep character. The separator defining different administrative levels
#'   in both location and known distribution columns. (e.g.,
#'   "country_state_municipality_locality"). `sep` can be one of \code{"_"},
#'   \code{"/"}, or \code{"|"}. Default to \code{"_"}.
#' @param source character or `data.frame`. Source to be consulted for known
#'   distribution information. One of "bfo", "wcvp", or a user-defined
#'   `data.frame`. Default to "bfo".
#'
#' @details The function provides a way to check if the species distribution
#'   obtained from the __plantR__ location string (e.g, `loc.correct` =
#'   "brazil_sao paulo_campinas") matches the species known distribution in a
#'   specific `source`. The information is returned in the column `dist.check`,
#'   with auxilliary information being returned in column `dist.check.obs`. If
#'   `dist.check` is "ok.dist", it means that the location string of the record
#'   is within the species known distribution. If `dist.check` is
#'   "invalid.dist", it means that the record is outside of the species known
#'   distribution. For `source` = "bfo" and `dist.check` "no.cannot.check", it
#'   means that the record cannot be validated at state-level (`loc.correct` =
#'   "brazil") or that it occurs outside of Brazil (`loc.correct` =
#'   "chile_santiago"). `dist.check` is `NA` when there is no location string
#'   available for the record or the species distribution is unknown. Column
#'   `dist.check.obs` will indicate if the input species name is complete
#'   (`scientificName`+`scientificAuthorship`; "full.name.match"), partially
#'   complete (`scientificName` only; "canonical.name.match"), or do not exist
#'   in the `source` (for `source` = "bfo" only; "no.match"). When `source` is a
#'   user-defined `data.frame`, `checkDist()` works with two administrative
#'   levels (e.g., country and state; state and municipality) and checks if the
#'   record was found in a coarser administrative level, e.g., only at
#'   country-level ("ok.dist.coarser") or in a finer ("ok.dist.finer"). For a
#'   user-defined `data.frame`, the `data.frame` should contain columns called
#'   `tax.name`, `tax.author`, and `taxon.distribution` and indicate the
#'   occurrence of multiple species known distributions with a pipe \code{"|"}.
#'
#'   The species known distribution can be obtained from three `source`: Flora
#'   do Brasil Online ("bfo", the default), The World Checklist of Vascular
#'   Plants (WCVP) or a "user-defined" `data.frame`. "bfo" provides the species
#'   known distribution at Brazilian state-level (e.g, BR-PR), while "wcvp" uses
#'   the World Scheme for Recording Plant Distribution (WGSRPD, Brummitt et al.,
#'   2006) and provides the species known distribution at the
#'   *Botanical Country* and *Basic Recording Units* levels.
#'
#'   Due to taxonomic differences between backbones, we recommend to prepare the
#'   names according to the source of species known distribution to be used
#'   (ideally using __plantR__ `formatTax()` function). That is, if the user is
#'   using BFO backbone to check taxonomy, use `source` = "bfo". As an example,
#'   in BFO, Araucaria angustifolia (Bertol.) Kuntze is the accepted name, while
#'   in WCVP is Araucaria angustifolia (Bertol.) Steud. If the user were to use
#'   the BFO species name to check botanical countries (`source` = "wcvp") he
#'   would incur in a warning in the output column `dist.check.obs`, indicating
#'   that only the canonical name could be found.
#'
#' @return The input data.frame `x` with two additional columns containing the
#'   information if there was a match in the location string (`dist.check`) and
#'   auxilliary information (`dist.check.obs`).
#'
#' @author Guilherme S. Grittz & Renato A. F. de Lima
#'
#' @references
#'
#' Brummitt, R. K., Pando, F., Hollis, S., & Brummitt, N. A. (2001). World
#' geographical scheme for recording plant distributions (Vol. 951, p. 952).
#' Pittsburg: International working group on taxonomic databases for plant
#' sciences (TDWG).
#'
#' @examples
#' spp_names <- c(rep("Ocotea porosa", 8),
#'                rep("Araucaria angustifolia", 2),
#'                rep("Oreodaphne porosa", 2),
#'                "Echites hirsutus var. angustifolius",
#'                "Acalypha annobonae")
#' spp_authors <- c(rep("(Nees & Mart.) Barroso", 8),
#'                 rep(NA, 2),
#'                 "Nees & Mart.", NA,
#'                 "Stadelm.",
#'                 "Pax & K.Hoffm.")
#' loc <- c("brazil", "brazil_parana",
#' "brazil_santa catarina_blumenau_parque são francisco",
#' "paraguay",
#' "japan_hokkaido",
#' "paraguay_asuncion",
#' "brazil_amazonas_manaus",
#' NA,
#' "brazil_espirito santo",
#' "brazil_santa catarina_blumenau",
#' "chile_santiago", "brazil_parana_curitiba", "brazil_minas gerais",
#' "argentina_buenos aires")
#'
#' df <- data.frame(scientificName = spp_names,
#'                  scientificNameAuthorship = spp_authors,
#'                  loc.correct = loc)
#' checkDist(df)
#'
#' @seealso Functions \link[plantR]{fixLoc}, \link[plantR]{prepLoc},
#'   \link[plantR]{checkCoord}, \link[plantR]{validateCoord}.
#'
#' @importFrom dplyr left_join
#' @importFrom stringr str_replace_all fixed
#' @importFrom stats setNames
#'
#' @export checkDist

checkDist <- function(x,
                      tax.name = "scientificName",
                      tax.author = "scientificNameAuthorship",
                      loc = "loc.correct",
                      sep = "_",
                      source = "bfo") {

  if(!inherits(x, "data.frame"))
    stop("Input object 'x' needs to be a data.frame!", call. = FALSE)

  if(nrow(x) == 0)
    stop("Input data.frame is empty!", call. = FALSE)

  if(!loc %in% names(x))
    stop("Input data.frame must have a column with localities!", call. = FALSE)

  if(!tax.name %in% names(x))
    stop("Input data.frame must have a column with species names!", call. = FALSE)

  if(is.na(tax.author)) {
    warning("Column name with species authorities not provided; setting to
            'scientificNameAuthorship'")
    tax.author <- "scientificNameAuthorship"
  }

  x1 <- x
  sep <- gsub("(\\|)", "\\\\\\1", sep, perl = TRUE)
  sep <- gsub("(\\/)", "\\\\\\1", sep, perl = TRUE)
  x1[[loc]] <- prepLoc(x1[[loc]])

  patt <- paste0("^([^", sep,
                 "]*", sep,
                 "[^", sep,
                 "]*)", sep,
                 ".*")
  x1$loc.abbrev <- gsub(patt, "\\1", x1[[loc]], perl = TRUE)

  if(inherits(source, "data.frame")){
    user_def <- source
    key.cols <- c("tax.name", "tax.author", "taxon.distribution")

    if(dim(user_def)[1] == 0)
      stop("The reference 'source' data.frame cannot be empty!", call. = FALSE)

    if(any(!key.cols %in% colnames(user_def)))
      stop("The reference 'source' data.frame must contain the columns: ",
           paste(key.cols, collapse = ", "), call. = FALSE)

    if(length(grep("|", user_def[[key.cols[3]]], fixed = TRUE)) == 0)
      stop("Known distributions must be separated by a pipe '|'", call. = FALSE)

    loc.source <- key.cols[3]
    user_def[["taxon.distribution"]] <- prepLoc(user_def[["taxon.distribution"]])

    x1$hierarchy.high <- prepLoc(gsub(paste0(sep, ".*"),
                                      "",
                                      x1[[loc]]))
    x1$hierarchy.low <- prepLoc(gsub(paste0("^[^",
                                            sep, "]*",
                                            sep, "([^",
                                            sep, "]+).*"),
                                     "\\1",
                                     x1[[loc]]))
    x1$hierarchy.low[!grepl(sep, x1[[loc]])] <- NA

    x1 <- dplyr::left_join(x1, user_def[, key.cols],
                           by = setNames("tax.name", tax.name),
                           keep = TRUE)

    x1$obs <- NA
    x1$obs <- ifelse(
      !is.na(x1[[tax.name]]) & (x1[[tax.name]] == x1[[tax.name]]),
      ifelse(
        !is.na(x1[[tax.author]]) & (x1[[tax.author]] == x1[["tax.author"]]),
        "full.name.match",
        "canonical.name.match"
      ),
      x1$obs
    )
    x1$obs[is.na(x1$obs)] <- "no.match"

    dist.user <- strsplit(x1[["taxon.distribution"]],
                          split = "|",
                          fixed = TRUE)

    case.high <- mapply(grepl, x1$hierarchy.high, dist.user)
    case.low <- mapply(grepl, x1$hierarchy.low, dist.user)

    x1$dist.check <- NA
    case.high.vec <- sapply(case.high, function(x) any(x, na.rm = FALSE))
    case.low.vec <- sapply(case.low, function(x) any(x, na.rm = FALSE))
    na_mask <- is.na(case.high.vec) & is.na(case.low.vec)

    x1$dist.check[!na_mask & case.high.vec & case.low.vec] <- "ok.dist.finer"
    x1$dist.check[!na_mask & case.high.vec & !case.low.vec] <- "ok.dist.coarser"
    x1$dist.check[!na_mask & !case.high.vec & !case.low.vec] <- "invalid.dist"
    x1$dist.check[!na_mask & case.high.vec & is.na(case.low.vec)] <- "ok.dist.coarser"
    x1$dist.check[!na_mask & !case.high.vec & is.na(case.low.vec)] <- "invalid.dist"
    x1$dist.check[is.na(x1$taxon.distribution)] <- "no.cannot.check"
    x$dist.check <- x1$dist.check
    x$dist.check.obs <- x1$obs

    return(x)
  }

  if(source == 'bfo') {
    x1 <- dplyr::left_join(x1,
                           plantR::bfoNames[, c("tax.name",
                                                "tax.authorship",
                                                "taxon.distribution")],
                           by = setNames("tax.name", tax.name),
                           keep = TRUE)

    x1$obs <- NA
    x1$obs <- ifelse(
      !is.na(x1[[tax.name]]) & (x1[[tax.name]] == x1$tax.name),
      ifelse(
        !is.na(x1[[tax.author]]) & (x1[[tax.author]] == x1$tax.authorship),
        "full.name.match",
        "canonical.name.match"
      ),
      x1$obs
    )
    x1$obs[is.na(x1$obs)] <- "no.match"
    x1$dist.check <- "no.cannot.check"

    rep_these <- grepl("brazil", x1$loc.abbrev, fixed = TRUE) &
      !is.na(x1$loc.abbrev) &
      !x1$taxon.distribution %in% NA

    if (any(rep_these)) {
      exact_brazil <- x1$loc.abbrev[rep_these] == "brazil"

      if (any(exact_brazil)) {
        x1$dist.check[rep_these][exact_brazil] <- "no.cannot.check"
      }

      if (any(!exact_brazil)) {
        pattern <- statesBR
        names(pattern) <- paste0("brazil", sep, names(statesBR))
        x1$loc.abbrev[rep_these][!exact_brazil] <-
          stringr::str_replace_all(
            x1$loc.abbrev[rep_these][!exact_brazil],
            stringr::fixed(pattern)
          )

        dist_matches <- mapply(
          `%in%`,
          x1$loc.abbrev[rep_these][!exact_brazil],
          strsplit(
            x1$taxon.distribution[rep_these][!exact_brazil],
            split = "|",
            fixed = TRUE
          )
        )

        x1$dist.check[rep_these][!exact_brazil] <- ifelse(
          dist_matches,
          "ok.dist",
          "invalid.dist"
        )
      }
    }

    x1$dist.check[is.na(x1$loc.abbrev)] <- NA
    x$dist.check <- x1$dist.check
    x$dist.check.obs <- x1$obs

    return(x)
  }

  if(source == "wcvp") {
    if (!requireNamespace("plantRdata", quietly = TRUE))
      stop("Please install 'plantRdata' to use this feature")

    wcvp_lookup <- botanicalCountries
    wcvpNames <-
      plantRdata::wcvpNames[, c("tax.name", "tax.authorship",
                                "taxon.distribution")]

    wcvpNames <- wcvpNames[!duplicated(paste(wcvpNames$tax.name,
                                             wcvpNames$tax.authorship)), ]

    x1$level3 <- prepLoc(sub(paste0(sep, ".*"), "", x1[[loc]], perl = TRUE))
    x1$level4 <- prepLoc(gsub(paste0("^[^", sep,
                                     "]*", sep,
                                     "([^", sep,
                                     "]+).*"),
                              "\\1",
                              x1[[loc]], perl = TRUE))
    x1$level4[!grepl(sep, x1[[loc]], fixed = TRUE)] <- NA

    x1 <- dplyr::left_join(x1,
                           wcvpNames,
                           by = setNames("tax.name", tax.name),
                           keep = TRUE)

    x1$obs <- NA
    x1$obs <- ifelse(
      !is.na(x1[[tax.name]]) & (x1[[tax.name]] == x1$tax.name),
      ifelse(
        !is.na(x1[[tax.author]]) & (x1[[tax.author]] == x1$tax.authorship),
        "full.name.match",
        "canonical.name.match"
      ),
      x1$obs
    )

    dist.wcvp <- strsplit(x1$taxon.distribution,
                          split = "|",
                          fixed = TRUE)

    f1 <- function(x, y, z){
      x2 <- z[match(x, y, incomparables = NA, nomatch = 0)]
      return(x2)
    }

    dist.wcvp.name <- lapply(dist.wcvp, function(x)
      f1(x, wcvp_lookup$taxon.distribution.bru.code,
         wcvp_lookup$taxon.distribution.bru))

    case1 <- mapply(`%in%`, x1$level3, dist.wcvp.name)
    case2 <- mapply(`%in%`, x1$level4, dist.wcvp.name)

    x1$dist.check <- case1 | case2
    x1$dist.check[is.na(x1$loc.abbrev)] <- NA
    x1$dist.check[x1$dist.check %in% TRUE] <- 'ok.dist'
    x1$dist.check[x1$dist.check %in% FALSE] <- 'invalid.dist'
    x$dist.check <- x1$dist.check
    x$dist.check.obs <- x1$obs

    return(x)
  }
}

