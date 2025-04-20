#' @title Check Valid Distribution
#'
#' @description The function checks the location information obtained
#'   for each species record with the Flora do Brasil Online, the
#'   World Checklist of Vascular Plants (WCVP), or a user-defined data
#'   set.
#'
#' @param x a data.frame containing the taxon name, taxon authorship
#'   name authorship, and the record location.
#' @param tax.name character. The name of the column containing the
#'   taxon name. Default to "scientificName".
#' @param tax.author character. The name of the column containing the
#'   taxon name authorship. Default to "scientificNameAuthorship".
#' @param loc character. The name of the column containing the record
#'   locality information. Default to "loc.correct".
#' @param sep character. The separator within the locality information
#'   column and within the source of valid distribution Each separator
#'   separates diffrent distribution levels (e.g.,
#'   country_state_municipality_locality). `sep` can be one of "_",
#'   "/", or "|". Default to "_".
#' @param source character or data frame. Source to be consulted for
#'   valid distribution information. One of "bfo", "wcvp", or a
#'   user-defined data.frame. Default to "bfo".
#'
#' @details The function provides a way to check if the species
#'   distribution obtained from the __plantR__ locality string (e.g,
#'   loc.correct = "brazil_sao paulo_campinas") matches the
#'   known distribution of the species. This information is returned
#'   in the column 'dist.check'. If 'dist.check' is 'ok.dist' it means
#'   that the locality string of the record is within the known
#'   species distribution. If 'dist.check' is 'invalid.dist', it means
#'   that the record is outside the known species distribution.
#'
#'   The known distribution can be obtained from the Flora do Brasil
#'   Online ("bfo", the default), The World Checklist of Vascular
#'   Plants (WCVP) or a "user-defined" data frame. "bfo" provides the
#'   known distribution of the species at state level (e.g, BR-PR),
#'   while the argument "wcvp" uses the World Scheme for Recording
#'   Plant Distribution (WGSRPD, Brummitt et al., 2006) and provides
#'   the known distribution at the *Botanical Country* (Level 3) and
#'   *Basic Recording Units* (Level 4) levels.
#'
#'   Due to taxonomic differences between the BFO and WCVP backbones,
#'   we first recommend to prepare the names (ideally using __plantR__
#'   `formatTax()` function) according to the source of distribution
#'   information to be used. That is, if you are using BFO backbone
#'   to check the taxonomy, use `source` = "bfo" and use `source` =
#'   "wcvp" if your are checking the taxonomy using 'WCVP'.
#'
#'   As an example, in BFO, Araucaria angustifolia (Bertol.) Kuntze is
#'   the accepted name, while in WCVP the accepted name is Araucaria
#'   angustifolia (Bertol.) Steud. If we were to use the first name to
#'   check botanical countries ("wcvp") we would incur in a warning in
#'   the output column "dist.check.obs", indicating that only the
#'   canonical name (species name without authorship) could be found.
#'   For most names this should not be a problem since accepted names
#'   tend to be unique, but bear in mind that this could occur.
#'
#' @return The input data.frame `x` with two additional columns
#'   containing the info if there was a match in the locality string
#'   ("dist.check") and some auxilliary information ("dist.check.obs").
#'
#' @author Guilherme S. Grittz & Renato A. F. de Lima
#'
#' @references
#'
#' Brummitt, R. K., Pando, F., Hollis, S., & Brummitt, N. A. (2001).
#' World geographical scheme for recording plant distributions (Vol.
#' 951, p. 952). Pittsburg: International working group on taxonomic
#' databases for plant sciences (TDWG).
#'
#' @examples
#' spp_names <- c(rep("Ocotea porosa", 8),
#'                rep("Araucaria angustifolia", 2),
#'                rep("Oreodaphne porosa", 2))
#' spp_authors <- c(rep("(Nees & Mart.) Barroso", 8),
#'                 rep(NA, 2),
#'                 "Nees & Mart.", NA)
#' loc <- c("brazil",
#' "brazil_parana",
#' "brazil_santa catarina_blumenau_parque são francisco",
#' "paraguay",
#' "japan_hokkaido",
#' "paraguay_asuncion",
#' "brazil_amazonas_manaus",
#' NA,
#' "brazil_espirito santo",
#' "brazil_santa catarina_blumenau",
#' "chile_santiago", "brazil_parana_curitiba")
#'
#' df <- data.frame(scientificName = spp_names,
#'                  scientificNameAuthorship = spp_authors,
#'                  loc.correct = loc)
#' checkDist(df)
#'
#' @seealso Functions \link[plantR]{fixLoc}, \link[plantR]{prepLoc},
#' \link[plantR]{checkCoord}.
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
    stop("Input object 'x' needs to be a data frame!", call. = FALSE)

  if(nrow(x) == 0)
    stop("Input data frame is empty!", call. = FALSE)

  if(!loc %in% names(x))
    stop("Input data frame must have a column with localities!",
         call. = FALSE)

  if(!tax.name %in% names(x))
    stop("Input data frame must have a column with species names!",
         call. = FALSE)

  if(is.na(tax.author)) {
    warning("Column name with species authorities not provided; setting to
            'scientificNameAuthorship'")
    tax.author <- "suggestedAuthorship"
  }

  x1 <- x
  x1[["tempID"]] <- 1:nrow(x1)
  x[["tempID"]] <- 1:nrow(x)
  sep <- gsub("(\\|)", "\\\\\\1", sep, perl = TRUE)
  sep <- gsub("(\\/)", "\\\\\\1", sep, perl = TRUE)
  x1[[loc]] <- prepLoc(x1[[loc]])
  patt <- paste0("^([^", sep,
                 "]*", sep,
                 "[^", sep,
                 "]*)", sep,
                 ".*")
  x1$loc.abbrev <- gsub(patt, "\\1", x1[[loc]], perl = TRUE)

  if(source == 'bfo') {

    check_these <- is.na(bfoNames$taxon.distribution)
    if(any(check_these))
      bfoNames$taxon.distribution[check_these] <- "check.name.status"
    x1 <- dplyr::left_join(x1,
                    bfoNames[, c("tax.name", "tax.authorship",
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
    get_these <- which(x1$taxon.distribution %in% "check.name.status")
    if (length(get_these) > 0)
      x1$obs[get_these] <- "check.name.status"

    x1$dist.check <- NA

    rep_these <- grepl("brazil", x1$loc.abbrev, fixed = TRUE)
    if (any(rep_these)) {

      rep_these1 <- x1$loc.abbrev[rep_these] %in% "brazil"
      if (any(rep_these1))
        x1$dist.check[rep_these][rep_these1] <- "no.cannot.check"

      pattern <- statesBR
      names(pattern) <- paste0("brazil", sep, names(statesBR))
      x1$loc.abbrev[rep_these][!rep_these1] <-
        stringr::str_replace_all(x1$loc.abbrev[rep_these][!rep_these1],
                                 stringr::fixed(pattern))

      x1$dist.check[rep_these][!rep_these1] <-
        mapply(`%in%`, x1$loc.abbrev[rep_these][!rep_these1],
               strsplit(x1$taxon.distribution[rep_these][!rep_these1],
                                       split = "|", fixed = TRUE))

      # pattern <- paste0("^","brazil", sep, names(statesBR), "$")
      # for (j in seq_along(statesBR)) {
      #   x1$loc.abbrev <- gsub(pattern[j], replacement[j], x1$loc.abbrev)
      # }
      # x1$dist.check <- ifelse(is.na(x1$loc.abbrev), NA,
      #                         ifelse(x1$loc.abbrev == "brazil", "no.cannot.check",
      #                                mapply(`%in%`,
      #                                       x1$loc.abbrev,
      #                                       strsplit(x1$taxon.distribution,
      #                                                split = "|",
      #                                                fixed = TRUE))))
    }
    x1$dist.check[!rep_these] <- FALSE

    # x1$dist.check <- ifelse(is.na(x1$loc.abbrev), NA,
    #                         ifelse(x1$loc.abbrev == "brazil", "no.cannot.check",
    #                                mapply(`%in%`,
    #                                       x1$loc.abbrev,
    #                                       strsplit(x1$taxon.distribution,
    #                                                split = "|",
    #                                                fixed = TRUE))))
    x1$dist.check[is.na(x1$loc.abbrev)] <- NA
    x1$dist.check[x1$dist.check %in% TRUE] <- 'ok.dist'
    x1$dist.check[x1$dist.check %in% FALSE] <- 'invalid.dist'
    x$dist.check[x$tempID %in% x1$tempID] <- x1$dist.check
    x$dist.check.obs[x$tempID %in% x1$tempID] <- x1$obs
    x <- x[, !(names(x) %in% "tempID")]

    return(x)

  }

  if(source == "wcvp"){
    if (!requireNamespace("plantRdata", quietly = TRUE))
      stop("Please install 'plantRdata' to use this feature")

    wcvp_lookup <- botanicalCountries
    wcvpNames <-
      plantRdata::wcvpNames[, c("tax.name", "tax.authorship",
                                "taxon.distribution")]
    wcvpNames <- wcvpNames[!duplicated(paste(wcvpNames$tax.name,
                                             wcvpNames$tax.authorship)), ]
    x1$level3 <- prepLoc(sub(paste0(sep, ".*"), "", x1[[loc]],
                             perl = TRUE))
    x1$level4 <- prepLoc(gsub(paste0("^[^", sep,
                                     "]*", sep,
                                     "([^", sep,
                                     "]+).*"),
                              "\\1",
                              x1[[loc]], perl = TRUE))
    x1$level4[!grepl(sep, x1[[loc]], fixed = TRUE)] <- NA

    check_these <- is.na(wcvpNames$taxon.distribution)
    if(any(check_these))
      wcvpNames$taxon.distribution[check_these] <- "check.name.status"

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

    get_these <- which(x1$taxon.distribution %in% "check.name.status")
    if(length(get_these)>0)
      x1$obs[get_these] <- "check.name.status"

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

    case1 <- mapply(`%in%`,
                    x1$level3,
                    dist.wcvp.name)
    case2 <- mapply(`%in%`,
                    x1$level4,
                    dist.wcvp.name)

    # dist.wcvp.name <- sapply(dist.wcvp.name,
    #                          function(x) paste(x, collapse = "|"))
    # x1$level4.name <- dist.wcvp.name
    # case1 <- mapply(`%in%`,
    #                 x1$level3,
    #                 strsplit(x1$level4.name,
    #                          split = "|",
    #                          fixed = TRUE))
    # case2 <- mapply(`%in%`,
    #                 x1$level4,
    #                 strsplit(x1$level4.name,
    #                          split = "|",
    #                          fixed = TRUE))
    x1$dist.check <- case1 | case2

    x1$dist.check[is.na(x1$loc.abbrev)] <- NA
    x1$dist.check[x1$dist.check %in% TRUE] <- 'ok.dist'
    x1$dist.check[x1$dist.check %in% FALSE] <- 'invalid.dist'
    x$dist.check[x$tempID %in% x1$tempID] <- x1$dist.check
    x$dist.check.obs[x$tempID %in% x1$tempID] <- x1$obs
    x <- x[, !(names(x) %in% "tempID")]

    return(x)

  }

  if(inherits(source, "data.frame")){

    user_def <- source

    if(dim(user_def)[1] == 0)
      stop("The reference 'source' data frame cannot be empty!",
           call. = FALSE)

    key.cols <- c(tax.name, tax.author, loc)
    if(any(!key.cols %in% colnames(user_def)))
      stop("The reference 'source' data frame must contain the columns: ",
           paste(key.cols, collapse = ", "), call. = FALSE)

    if(any(!key.cols %in% colnames(x)))
      stop("The input 'x' data frame must contain the columns: ",
           paste0(key.cols, collapse = ", "), call. = FALSE)

    if(length(grep("|", user_def[[loc]], fixed = TRUE)) == 0)
      stop("The known locality distribution must be separated by a pipe '|'",
           call. = FALSE)

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

    check_these <- is.na(user_def[[loc]])
    if(any(check_these))
      user_def[[loc]][check_these] <- "check.name.status"

    x1 <- dplyr::left_join(x1, user_def[, key.cols],
                    by = c(tax.name, tax.author))
    loc.source <- names(x1)[ncol(x1)]
    x1$obs <- NA
    get_these <- which(!is.na(x1[[loc.source]]) &
                         !is.na(x1[tax.author]) &
                         x1[[loc.source]] != "check.name.status")
    if (any(get_these))
      x1$obs[get_these] <- "full.name.match"

    get_these <- which(x1[[loc.source]] %in% "check.name.status")
    if (any(get_these))
      x1$obs[get_these] <- "check.name.status"

    get_these <- which(is.na(x1[[loc.source]]))
    if (any(get_these)) {
      x1.1 <- dplyr::left_join(x1[get_these, ],
                               user_def[, c(tax.name, loc)],
                               by = tax.name)
      x1[[loc.source]][get_these] <- x1.1[, ncol(x1.1)]

      x1$obs[get_these][x1[[loc.source]][get_these] !=
                          "check.name.status"] <- "canonical.name.match"
    }
    x1$obs[is.na(x1$obs)] <- "check.name.status"
    x1[[loc.source]] <- prepLoc(x1[[loc.source]])

    case.high <- mapply(grepl,
                        x1$hierarchy.high,
                        x1[[loc.source]])
    case.low <- mapply(grepl,
                       x1$hierarchy.low,
                       x1[[loc.source]])

    x1$dist.check <- NA
    x1$dist.check[case.high &
                    case.low & !is.na(case.high | case.low)] <-
      "ok.dist.finer"
    x1$dist.check[case.high &
                    !case.low & !is.na(case.high | case.low)] <-
      "ok.dist.coarser"
    x1$dist.check[!case.high &
                    !case.low & !is.na(case.high | case.low)] <-
      "invalid.dist"
    x1$dist.check[is.na(case.high|case.low)] <- NA
    x$dist.check[x$tempID %in% x1$tempID] <- x1$dist.check
    x$dist.check.obs[x$tempID %in% x1$tempID] <- x1$obs
    x <- x[, !(names(x) %in% "tempID")]

    return(x)

  }
}

