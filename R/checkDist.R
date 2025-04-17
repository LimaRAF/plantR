#' @title Check Valid Distribution
#'
#' @description The function checks the location information obtained for each
#'   species record with the Flora do Brasil Online, the World Checklist of
#'   Vascular Plants (WCVP), or a user-defined data set.
#' @param x a data.frame containing the taxon name, taxon authorship name
#'   authorship, and the record location.
#' @param tax.name character. The name of the column containing the taxon name.
#'   Default to "suggestedName".
#' @param tax.author character. The name of the column containing the taxon name
#'   authorship. Default to "suggestedAuthorship".
#' @param loc character. The name of the column containing the record locality
#'   information. Default to "loc.correct".
#' @param sep character. The separator within the locality information column
#'   and within the source of valid distribution Each separator separates
#'   diffrent distribution levels (e.g., country_state_municipality_locality).
#'   `sep` can be one of "_", "/", or "|". Default to "_".
#' @param source character. Source to be consulted for valid distribution
#'   information. One of "bfo", "wcvp", or a user-defined data.frame. Default to
#'   "bfo".
#'
#' @return The input data.frame `x` with two additional columns containing the
#'   validated locality ("dist.check") and auxilliary information
#'   ("dist.check.obs").
#'
#' @details The function provides a way to check if the species distribution
#'   obtained from the locality (e.g, loc.correct = "brazil_sao paulo_campinas")
#'   matches against the known distribution of the species. The known
#'   distribution can be obtained from the Flora do Brasil Online ("bfo", the
#'   default), The World Checklist of Vascular Plants (WCVP) or a "user-defined"
#'   data frame. "bfo" provides the known distribution of the species at state
#'   level (e.g, BR-PR), while the argument "wcvp" uses the World Scheme for
#'   Recording Plant Distribution (WGSRPD, Brummitt et al., 2006) and provides
#'   the known distribution at the
#'   *Botanical Country* (Level 3) and
#'   *Basic* *Recording Units* (Level 4) levels. Due to taxonomic differences
#'   between backbones, we first recommend to prepare the names (ideally using
#'   __plantR__) accordingly to the source of distribution information to be
#'   used, that is, using BFO backbone and `source` = "bfo" or WCVP backbone and
#'   `source` = "wcvp". As an example, in BFO, Araucaria angustifolia (Bertol.)
#'   Kuntze is the accepted name, while in WCVP the accepted name is Araucaria
#'   angustifolia (Bertol.) Steud. If we were to use the first name to check
#'   botanical countries ("wcvp") we would incur in a warning in the output
#'   column "dist.check.obs", indicating that only the canonical name (species
#'   name without authorship) could be found. For most names this should not be
#'   a problem since accepted names tend to be unique, but bear in mind that
#'   this could occur.
#'   
#' @author Guilherme S. Grittz & Renato A. F. de Lima
#'
#' @references Brummitt, R. K., Pando, F., Hollis, S., & Brummitt, N. A. (2001).
#' World geographical scheme for recording plant distributions (Vol. 951, p.
#' 952). Pittsburg: International working group on taxonomic databases for plant
#' sciences (TDWG).
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
#' df <- data.frame(suggestedName = spp_names,
#'                  suggestedAuthorship = spp_authors,
#'                  loc.correct = loc)
#' checkDist(df)
#' checkDist(df, source = "wcvp")
#'
#' @seealso Functions \link[plantR]{fixLoc}, \link[plantR]{prepLoc},
#' \link[plantR]{checkCoord}.
#'
#' @importFrom dplyr left_join
#' @importFrom stats setNames
#'
#' @export checkDist

checkDist <- function(x,
                      tax.name = "suggestedName",
                      tax.author = "suggestedAuthorship",
                      loc = "loc.correct",
                      sep = "_",
                      source = "bfo") {
  
  if(!inherits(x, "data.frame"))
    stop("Input object 'x' needs to be a data frame!", call. = FALSE)
  
  if(nrow(x) == 0)
    stop("Input data frame is empty!", call. = FALSE)
  
  if(!loc %in% names(x))
    stop("Input data frame must have a column with localities!", call. = FALSE)
  
  if(!tax.name %in% names(x))
    stop("Input data frame must have a column with species names!", call. = FALSE)
  
  if(is.na(tax.author)) {
    warning("Column name with species authorities not provided; setting to 
            'suggestedAuthorship'")
    tax.author <- "suggestedAuthorship"
  }
  
  x1 <- x
  tempID <- "tempID"
  x1$tempID <- 1:nrow(x1)
  x$tempID <- 1:nrow(x)
  sep <- gsub("(\\|)", "\\\\\\1", sep, perl = TRUE)
  sep <- gsub("(\\/)", "\\\\\\1", sep, perl = TRUE)
  x1[[loc]] <- prepLoc(x1[[loc]])
  x1$loc.abbrev <- gsub(paste0("^([^", sep, 
                               "]*", sep, 
                               "[^", sep, 
                               "]*)", sep, 
                               ".*"),
                        "\\1",
                        x1[[loc]],
                        perl = TRUE)
  
  if(source == 'bfo') {
    bfoNames <- bfoNames
    # Ugly hardcore fix due to 'name' column that exists, for some reason,
    # in GBIF data
    names(bfoNames)[which(names(bfoNames) == "name")] <- "tax.name"
    names(bfoNames)[which(names(bfoNames) == "authorship")] <- "tax.authorship"
    check_these <- is.na(bfoNames$taxon.distribution)
    if(any(check_these))
      bfoNames$taxon.distribution[check_these] <- "check.name.status"
    x1 <- left_join(x1, 
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
    x1$obs[get_these] <- "check.name.status"
    
    states <- c("acre", "alagoas", "amapa", "amazonas", "bahia", "ceara", 
                "distrito federal", "espirito santo", "goias", "maranhao", 
                "mato grosso", "mato grosso sul", "minas gerais", "para", 
                "paraiba", "parana", "pernambuco", "piaui", "rio janeiro", 
                "rio grande norte", "rio grande sul", "rondonia", "roraima", 
                "santa catarina", "sao paulo", "sergipe", "tocantins")
    
    pattern <- paste0("^","brazil", sep, states, "$")
    
    replacement <- c(
      "BR-AC", "BR-AL", "BR-AP", "BR-AM", "BR-BA", "BR-CE", "BR-DF", 
      "BR-ES", "BR-GO", "BR-MA", "BR-MT", "BR-MS", "BR-MG", "BR-PA", 
      "BR-PB", "BR-PR", "BR-PE", "BR-PI", "BR-RJ", "BR-RN", "BR-RS", 
      "BR-RO", "BR-RR", "BR-SC", "BR-SP", "BR-SE", "BR-TO")
    
    for (j in seq_along(pattern)) {
      x1$loc.abbrev <- gsub(pattern[j], replacement[j], x1$loc.abbrev)
    }
    
    x1$dist.check <- ifelse(is.na(x1$loc.abbrev), NA,
                            ifelse(x1$loc.abbrev == "brazil", "no.cannot.check",
                                   mapply(`%in%`,
                                          x1$loc.abbrev,
                                          strsplit(x1$taxon.distribution, 
                                                   split = "|", 
                                                   fixed = TRUE))))
    x1$dist.check[is.na(x1$loc.abbrev)] <- NA
    x1$dist.check[x1$dist.check == TRUE] <- 'ok.dist'
    x1$dist.check[x1$dist.check == FALSE] <- 'invalid.dist'
    x$dist.check[x$tempID %in% x1$tempID] <- x1$dist.check
    x$dist.check.obs[x$tempID %in% x1$tempID] <- x1$obs
    x <- x[, !(names(x) %in% "tempID")]
    
    return(x)
    
  }
  
  if(source == "wcvp"){
    if (!requireNamespace("plantRdata", quietly = TRUE))
      stop("Please install 'plantRdata' to use this feature")
    wcvp_lookup <- botanicalCountries
    # Had to leave it with :: since wcvpNames is from another package on
    # DESCRIPTION -> Suggestions
    # Same here...
    wcvpNames <- plantRdata::wcvpNames[, c("name", "authorship", "taxon.distribution")]
    names(wcvpNames)[which(names(wcvpNames) == "name")] <- "tax.name"
    names(wcvpNames)[which(names(wcvpNames) == "authorship")] <- "tax.authorship"
    
    wcvpNames <- wcvpNames[!duplicated(paste(wcvpNames$tax.name, 
                                             wcvpNames$tax.authorship)), ]
    x1$level3 <- prepLoc(gsub(paste0(sep, ".*"), "", x1[[loc]]))
    x1$level4 <- prepLoc(gsub(paste0("^[^", sep, 
                                     "]*", sep, 
                                     "([^", sep, 
                                     "]+).*"),
                              "\\1", 
                              x1[[loc]]))
    x1$level4[!grepl(sep, x1[[loc]])] <- NA
    
    check_these <- is.na(wcvpNames$taxon.distribution)
    if(any(check_these))
      wcvpNames$taxon.distribution[check_these] <- "check.name.status"
    x1 <- left_join(x1, 
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
    
    dist.wcvp.name <- sapply(dist.wcvp.name, function(x) paste(x, collapse = "|"))
    x1$level4.name <- dist.wcvp.name
    case1 <- mapply(`%in%`, 
                    x1$level3, 
                    strsplit(x1$level4.name, 
                             split = "|", 
                             fixed = TRUE))
    case2 <- mapply(`%in%`, 
                    x1$level4, 
                    strsplit(x1$level4.name, 
                             split = "|", 
                             fixed = TRUE))
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
    
    userdef <- source
    
    if(dim(userdef)[1] == 0)
      stop("The reference 'source' data frame cannot be empty!",
           call. = FALSE)
    
    key.cols <- c(tax.name, tax.author, loc)
    if(any(!key.cols %in% colnames(userdef)))
      stop("The reference 'source' data frame must contain the columns: ",
           paste(key.cols, collapse = ", "), call. = FALSE)
    
    if(any(!key.cols %in% colnames(x)))
      stop("The input 'x' data frame must contain the columns: ",
           paste0(key.cols, collapse = ", "), call. = FALSE)
    
    if(length(grep("|", userdef[[loc]], fixed = TRUE)) == 0) 
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
    
    check_these <- is.na(userdef[[loc]])
    if(any(check_these))
      userdef[[loc]][check_these] <- "check.name.status"
    
    x1 <- left_join(x1, userdef[, key.cols],
                    by = c(tax.name, tax.author))
    loc.source <- names(x1)[ncol(x1)]
    get_these <- which(!is.na(x1[[loc.source]]) &
                         !is.na(x1[tax.author]) &
                         x1[[loc.source]] != "check.name.status")
    x1$obs <- NA
    x1$obs[get_these] <- "full.name.match"
    get_these <- which(x1[[loc.source]] %in% "check.name.status")
    x1$obs[get_these] <- "check.name.status"
    get_these <- which(is.na(x1[[loc.source]]))
    x1.1 <- left_join(x1[get_these, ], 
                      userdef[, c(tax.name, loc)],
                      by = tax.name)
    x1[[loc.source]][get_these] <- x1.1[, ncol(x1.1)]
    
    x1$obs[get_these][x1[[loc.source]][get_these] != 
                        "check.name.status"] <- "canonical.name.match"
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
                    case.low & !is.na(case.high | case.low)] <- "ok.dist.finer"
    x1$dist.check[case.high &
                    !case.low & !is.na(case.high | case.low)] <- "ok.dist.coarser"
    x1$dist.check[!case.high &
                    !case.low & !is.na(case.high | case.low)] <- "invalid.dist"
    x1$dist.check[is.na(case.high|case.low)] <- NA
    x$dist.check[x$tempID %in% x1$tempID] <- x1$dist.check
    x$dist.check.obs[x$tempID %in% x1$tempID] <- x1$obs
    x <- x[, !(names(x) %in% "tempID")]
    
    return(x)
    
  }
}

