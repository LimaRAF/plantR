#' @title Standardize Species Name Casing
#'
#' @description Capitalize the genus first letter and lowercase the
#'   (infra-)specific epithet of species names.
#'
#' @param x the character string or vector to be standardized.
#'
#' @return the character string equal to \code{x} with the first letter of the
#'   genus capitalized and the first letter of the (infra-)specific epithet in
#'   lowercase.
#'
#' @details The function try to fix some common upper/lowercase issues in
#' species names, taking into account the possible presence of name authorities,
#' as an alternative to the function `fixCase()` from the package `flora`.
#'
#' It works for names at any taxonomic level (e.g. genus, species and
#' infra-specific) and it handle well most common combinations of species names
#' and their authorities. However, the function has not been tested for a wide
#' range of possibilities, so some level of double-checking may be necessary.
#'
#' @author Renato A. F. de Lima
#'
#' @keywords internal
#'
#' @importFrom stringr str_count fixed str_split regex
#' @importFrom stringi stri_locate_all
#'
#' @examples
#'
#' \dontrun{
#' nomes <- c("Lindsaea lancea", "lindsaea lancea", "lindsaea Lancea",
#'           "Lindsaea Lancea", "LINDSAEA LANCEA", "Lindsaea lancea var. Angulata",
#'           "Lindsaea lancea (L.) Bedd.", "Lindsaea Lancea (L.) Bedd.",
#'           "lindsaea lancea (L.) Bedd.",
#'           "Lindsaea", "Lindsaea Dryand. ex Sm.",
#'           "LINDSAEA", "LINDSAEA Dryand. ex Sm.", "LINDSAEA LANCEA (L.) Bedd.",
#'           "Lindsaea lancea var. angulata Rosenst.",
#'           "Lindsaea lancea angulata Rosenst.",
#'           "LINDSAEA LANCEA ANGULATA Rosenst.",
#'           "Blechnum antillanum Proctor",
#'           "Blechnum occidentale leopoldense Dutra",
#'           "Blechnum occidentale var. leopoldense Dutra",
#'           "Cf. Australe")
#'
#' fixCase(nomes)
#' }
#'
fixCase <- function (x) {

  #Isolating the original vector
  nomes <- x

  # detecting missing names
  miss.name <- x %in% c("NA", NA, "", " ")
  x0 <- x
  x <- x[!miss.name]

  if (all(miss.name)) {

    names(x0) <- nomes
    return(x0)

  } else {

    #First letter no capitalized?
    first <- grepl("^[a-z]", x, perl = TRUE)
    if (any(first))
      x[first] <- gsub("(^[a-z])", "\\U\\1", x[first], perl = TRUE)

    #Getting the position of the second space, if present
    spc.loc <- stringi::stri_locate_all(x, fixed = " ")
    spc.loc <- sapply(spc.loc, function(x) x[,1][1:3])
    spc.loc1 <- spc.loc[1, ]
    spc.loc2 <- spc.loc[2, ]
    spc.loc3 <- spc.loc[3, ]

    #All letters capitalized, only the species name
    all.caps <- x == toupper(x) & is.na(spc.loc2)
    if (any(all.caps))
      x[all.caps] <- paste0(substring(x[all.caps], 1, 1),
                            tolower(substring(x[all.caps], 2)))

    sp.caps <- substr(x, 1, spc.loc1 -1) == toupper(substr(x, 1, spc.loc1 -1)) & !is.na(spc.loc1)
    sp.caps[is.na(sp.caps)] <- FALSE
    if (any(sp.caps)) {
      split <- strsplit(gsub("[^[:alnum:] ]", "", x[sp.caps],
                             perl = TRUE), " ", fixed = TRUE)
      n.nms <- lapply(split, function(x) nchar(x) > 1 & x == toupper(x))
      n.caps <- sapply(n.nms, sum, na.rm = TRUE)
      loc <- spc.loc2[sp.caps]
      loc[n.caps == 1] <- spc.loc1[sp.caps][n.caps == 1]
      loc[n.caps == 3] <- spc.loc3[sp.caps][n.caps == 3]
      x[sp.caps] <- paste(paste0(substring(x[sp.caps], 1, 1),
                                 tolower(substring(x[sp.caps], 2, loc -1))),
                                 substring(x[sp.caps], loc),
                                 sep = "")
    }

    #Specific or infra-specific epithet with firt letter capitalized
    patt <- " (?=[A-Z])| (?=\\()"
    # split <- stringr::str_split(x, stringr::regex(patt))
    split <- strsplit(x, patt, perl = TRUE)
    n.str <- lengths(split)
    n.max <- max(n.str, na.rm = TRUE)
    split <- t(sapply(split, `length<-`, n.max))
    split[is.na(split)] <- ""

    if (n.max > 1) {
      not.name <- nchar(split[,2])
      upper_these <- n.str > 1 & !split[,1] %in% c("cf.","Cf.","aff.","Aff.") &
                        grepl("^[A-Z]", split[,2], perl = TRUE) &
                          !grepl("\\W+", split[,2], perl = TRUE) &
                            !(!grepl("\\.$", split[,1], perl = TRUE) &
                               stringr::str_count(split[,1], stringr::fixed(" ")) >= 1) &
                                  not.name >= 3

      if (any(upper_these))
        split[upper_these, 2] <-
          gsub("(^[A-Z])", "\\L\\1", split[upper_these, 2], perl = TRUE)

      #Binding names back together
      fixed.names <- gsub("\\s+$", "",
                          apply(split, 1, paste, collapse = " "),
                          perl = TRUE)
    } else {
      fixed.names <- gsub("\\s+$", "",
                          as.character(split),
                          perl = TRUE)
    }

    # Saving elements and names back to the original vector
    x0[!miss.name] <- fixed.names
    names(x0) <- nomes
    return(x0)
  }
}
