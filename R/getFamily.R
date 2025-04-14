#' @title Get Family Name
#'
#' @description Search for family names based on a
#'   vector of genus names and a taxonomic backbone
#'
#' @param x a vector of genus names with no default.
#' @param db the taxonomic backbone to be consulted for family names.
#'   By default, the Brazilian Flora 2020 taxonomic backbone
#'   ('bfo') is used.
#' @param fuzzy.match logical. Whether if fuzzy matching on genus names
#'   should be conducted. Defaults to FALSE (no fuzzy matching)
#'
#' @return a vector of the same length as \code{x} with the family
#'   names matched in the taxonomic backbone
#'
#' @details
#'
#' The exact and fuzzy matching (if selected) between \code{x} and the
#' names of genera present in the backbone (argument `db`) are only
#' performed based on currently accepted genus names in the taxonomic
#' backbone (i.e. backbone column taxon.status equal to 'accepted').
#'
#' Currently, fuzzy macthes are conducted using the defaults of
#' function `nameMatching()`, which is used internally.
#'
#' @author Renato A. Ferreira de Lima
#'
#' @export getFamily
#'
#' @importFrom dplyr left_join
#'
#' @examples
#'
#' genus <- c("Trema", "Casearia", "Turnera", "Viviania", "Dombeya",
#'         "Casearea", "Indet.", "",  NA)
#' getFamily(genus)
#' getFamily(genus, fuzzy.match = TRUE)
#'
getFamily <- function(x = NULL,
                      db = "bfo",
                      fuzzy.match = FALSE) {

  if (is.null(x))
    stop("Input vector of genus names is empty!")

  na.strings <- c("", " ", "na", "indet", "indet.")
  x[tolower(x) %in% na.strings] <- NA_character_

  names <- data.frame(x)
  colnames(names) <- "gen.name"
  names.na <- is.na(names[["gen.name"]])

  if (any(!names.na)) {
    genera <- unique(names[!names.na, , drop = FALSE])

    dbs <- getTaxBackbone(db)[[1]]
    dbs <- unique(dbs[dbs$taxon.rank %in% "genus" &
                        dbs$taxon.status %in% "accepted",
                      c("family", "name")])
    names(dbs)[2] <- "gen.name"

    genera <- dplyr::left_join(genera, dbs, by = "gen.name")

    if (fuzzy.match) {
      check_these <- is.na(genera$family)
      if (any(check_these)) {
        tmp.genera.match <-
          nameMatching(genera[["gen.name"]][check_these],
                       dbs[["gen.name"]])

        not.na <- !is.na(tmp.genera.match)
        if (any(not.na))
          genera[check_these, "family"] <-
            dbs[tmp.genera.match, "family"]
      }
    }

    genera1 <- dplyr::left_join(names, genera, by = "gen.name")

    return(genera1$family)
  } else {
    return(rep(NA, length(x)))
  }
}
