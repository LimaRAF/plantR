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
#' @param ... Any argument to be passed on to the function
#'   `nameMatching` when 'fuzzy.match = TRUE'
#'
#' @return a vector of the same length as \code{x} with the family
#'   names matched in the taxonomic backbone
#'
#' @details
#'
#' The exact and fuzzy matching (if selected) between \code{x} and the
#' names present in the backbone (argument `db`) are only first performed
#' based on currently accepted genus names in the taxonomic backbone
#' (i.e. backbone column taxon.status equal to 'accepted'). If no
#' exact match with an accepted name is found, the function then tries
#' to match input names to synonyms and unplaced names as well.
#'
#' Fuzzy name matching is only performed using the currently accepted
#' genus names in the backbone and the defaults of the __plantR__
#' function `nameMatching()`, which is used internally. But one can
#' specify other values for the arguments of `nameMatching()`. The
#' argument `max.dist` is probably the one that matters most, since it
#' controls the maximum accepted distance between input names and
#' those in the backbone.
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
                      fuzzy.match = FALSE,
                      ...) {

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
    dbs1 <- unique(dbs[dbs$taxon.rank %in% "genus" &
                        dbs$taxon.status %in% "accepted",
                      c("family", "tax.name")])
    names(dbs1)[2] <- "gen.name"

    genera <- dplyr::left_join(genera, dbs1, by = "gen.name")

    check_syn <- is.na(genera$family)
    if (any(check_syn)) {
      dbs2 <- unique(dbs[dbs$taxon.rank %in% "genus" &
                           !dbs$taxon.status %in% "accepted",
                         c("family", "tax.name")])
      names(dbs2)[2] <- "gen.name"
      if (any(duplicated(dbs2$gen.name))) {
        dup_names <- dbs2$gen.name[duplicated(dbs2$gen.name)]
        dbs2 <- dbs2[!dbs2$gen.name %in% dup_names, ]
      }

      tmp <- dplyr::left_join(genera, dbs2, by = "gen.name")
      rep_these <- !is.na(tmp$family.y) & is.na(tmp$family.x)
      if (any(rep_these))
        genera$family[rep_these] <- tmp$family.y[rep_these]
    }

    if (fuzzy.match) {
      check_these <- is.na(genera$family)
      if (any(check_these)) {
        tmp.genera.match <-
          nameMatching(genera[["gen.name"]][check_these],
                       dbs1[["gen.name"]], ...)

        not.na <- !is.na(tmp.genera.match)
        if (any(not.na))
          genera[check_these, "family"] <-
            dbs1[tmp.genera.match, "family"]
      }
    }

    genera1 <- dplyr::left_join(names, genera, by = "gen.name")

    return(genera1$family)
  } else {
    return(rep(NA, length(x)))
  }
}
