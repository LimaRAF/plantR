#' @title Fix Taxon Name with Authorship
#'
#' @description Recognize strings of scientific names with authorship
#'   and split scientific name and authorship into two different
#'   columns of a data frame.
#'
#' @return a data frame with the original name, the cleaned scientific
#'   and the authorship in different columns
#'
#' @details The function works for most cases but it is not perfect,
#'   particularly if authorship notation in different of what is
#'   commonly expected. The expectation are names following the
#'   notation:
#'   * Higher-taxa authorship: e.g. 'Araceae Juss.'
#'   * Genus epithet authorship: e.g. 'Anthurium amoenum Kunth & C.D.Bouche'
#'   * Genus epithet infraepithet authorship: e.g. 'Anthurium amoenum var. humile (Schott) Engl.'
#'
#'   Besides rare examples of author names starting with lower-case
#'   prepositions (e.g. 'de', 'van'), authorships should have
#'   the first letter capitalized, otherwise the function will not
#'   recognize the start of the authorship.
#'
#'   In addition, the function is case-sensitive and it assumes that
#'   there are no issues related to scientific name casing (e.g.
#'   'Anthurium cf. Amoenum') to separate the scientific name from the
#'   authorship. So, please make sure there are no casing issues. See
#'   the `plantR` functions `fixCase()`
#'
#' @param taxa a vector containing taxon names
#' @param ranks a vector containing the super or infra-specific ranks
#'   to be searched and corrected within problematic taxon
#'   name-authorship splits. Defaults to 'var.', 'subsp.', 'f.' and
#'   'forma'
#'
#' @author Renato A. Ferreira de Lima
#'
#' @seealso
#'  \link[plantR]{fixCase}
#'
#'
#' @examples
#' names <- c("Lindsaea", "Lindsaea sp.",
#'  "Lindsaea lancea",
#'  "Lindsaea lancea (L.) Bedd.",
#'  "Parablechnum C.Presl",
#'  "Blechnum spannagelii Rosenst.",
#'  "Blechnum austrobrasilianum de la Sota",
#'  "Blechnum occidentale leopoldense Dutra",
#'  "Casearia sylvestris var. angustifolia",
#'  "Casearia sylvestris var. angustifolia Uittien",
#'  "Casearia sylvestris Sw. var. sylvestris",
#'  "Philodendron sodiroi hort.")
#'
#' fixAuthors(names)
#'
#' @importFrom stringr str_count fixed
#'
#' @export fixAuthors
#'
#'
fixAuthors <- function(taxa = NULL,
                       ranks = c("var.", "subsp.", "f.", "forma")) {

  if (is.null(taxa)) {
    stop("Input vector is empty!")
  }

  if (!inherits(taxa, "character")) {
    stop("Input object needs to be a character vector!")
  }

  # Generate return object
  res0 <- data.frame(orig.name = taxa,
                    tax.name = NA,
                    tax.author = NA)

  rep_ids0 <- grepl(" ", taxa, fixed = TRUE)
  # Check if there are any potential names to be fixed
  if (sum(rep_ids0) == 0) {
    res0$tax.name <- taxa
    return(res0)
  }

  taxa1 <- taxa[rep_ids0]
  res <- data.frame(orig.name = taxa1,
                    tax.name = NA,
                    tax.author = NA,
                    fix.author = NA)

  rep_ids1 <- grep(" \\(", taxa1, perl = TRUE)
  if (length(rep_ids1) > 0L) {
    res[[2]][rep_ids1] <-
      gsub(" \\(.*", "", res[[1]][rep_ids1], perl = TRUE)
    res[[3]][rep_ids1] <-
      gsub("(.* )(\\(.*)", "\\2", res[[1]][rep_ids1], perl = TRUE)
  }

  no_auth <- res[[2]] %in% NA
  author_preps <- c(" de ", " la ", " du ", " van ", " von ", " del ",
                    " den ")
  rep_ids2 <- grep(paste0(author_preps, collapse = "|"),
                   taxa1[no_auth], perl = TRUE, ignore.case = TRUE)
  if (length(rep_ids2) > 0L) {
    taxa2check <- taxa1[no_auth][rep_ids2]
    for (i in seq_along(author_preps)) {
      preps_ids <- grep(author_preps[i], taxa2check, perl = TRUE,
                        ignore.case = TRUE)
      if (length(preps_ids) > 0L)
        taxa2check[preps_ids] <- gsub(paste0(author_preps[i], ".*"), "",
                                      taxa2check[preps_ids],
                                      perl = TRUE, ignore.case = TRUE)
    }
    res[[2]][no_auth][rep_ids2] <- taxa2check
  }

  caps_and_prep <-
    grep("\\s\\p{Lu}", res[[2]][no_auth][rep_ids2], perl = TRUE)
  if (length(caps_and_prep) > 0L)
    res[[2]][no_auth][rep_ids2][caps_and_prep] <-
      gsub("\\s\\p{Lu}.*", "",
           res[[2]][no_auth][rep_ids2][caps_and_prep], perl = TRUE)

  no_auth <- res[[2]] %in% NA
  many_names <- stringr::str_count(taxa1,
                                   stringr::fixed(" ")) > 1
  other_preps <- c(" hort", " auct", " sensu", " d'", " ined")
  other_patt <- paste0(other_preps, collapse = "|")
  rep_ids2.1 <- grep(other_patt, taxa1[no_auth & many_names],
                     perl = TRUE)
  if (length(rep_ids2.1) > 0L) {
    taxa2check <- taxa1[no_auth & many_names][rep_ids2.1]
    for (i in seq_along(other_preps)) {
      preps_ids1 <- grep(other_preps[i], taxa2check, fixed = TRUE)
      if (length(preps_ids1) > 0L)
        taxa2check[preps_ids1] <- gsub(paste0(other_preps[i], ".*"), "",
                                      taxa2check[preps_ids1], perl = TRUE)
    }

    ranks_patt <- paste0(ranks, "$", collapse = "|")
    ranks_patt <- gsub("\\.", "\\\\.", ranks_patt, perl = TRUE)
    check_issues <- !grepl(" ", taxa2check, fixed = TRUE) |
                      grepl(ranks_patt, taxa2check, perl = TRUE)

    if (any(check_issues)) {
      taxa2check1 <- taxa1[no_auth & many_names][rep_ids2.1][check_issues]
      taxa2check[check_issues] <-
        gsub("\\(|\\s\\p{Lu}.*", "", taxa2check1, perl = TRUE)
    }

    res[[2]][no_auth & many_names][rep_ids2.1] <- taxa2check
  }

  no_auth <- which(res[[2]] %in% NA)
  rep_ids3 <- grep("\\s\\p{Lu}", taxa1[no_auth],perl = TRUE)
  if (length(rep_ids3) > 0L) {
    res[[2]][no_auth][rep_ids3] <-
      gsub("\\s\\p{Lu}.*", "",
           res[[1]][no_auth][rep_ids3], perl = TRUE)
    res[[3]][no_auth][rep_ids3] <-
      sub(".*? (\\p{Lu})", "\\1",
           res[[1]][no_auth][rep_ids3], perl = TRUE)
  }

  first_hyb <- res[[2]] %in% c("x", "\u00d7")
  if (any(first_hyb)) {
    res[[1]][first_hyb] <-
      gsub("^x |^\u00d7 ", "", res[[1]][first_hyb], perl = TRUE)
    res[[2]][first_hyb] <-
      gsub("\\s\\p{Lu}.*", "", res[[1]][first_hyb], perl = TRUE)
    res[[3]][first_hyb] <-
      sub(".*? (\\p{Lu})", "\\1", res[[1]][first_hyb], perl = TRUE)
  }

  bad_preps <- c(" ex$", " in$", " &$", " apud$")
  bad_patt <- paste0(bad_preps, collapse = "|")
  bad_names <- grep(bad_patt, res[[2]], perl = TRUE)
  if (length(bad_names) > 0L) {
    taxa2check <- res[[2]][bad_names]
    taxa2check <- gsub(bad_patt, "", taxa2check, perl = TRUE)

    too_many_names <-
      stringr::str_count(taxa2check, stringr::fixed(" ")) > 3
    if (any(too_many_names)) {
      pos <- regexpr("^(?:(?:[^\\s]+ ){4})([^\\s]+)",
                     taxa2check[too_many_names], perl = TRUE)
      pos_start <- attributes(pos)$capture.start
      good_match <- pos_start > 0
      if (any(!good_match))
        pos_start[!good_match] <-
          nchar(taxa2check[too_many_names])[!good_match]

      pos_start[good_match] <- pos_start[good_match] - 2
      taxa2check[too_many_names] <-
        substring(taxa2check[too_many_names], 0, pos_start)
    }

    still_bad <- grep("\\.$", taxa2check, perl = TRUE)
    if (length(still_bad) > 0L) {
      tax_split <- strsplit(taxa2check[still_bad], " ", fixed = TRUE)
      any_rank <- sapply(tax_split, function(x) any(x %in% ranks))
      paste_names <-
        function(x, y) return(paste(x[1:y], collapse = " "))
      if (any(any_rank)) {
        which_rank <- lapply(tax_split[any_rank],
                            function(x) which(x %in% ranks))
        last_name <- unlist(which_rank) + 1
        new_names <- unlist(
          mapply(paste_names, tax_split[any_rank], last_name, SIMPLIFY = FALSE))
        taxa2check[still_bad][any_rank] <- new_names
      } else {
        which_bad <- lapply(tax_split[!any_rank],
                                       function(x) grep("\\.$", x, perl = TRUE))
        last_name <- unlist(which_bad) - 1
        new_names <- unlist(
          mapply(paste_names, tax_split[!any_rank], last_name, SIMPLIFY = FALSE))
        taxa2check[still_bad][!any_rank] <- new_names
      }
    }
    res[[2]][bad_names] <- taxa2check
    res[[3]][bad_names] <- NA
  }

  no_auth <- res[[2]] %in% NA
  ranks_patt <- paste0(" ", c(ranks, "sp.", "spp."), "$",collapse = "|")
  ranks_patt <- gsub("\\.", "\\\\.", ranks_patt, perl = TRUE)
  low_names <- stringr::str_count(taxa1[no_auth],
                                  stringr::regex(" [a-z]"))
  low_authors <- (low_names > 1 |
                    grepl("\\.$", taxa1[no_auth], perl = TRUE)) &
                      !grepl(ranks_patt, taxa1[no_auth], perl = TRUE)
  if (any(low_authors)) {
    tax2check <- taxa1[no_auth][low_authors]
    too_many_names <- low_names[low_authors] > 3

    if (any(too_many_names)) {
      pos <- regexpr("^(?:(?:[^\\s]+ ){4})([^\\s]+)",
                     tax2check[too_many_names], perl = TRUE)
      pos_start <- attributes(pos)$capture.start
      good_match <- pos_start > 0
      if (any(!good_match))
        pos_start[!good_match] <-
        nchar(taxa1[no_auth][too_many_names])[!good_match]

      pos_start[good_match] <- pos_start[good_match] - 2
      tax2check[too_many_names] <-
        substring(tax2check[too_many_names], 0, pos_start)
      res[[4]][no_auth][low_authors][too_many_names] <- "yes"
    }

    if (any(!too_many_names)) {
      tax_split <- strsplit(tax2check[!too_many_names], " ",
                            fixed = TRUE)
      any_rank <- sapply(tax_split, function(x) any(x %in% ranks))
      paste_names <-
        function(x, y) return(paste(x[1:y], collapse = " "))
      if (any(any_rank)) {
        which_rank <- lapply(tax_split[any_rank],
                             function(x) which(x %in% ranks))
        last_name <- unlist(which_rank) + 1
        new_names <- unlist(
          mapply(paste_names, tax_split[any_rank], last_name, SIMPLIFY = FALSE))
        tax2check[!too_many_names][any_rank] <- new_names
        res[[4]][no_auth][low_authors][!too_many_names][any_rank] <-
          "yes"
      }

      if (any(!any_rank)) {
        pos <- regexpr("^(?:(?:[^\\s]+ ){2})([^\\s]+)",
                       tax2check[!too_many_names][!any_rank], perl = TRUE)
        pos_start <- attributes(pos)$capture.start
        good_match <- pos_start > 0
        pos_start[good_match] <- pos_start[good_match] - 2

        if (any(!good_match)) {
          pos_start[!good_match] <-
            regexpr("\\s\\p{Ll}",
                    tax2check[!too_many_names][!any_rank],
                    perl = TRUE)[!good_match] - 1
        }

        tax2check[!too_many_names][!any_rank] <-
          substring(tax2check[!too_many_names][!any_rank], 0, pos_start)
        res[[4]][no_auth][low_authors][!too_many_names][!any_rank] <-
          "yes"
      }
    }

    res[[2]][no_auth][low_authors] <- tax2check
  }

  ranks_patt <- paste0(" ", ranks, " ", collapse = "|")
  ranks_patt <- gsub("\\.", "\\\\.", ranks_patt, perl = TRUE)
  check_ids <- which(grepl(ranks_patt, res[[3]], perl = TRUE) &
                       !grepl(" f\\. ex | f\\. & ", res[[3]], perl = TRUE))
  if (length(check_ids) > 0L) {
    subs_ids <- regexpr(ranks_patt, res[[3]][check_ids], perl = TRUE)
    auth.final <- substring(res[[3]][check_ids], 0, subs_ids - 1)
    sci.infra <- substring(res[[3]][check_ids], subs_ids + 1,
                           nchar(res[[3]][check_ids]))
    sci.final <- paste(res[[2]][check_ids],
                       sci.infra, sep = " ")
    res[[2]][check_ids] <- sci.final
    res[[3]][check_ids] <- auth.final
  }

  no_auth <- res[[2]] %in% NA | !res[[3]] %in% NA
  auth_ids <- which(!no_auth)
  if (length(auth_ids) > 0L) {
    orig_split <- strsplit(res[[1]][auth_ids], " ", fixed = TRUE)
    name_split <- strsplit(res[[2]][auth_ids], " ", fixed = TRUE)
    # char_diff <- function(x, y) return(x[!(x %in% y & !duplicated(x))])
    # char_diff <- function(x, y) return(x[!(x %in% y | duplicated(x))])
    char_diff <- function(x, y) return(x[!x %in% y & !x %in% ranks])
    name_diff <- mapply(char_diff, orig_split, name_split, SIMPLIFY = FALSE)
    auth.name <- sapply(name_diff, paste, collapse = " ")

    ranks_patt <- paste0(" ", ranks, " ", collapse = "|")
    ranks_patt <- gsub("\\.", "\\\\.", ranks_patt, perl = TRUE)
    check_ids <- which(grepl(ranks_patt, auth.name, perl = TRUE) &
                         !grepl(" f\\. ex | f\\. & ", auth.name, perl = TRUE))
    if (length(check_ids) > 0L) {
      subs_ids <- regexpr(ranks_patt, auth.name[check_ids], perl = TRUE)
      auth.final <- substring(auth.name[check_ids], 0, subs_ids - 1)
      sci.infra <- substring(auth.name[check_ids], subs_ids + 1,
                             nchar(auth.name[check_ids]))
      sci.final <- paste(res[[2]][auth_ids][check_ids],
                         sci.infra, sep = " ")
      res[[2]][auth_ids][check_ids] <- sci.final
      auth.name[check_ids] <- auth.final
    }

    auth.name[auth.name %in% ""] <- NA
    res[[3]][auth_ids] <- auth.name
  }

  fix_these <- res[[4]] %in% "yes" & !res[[3]] %in% ""
  if (any(fix_these))
    res[[3]][fix_these] <-
      gsub("^([a-z])", "\\U\\1", res[[3]][fix_these], perl = TRUE)

  no_auth <- res[[2]] %in% NA
  if (any(no_auth))
    res[[2]][no_auth] <- res[[1]][no_auth]

  if (any(first_hyb)) {
    res[[1]][first_hyb] <- taxa1[first_hyb]
    res[[2]][first_hyb] <- paste0(substr(taxa1[first_hyb], 0, 2),
                                  res[[2]][first_hyb])
  }

  res <- res[!duplicated(res$orig.name), ]

  res <- res[, -which(names(res) %in% "fix.author")]
  res1 <- dplyr::left_join(res0, res, by = "orig.name",
                           suffix = c(".x", ""))

  res1$tax.name[!rep_ids0] <- taxa[!rep_ids0]

  res2 <- res1[, c("orig.name", "tax.name", "tax.author")]

  return(res2)
}
