#' @name accessory_tax
#' @aliases rmOpen
#' @aliases rmInfra
#' @aliases rmHyb
#' @aliases addRank
#'
#' @title Accessory, internal functions for taxonomic manipulation
#'
#' @description These accessory functions work for editing the
#'   notation of scientific names and are mainly used within
#'   __plantR__ function `fixSpecies()`.
#'
#' @details The functions `rmOpen()`, `rmInfra()`, and `rmHyb()`
#'   require only a vector of scientific names, while the function
#'   `addRank()` also requires a `rank` to be provided.
#'
#'   The function `rmOpen()` removes the open nomenclature 'cf.' and
#'   'aff.'.
#'
#'   The function `rmInfra()` removes the infra-specific ranks from
#'   varieties, sub-species and forms (e.g. 'var.', 'subsp.', 'f.')
#'
#'   The function `rmHyb()` removes the hybrid symbol 'x'.
#'
#'   The function `addRank()` does the opposite operation: it adds
#'   ranks, open nomenclature or hybrid symbols into scientific names.
#'   For this function if the number of ranks is equal to the number
#'   of names provided each rank is assigned to the corresponding
#'   name. If the number of ranks is different, the function silently
#'   uses the first rank for all names. The function always adds the
#'   rank between the last two names, independently if the name is a
#'   binomial, trinomial or quadrinomial.
#'
#' @param x a vector with scientific names to be standardized.
#' @param rank the expression or symbol to be added between names.
#' @param pretty.hyb should the hybrid symbol be added close to the
#'   epiteth? Defaults to FALSE
#'
#' @author Sara Mortara & Renato A. Ferreira de Lima
#'
#' @keywords internal
#'
#' @rdname accessory_tax
#'
#' @seealso
#'  \link[plantR]{fixCase}.
#'
#' @examples
#' \dontrun{
#' rmOpen(c("Lindsaea cf. lancea", "Lindsaea aff. lancea"))
#' }
#'
rmOpen <- function(x) {

  aff_string <- "^aff\\.|^aff | aff\\. | aff "
  cf_string <- "^cf\\.|^cf | cf\\. | cf "
  aff_cf <- paste(aff_string, cf_string, sep = "|")

  x_new <- gsub(aff_cf, " ", x, perl = TRUE, ignore.case = TRUE)

  x_new <- squish(x_new)

  return(x_new)
}

#'
#' @rdname accessory_tax
#' @examples
#' \dontrun{
#' rmInfra(c("Lindsaea lancea var. angulata",
#'           "Lindsaea lancea (L.) Bedd. var. angulata Rosenst.",
#'           "Lindsaea schomburgkii f. coriifolia (Lindm.) K.U. Kramer"))
#' }
#'
rmInfra <- function(x) {

  form_string <- " f\\. | form\\. | fo\\. | forma | f "
  subsp_string <-  " ssp\\.| subsp\\.| subsp | ssp "
  var_string <- " var\\.| var "
  subsp_var <- paste(subsp_string, var_string, form_string, sep = "|")

  infra <- regexpr(subsp_var, x, perl = TRUE, ignore.case = TRUE)
  to_rm <- infra > 0
  to_rm[is.na(to_rm)] <- FALSE

  if (any(to_rm)) {
    x1 <- x[to_rm]
    pos_last <- infra[to_rm]
    start <- strtrim(x1, pos_last - 1)

    all_caps <- start == toupper(start)
    if (any(all_caps))
      start[all_caps] <- tolower(start[all_caps])

    start <- fixAuthors(start)$tax.name
    end <- substring(x1, pos_last)
    end <- gsub(subsp_var, " ", end, perl = TRUE, ignore.case = TRUE)

    if (any(all_caps))
      start[all_caps] <- toupper(start[all_caps])

    x[to_rm] <- squish(paste(start, end))
  }
  return(x)
}
#'
#' @rdname accessory_tax
#' @examples
#' \dontrun{
#' rmHyb(c("Blechnum ×antillanum", "Blechnum × antillanum",
#'        "Blechnum x antillanum", "Blechnum X antillanum",
#'        "× Blechnum antillanum"))
#' }
#'
rmHyb <- function(x)  {

  hyb_string <- " x |\u00d7 | \u00d7 | \u00d7(?=[[:alpha:]])"

  x_new <- gsub(hyb_string, " ", x, perl = TRUE, ignore.case = TRUE)

  return(squish(x_new))
}
#'
#' @rdname accessory_tax
#' @examples
#' \dontrun{
#' addRank(c("Lindsaea lancea", "Lindsaea lancea angulata",
#'           "Lindsaea schomburgkii coriifolia", "Blechnum antillanum"),
#'           rank = c("cf.", "var.", "f.", "\u00d7"))
#' }
#'
#'
addRank <- function(x, rank = NULL, pretty.hyb = FALSE) {

  if (is.null(rank))
    stop("please chose a rank, symbol or character to be added")

  if (length(x) != length(rank))
    rank <- rep(rank[1], length(x))

  spaces <- attributes(regexpr(".*\\s", x, perl = TRUE))$match.length
  to_add <- spaces > 0
  to_add[is.na(to_add)] <- FALSE

  if (any(to_add)) {
    x1 <- x[to_add]
    pos_last <- spaces[to_add]
    start <- strtrim(x1, pos_last - 1)
    end <- substring(x1, pos_last + 1)

    x[to_add] <- paste(start, rank[to_add], end)
  }

  if (pretty.hyb) {
    check_these <- rank %in% "\u00d7"
    if (any(check_these))
      x <- gsub(" \u00d7 ", " \u00d7", x, perl = TRUE)
  }

  return(x)
}
