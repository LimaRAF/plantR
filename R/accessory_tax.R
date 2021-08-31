#' @name accessory_tax
#' @aliases rmOpen
#' @aliases rmInfra
#' @aliases rmHyb
#' @aliases addRank
#'
#' @title Accessory, internal functions for taxonomic manipulation
#'
#' @description These accessory functions work for editing the notation of
#'   scientific names and are mainly used within __plantR__ function
#'   `fixSpecies()`.
#'
#' @details The functions `rmOpen()`, `rmInfra()`, and `rmHyb()` require only a
#'   vector of scientific names, while the function `addRank()` also requires a
#'   `rank` to be provided.
#'
#'   The function `rmOpen()` removes the open nomenclature 'cf.' and 'aff.'.
#'
#'   The function `rmInfra()` removes the infra-specific ranks from
#'   varieties, sub-species and forms (e.g. 'var.', 'subsp.', 'f.')
#'
#'   The function `rmHyb()` removes the hybrid symbol 'x'.
#'
#'   The function `addRank()` does the opposite opperation: it adds ranks, opena
#'   nomenclature or hybrid symbols into scientific names. For this function if
#'   the number of ranks is equal to the number of names provided each rank is
#'   assigned to the corresponding name. If the number of ranks is different,
#'   the function silently uses the first rank for all names.
#'
#' @param x a vector with scientific names to be standardized.
#' @param rank the expression or symbol to be added between names.
#'
#' @author Sara Mortara & Renato A. F. de Lima
#'
#' @keywords internal
#'
#' @rdname accessory_tax
#'
#' @seealso
#'  \link[plantR]{fixCase}.
#'
#' @importFrom stringr str_replace fixed regex str_split str_squish str_count
#'
#' @examples
#' \dontrun{
#' rmOpen(c("Lindsaea cf. lancea","Lindsaea aff. lancea"))
#' }
#'
rmOpen <- function(x) {

    aff_string <- "^aff\\.|^aff[[:space:]]|[[:space:]]aff\\.[[:space:]]|[[:space:]]aff[[:space:]]"
    cf_string <- "^cf\\.|^cf[[:space:]]|[[:space:]]cf\\.[[:space:]]|[[:space:]]cf[[:space:]]"
    aff_cf <- paste(aff_string, cf_string, sep = "|")

    x_new <- stringr::str_replace(x, stringr::regex(aff_cf, ignore_case = TRUE), " ")

    x_new <- stringr::str_squish(x_new)

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

  form_string <- "[[:space:]]f\\.[[:space:]]|[[:space:]]form\\.[[:space:]]"
  subsp_string <-  "[[:space:]]ssp\\.|[[:space:]]subsp\\.|[[:space:]]subsp[[:space:]]|[[:space:]]ssp[[:space:]]"
  var_string <- "[[:space:]]var\\.|[[:space:]]var[[:space:]]"
  subsp_var <- paste(subsp_string, var_string, form_string, sep = "|")

  split <- stringr::str_split(x, stringr::regex(subsp_var, ignore_case = TRUE))

  # split.mat <- stringi::stri_list2matrix(split, byrow = TRUE)
  split.mat <- t(sapply(split, "[", i = 1:2))
  if (dim(split.mat)[2] == 0) {
    return(NA_character_)
  } else {
    split.mat[, 2] <- stringr::str_squish(split.mat[, 2])
    infra_authors <- stringr::str_count(split.mat[, 1], stringr::fixed(" ")) > 1
    split.mat[infra_authors, 1] <- gsub(" [A-Z].*| \\(.*", "",
                                      split.mat[infra_authors, 1], perl = TRUE)
    x_new <- paste(split.mat[, 1], split.mat[, 2])

    return(x_new)
  }
}
#'
#' @rdname accessory_tax
#' @examples
#' \dontrun{
#' rmHyb(c("Blechnum ×antillanum", "Blechnum × antillanum",
#'        "Blechnum x antillanum", "Blechnum X antillanum"))
#' }
#'
rmHyb <- function(x)  {

  hyb_string <- "[[:space:]]x[[:space:]]|[[:space:]]\u00d7[[:space:]]|[[:space:]]\u00d7(?=[[:alpha:]])"

  x_new <- stringr::str_replace(x, stringr::regex(hyb_string, ignore_case = TRUE), " ")

  x_new <- gsub("\\s+", " ", x_new, perl = TRUE)

  return(x_new)
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
addRank <- function(x, rank = NULL) {

  if (is.null(rank))
    stop("please chose a rank, symbol or character to be added")

  if (length(x) != length(rank))
    rank <- rep(rank[1], length(x))

  split <- stringr::str_split(x, stringr::fixed(" "))
  n.str <- lengths(split)
  n.max <- suppressWarnings(max(n.str, na.rm = TRUE))
  split.mat <- t(sapply(split, "[", i = 1:n.max))


  if (n.max >= 3)
    split.mat[, 3] <- do.call(paste, as.data.frame(split.mat[, 3:n.max]))

  x_new <- x

  if (any(n.str == 2))
    x_new[n.str == 2] <- paste(split.mat[n.str == 2, 1],
                             rank[n.str == 2],
                             split.mat[n.str == 2, 2])
  if (any(n.str >= 3))
    x_new[n.str >= 3] <- paste(split.mat[n.str >= 3, 1],
                             split.mat[n.str >= 3, 2],
                             rank[n.str >= 3],
                             split.mat[n.str >= 3, 3], sep = " ")
  x_new <- gsub(" NA NA NA$| NA NA$| NA$", "", x_new, perl = TRUE)

  if (any(rank %in% c("\u00d7", "x")))
    x_new <- gsub(paste0(" ", rank, " "), paste0(" ", rank), x_new, perl = TRUE)

  return(x_new)
}
