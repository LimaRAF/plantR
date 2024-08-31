#' @title Get Unique Taxon Names
#'
#' @description Organize unique and multiple name matches in a reference
#'   backbone for taxon names without author
#'
#' @param df a data frame containing the input names
#' @param df.ref a data frame containing the reference names
#' @param match.col a vector with the name of the column in both
#'   datasets to be used for matching
#' @param orig.col a vector with the name of the column with the
#'   original name in the input data frame 'df'. Defaults to
#'   "scientificName.new" which is the output of the `plantR` function
#'   `fixSpecies()`
#' @param name.col a vector with the name of the column with the taxon
#'   name in the data frame 'df.ref'. Defaults to 'name'.
#' @param status.col a vector with the name of the column with the
#'   taxonomic status in the reference data frame 'df.ref'. Defaults to
#'   'taxon.status'.
#' @param type.match.col a vector with the name of the column with the
#'   type of name match. Defaults to 'match_type'.
#' @param mult.match.col a vector with the name of the column with the
#'   information on if there was a unique or a multiple name match.
#'   Defaults to "multiple_match"
#' @param agg.cols a vector with the names of the columns that should
#'   be concatenated in the case all matches are returned in the same
#'   line (see argument 'mult.matches')
#' @param mult.matches a character of how to deal with multiple
#'   matches. Options are 'all' (concatenate all matches in the same
#'   line) and 'best' (only one name match is returned per line).
#'   Defaults to 'all'.
#'
#' @details
#'  To obtain the best match the column defined in the argument
#'  'status.col' is used to organize the results and then remove all
#'  duplicated names. The priority order to name with the best match
#'  is (from high to low priority): 'accepted', 'synonym',
#'  'unchecked', 'unplaced' and '' (i.e. empty taxonomic status).
#'
#' @author Renato A. Ferreira de Lima
#'
#' @importFrom dplyr left_join
#'
#' @keywords internal
#'
getTaxUnique <- function(df = NULL, df.ref = NULL,
                         match.col = NULL,
                         orig.col = "scientificName.new",
                         name.col = "name",
                         status.col  = "taxon.status",
                         type.match.col = "match_type",
                         mult.match.col = "multiple_match",
                         mult.matches = "all",
                         agg.cols = c("id", "family", "authorship",
                                      "taxon.rank", "name.status",
                                      "taxon.status", "accepted.name",
                                      "accepted.authorship",
                                      "accepted.taxon.rank",
                                      "accepted.taxon.status",
                                      "accepted.name.status")) {

  if (!inherits(df, "data.frame"))
    stop("Argument 'df' must be a data frame", call. = FALSE)

  if (dim(df)[1] == 0)
    stop("Input data frame canot be empty!", call. = FALSE)

  if (!inherits(df.ref, "data.frame"))
    stop("Argument 'df.ref' must be a data frame", call. = FALSE)

  if (dim(df.ref)[1] == 0)
    stop("Reference data frame canot be empty!", call. = FALSE)

  if (!orig.col %in% names(df))
    stop("Input data frame must have a column named: ", orig.col,
         call. = FALSE)

  if (!status.col %in% names(df.ref))
    stop("Reference data frame must have a column named: ", status.col,
         call. = FALSE)

  if (!(match.col %in% names(df) & match.col %in% names(df.ref)))
    stop("Both input and reference data frame must have a column named: ",
         match.col, call. = FALSE)

  df.orig <- df
  df <- df[!duplicated(df[[match.col]]), ]

  rep_these <- df.ref[[status.col]] %in% ""
  if (any(rep_these))
    df.ref[[status.col]][rep_these] <- "unplaced1"

  rep_these <- df.ref[[status.col]] %in% "doubtful"
  if (any(rep_these))
    df.ref[[status.col]][rep_these] <- "unplaced"

  # exact macthes (based on cleaned names)
  res <- as.data.frame(dplyr::left_join(df, df.ref,
                                        by = match.col,
                                        suffix = c(".x", "")))
  res$tmp..ordem <- seq_len(dim(res)[1])

  dup_names <- duplicated(res[[orig.col]]) |
                duplicated(res[[orig.col]], fromLast = TRUE)

  if (any(!dup_names)) {
    # unique matches
    unique_spp <- res[[orig.col]][!dup_names]
    res_unique <-
      res[res[[orig.col]] %in% unique_spp, ]
    res_unique[[mult.match.col]] <- FALSE

    # multiple matches
    if (any(dup_names)) {
      duplicated_spp <- res[[orig.col]][dup_names]
      res_dup <-
        res[res[[orig.col]] %in% unique(duplicated_spp), ]
      res_dup[[mult.match.col]] <- TRUE

      if (mult.matches == "all") {
        tmp0 <- res_dup[, agg.cols]
        tmp <- aggregate(tmp0, list(res_dup[[orig.col]]),
                         function(x) paste(unique(x[!is.na(x)]),
                                           collapse = "|"))
        names(tmp)[1] <- orig.col

        res_dup <- res_dup[order(res_dup[[status.col]],
                                           decreasing = FALSE),]
        tmp1 <- res_dup[!duplicated(res_dup[[orig.col]]),]
        tmp1 <- tmp1[-which(names(tmp1) %in% agg.cols)]
        res_dup1 <- dplyr::left_join(tmp, tmp1,
                                         by = orig.col)
      }

      if (mult.matches == "best") {
        res_dup <- res_dup[order(res_dup[[status.col]],
                                           decreasing = FALSE),]
        res_dup1 <-
          res_dup[!duplicated(res_dup[[orig.col]]),]
      }

      res1 <- rbind.data.frame(res_unique,
                               res_dup1[, names(res_unique)])
      res1 <- res1[order(res1$tmp..ordem),]
      res1 <- res1[, -which(names(res1) %in% "tmp..ordem")]
      # rm_cols <- c(tax.names[2], match.type.col)
      rm_cols <- type.match.col
      res2 <-  as.data.frame(
        dplyr::left_join(df,
                         res1[, -which(names(res1) %in% rm_cols)],
                         by = orig.col,
                         suffix = c(".x", "")))
      res.final <- res2[, !grepl(".x", names(res2), fixed = TRUE)]

    } else {
      res2 <- res_unique[order(res_unique$tmp..ordem),]
      res2 <- res2[, -which(names(res2) %in% "tmp..ordem")]
      res.final <- res2[, !grepl(".x", names(res2), fixed = TRUE)]
    }
  } else {

    duplicated_spp <- res[[orig.col]][dup_names]
    res_dup <-
      res[res[[orig.col]] %in% unique(duplicated_spp), ]
    res_dup[[mult.match.col]] <- TRUE

    if (mult.matches == "all") {
      tmp0 <- res_dup[, agg.cols]
      tmp <- aggregate(tmp0, list(res_dup[[orig.col]]),
                       function(x) paste(unique(x[!is.na(x)]),
                                         collapse = "|"))
      names(tmp)[1] <- orig.col

      res_dup <- res_dup[order(res_dup[[status.col]],
                               decreasing = FALSE),]
      tmp1 <- res_dup[!duplicated(res_dup[[orig.col]]),]
      tmp1 <- tmp1[-which(names(tmp1) %in% agg.cols)]
      res_dup1 <- dplyr::left_join(tmp, tmp1,
                                   by = orig.col)
    }

    if (mult.matches == "best") {
      res_dup <- res_dup[order(res_dup[[status.col]],
                               decreasing = FALSE),]
      res_dup1 <-
        res_dup[!duplicated(res_dup[[orig.col]]),]
    }

    res1 <- res_dup1[order(res_dup1$tmp..ordem),]
    res1 <- res1[, -which(names(res1) %in% "tmp..ordem")]
    # rm_cols <- c(tax.names[2], match.type.col)
    rm_cols <- type.match.col
    res2 <-  as.data.frame(
      dplyr::left_join(df,
                       res1[, -which(names(res1) %in% rm_cols)],
                       by = orig.col,
                       suffix = c(".x", "")))
    res.final <- res2[, !grepl(".x", names(res2), fixed = TRUE)]
  }

  res.temp <- dplyr::left_join(df.orig, res.final, by = match.col,
                               suffix = c(".x", ""))
  cols_rep <- unique(c(mult.match.col, agg.cols))
  df.orig[, cols_rep] <- res.temp[, cols_rep]
  df.orig[[name.col]] <- res.temp[[match.col]]

  return(df.orig)
}
