#' @title Name Matching Against Reference
#'
#' @description Performs the exact and fuzzy matching between two sets
#'   of names (input and reference), based on cleaned names or on
#'   indexes. To speed up computational time for larger datasets, the
#'   function can parallelize the matching processes and/or perform
#'   matching by initial letters.
#'
#' @param names a vector of input names to be compared against a
#'   reference
#' @param ref.names a vector of the names of reference
#' @param match.type type of match: exact, fuzzy or both. Defaults to
#'   both.
#' @param clean.names logical. Should spaces, punctuation, symbols and
#'   species characters be removed prior to name matching? Defaults to
#'   FALSE.
#' @param dist.method fuzzy matching algorithm to be passed on to
#'   `stringdist::amatch()`. Defaults to "jw" (i.e. Jaro-Winkler)
#' @param p numeric. Weight of the shared prefix when `method` is
#'   'jw'. Defaults to 0.1.
#' @param max.dist no match is returned when name distance is larger
#'   than \code{max.dist}. Note that this argument depends on the
#'   \code{dist.method} specified. Defaults to 0.25, which is a rather
#'   inclusive threshold.
#' @param split.letters logical. Should fuzzy matching be performed
#'   separately by the input and reference name initials? Defaults to
#'   FALSE
#' @param parallel logical. Should fuzzy matching be performed in
#'   parallel? Defaults to FALSE
#' @param cores integer. The number of cores for parallel execution.
#'   Two by default.
#' @param show.progress logical. Whether fuzzy matching progress
#'   should displayed. Defaults to FALSE
#'
#' @return a vector with the indices (i.e. positions) of the matches
#'   of the input \code{names} in \code{ref.names}
#'
#' @details
#'
#' By default, the argument 'clean.names' is FALSE, to increase
#' computaional speed. However, both exact and fuzzy matches are case,
#' space and punctuation sensitive. So, cleaning names (i.e. seting
#' 'clean.names' to TRUE) can substantially increase the number of
#' matches. See the internal function `cleanName()` on how this
#' cleaning is performed.
#'
#' If 'split.letters' is TRUE, then the matching is done by chunks
#' composed by the initial letter of both input and references names.
#' If 'split.letters' and 'parallel' are both TRUE, matching is also
#' parallelized which is important in terms of computational time when
#' dealing with larger vectors (i.e. above tens of thousands names).
#'
#' If 'split.letters' is FALSE and 'parallel' is TRUE, then the
#' matching is done by chunks of input names. The size of the chunks
#' is arbitrarily defined by the number defined in the argument
#' 'cores' (two by default). If 'split.letters' and 'parallel' is
#' FALSE, then the matching is done using all input and reference
#' names together.
#'
#' The 'parallel' option is currently only implemented for fuzzy
#' matches. For the majority of cases, exact matching should be quite
#' fast.
#'
#' @author Renato A. Ferreira de Lima
#'
#'
#' @examples
#' # example code
#' names <- c("Ailton", "Highlander", "Hell", "Hallo", "Agnes-Lee")
#' refs <- c("Agnes Lee", "Hell", "Hellinger", "Hello", "Hill", "Hilton")
#'
#' refs[nameMatching(names, refs)]
#' refs[nameMatching(names, refs, match.type = c("exact"))]
#' refs[nameMatching(names, refs, match.type = c("exact"), clean.names = FALSE)]
#' refs[nameMatching(names, refs, max.dist = 0.15)]
#' refs[nameMatching(names, refs, split = TRUE)]
#'
#'
#' @importFrom foreach %dopar% %do% foreach
#' @importFrom parallel detectCores makePSOCKcluster stopCluster
#' @importFrom stringdist amatch
#' @importFrom doSNOW registerDoSNOW
#' @importFrom utils setTxtProgressBar
#'
#'
#' @export
nameMatching <- function(names = NULL,
                          ref.names = NULL,
                          match.type = c("exact", "fuzzy"),
                          clean.names = FALSE,
                          dist.method = "jw",
                          p = 0.1,
                          max.dist = 0.25,
                          split.letters = FALSE,
                          parallel = FALSE,
                          cores = 2,
                          show.progress = FALSE) {

  if (is.null(ref.names)) {
    stop("Argument 'ref.names' cannot be empty", call. = FALSE)
  }

  if (length(ref.names) == 0) {
    stop("Argument 'ref.names' must be a character of length > 0", call. = FALSE)
  }

  if (!is.character(ref.names)) {
    stop("Argument 'ref.names' must be a character", call. = FALSE)
  }

  if (is.null(names)) {
    stop("Argument 'names' cannot be empty", call. = FALSE)
  }

  if (!is.character(names)) {
    stop("Argument 'names' must be a character", call. = FALSE)
  }

  if (length(names) == 0) {
    stop("Argument 'names' must be a character of length > 0", call. = FALSE)
  }

  all_match_type <- c("exact", "fuzzy")
  match.type <-
    match.arg(tolower(match.type), all_match_type, several.ok = TRUE)

  if (length(dist.method) > 1) {
    stop("Argument 'method' must be a character of length 1", call. = FALSE)
  }

  all_methods <- c("osa", "lv", "dl", "hamming", "lcs", "qgram",
                   "cosine", "jaccard", "jw", "soundex")
  dist.method <- match.arg(tolower(dist.method), all_methods)

  if (!is.numeric(cores)) {
    stop("Argument 'cores' must be an integer", call. = FALSE)
  }

  if (length(cores) != 1) {
    stop("Argument 'cores' must be an integer of length 1", call. = FALSE)
  }

  if (length(cores) > parallel::detectCores()) {
    stop("Argument 'cores' must equal or smaller than the number of cores in your machine", call. = FALSE)
  }

  if (clean.names) {
    names <- cleanName(names)
    ref.names <- cleanName(ref.names)
  }

  match_vector <- rep(NA, length(names))

  if ("exact" %in% match.type) {
    exact_match <- match(names, ref.names)
    match_vector[!is.na(exact_match)] <-
      exact_match[!is.na(exact_match)]
  }

  if ("fuzzy" %in% match.type) {

    not_exact <- is.na(match_vector)

    orig_ord <- seq_along(names[not_exact])
    df1 <- data.frame(id = orig_ord, name = names[not_exact])

    ref_ord <- seq_along(ref.names)
    df2 <- data.frame(ref_id = ref_ord, name = ref.names)

    if (split.letters) {
      tmp_names <- squish(gsub("^[a|c]f+\\.|^\u00d7|^x ", "", df1$name,
                                perl = TRUE, ignore.case = TRUE))
      letras <- substr(tmp_names, 1, 1)
      miss_letras <- unique(letras[!letras %in% c(letters, LETTERS)])

      if (!identical(miss_letras, character(0))) {
        rep_these <- letras %in% miss_letras
        letras[rep_these] <-
          substr(squish(substr(tmp_names[rep_these], 2, 3)), 1, 1)
      }
      list_df1 <- split(df1, f = tolower(letras))

      tmp_ref_names <- squish(gsub("^[a|c]f+\\.|^\u00d7|^x ", "", df2$name,
                                    perl = TRUE, ignore.case = TRUE))
      letras_ref <- substr(tmp_ref_names, 1, 1)
      miss_letras <- unique(letras_ref[!letras_ref %in% c(letters, LETTERS)])

      if (!identical(miss_letras, character(0))) {
        rep_these <- letras_ref %in% miss_letras
        letras_ref[rep_these] <-
          substr(squish(substr(tmp_ref_names[rep_these], 2, 3)), 1, 1)
      }
      list_df2 <- split(df2, f = tolower(letras_ref))
      list_df2 <- list_df2[names(list_df2) %in% names(list_df1)]

      if (parallel) {
        cl <- parallel::makePSOCKcluster(cores)
        doSNOW::registerDoSNOW(cl)
        message("Parallel running with ", cores, " cores")
      } else {
        cl <- NULL
      }

      if (parallel) {
        `%d%` <- foreach::`%dopar%`
      } else {
        `%d%` <- foreach::`%do%`
      }

      prog <- progressBar(show.progress = show.progress,
                                             max.pb = length(list_df1))
      opts <- prog$opts
      pb <- prog$pb
      x <- NULL
      output <-
        foreach::foreach(
          x = 1:length(list_df1),
          .combine = 'c',
          .options.snow = opts
        ) %d% {
          if (!parallel & show.progress)
            utils::setTxtProgressBar(pb, x)

          letra.x <- names(list_df1)[x]
          if (letra.x %in% names(list_df2)) {

            res <- stringdist::amatch(list_df1[[letra.x]][["name"]],
                                      list_df2[[letra.x]][["name"]],
                                      method = dist.method,
                                      p = p,
                                      maxDist = max.dist)

            res1 <- match(list_df2[[letra.x]]$ref_id[res], df2$ref_id)
          } else {
            res1 <- rep(NA, length(list_df1[[x]]$name))
          }
          res1
        }

      if(parallel) parallel::stopCluster(cl)
      if(show.progress) close(pb)

      df1_new <- do.call(rbind.data.frame, list_df1)
      output <- output[order(df1_new$id)]

    } else {

      if (parallel) {

        chunks <- seq_len(cores)
        chunks.int <- chunks[max(which(round(nrow(df1)/chunks, 0) == nrow(df1)/chunks))]
        if (chunks.int >= 2) {
          list_df1 <- split(df1, seq_len(chunks.int))
        } else {
          list_df1 <- suppressWarnings(split(df1, 2))
        }

        cl <- parallel::makePSOCKcluster(cores)
        doSNOW::registerDoSNOW(cl)
        message("Parallel running with ", cores, " cores")

        `%d%` <- foreach::`%dopar%`

        prog <- progressBar(show.progress = show.progress,
                            max.pb = length(list_df1))
        opts <- prog$opts
        pb <- prog$pb
        x <- NULL

        output <-
          foreach::foreach(
            x = 1:length(list_df1),
            .combine = 'c',
            .options.snow = opts
          ) %d% {
            if (!parallel & show.progress)
              utils::setTxtProgressBar(pb, x)

            res <- stringdist::amatch(list_df1[[x]][["name"]],
                                      df2[["name"]],
                                      method = dist.method,
                                      p = p,
                                      maxDist = max.dist)
            names(res) <- list_df1[[x]][["id"]]
            res
          }

        if (parallel) parallel::stopCluster(cl)
        if (show.progress) close(pb)

        # output <- output[order(df1$id)]
        output <- output[order(as.numeric(names(output)))]

      } else {
        output <- stringdist::amatch(df1[["name"]], df2[["name"]],
                                     method = dist.method,
                                     p = p,
                                     maxDist = max.dist)
      }
    }

    match_vector[not_exact] <- output
    return(match_vector)
  } else {
    return(match_vector)
  }

}
