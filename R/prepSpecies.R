#' @title Get Valid Taxon Names
#'
#' @description Provides valid taxon names or corrects typos and
#'   orthographic variants based on taxonomic backbones provided by
#'   the user or by the `plantR` companion packages `plantRdata` (the
#'   default).
#'
#' @param x a vector or data.frame of taxon names and their
#'   authorships, ideally the output from function `fixSpecies()`.
#' @param tax.names character. Names of the columns containing the
#'   taxon names and authorships. Defaults to 'scientificName.new' and
#'   'scientificNameAuthorship.new'.
#' @param db the taxonomic backbones to be consulted for valid names,
#'   in their preferred order of priority. By default, only the
#'   Brazilian Flora 2020 taxonomic backbone ('bfo') is used.
#' @param use.authors logical. Should authorships be used in the name
#'   matching process? Default to TRUE.
#' @param mult.matches a character declaring how to deal with multiple
#'   name matches. Options are 'all' (concatenate all matches in the
#'   same line) and 'best' (only one name match is returned per line).
#'   Defaults to 'all'.
#' @param replace.names logical. If taxon names should be replaced
#'   based on the results of the name checking (e.g. if synonyms
#'   should be replaced by its accepted name. Default to TRUE.
#' @param sug.dist a fraction expressing the maximum distance allowed
#'   between the taxon name and the suggested names in the backbone,
#'   which is passed to the argument `max.dist` of the function
#'   `nameMatching()`. Defaults to 0.9 (i.e. maximum of 10%
#'   difference).
#' @param clean.indet logical. Should the markers and identifiers of
#'   unidentified species (e.g. sp. or spp.) be removed prior to name
#'   matching? Defaults to TRUE.
#' @inheritParams nameMatching
#' @param drop.cols character. Name of columns from the reference
#'   backbone that should be dropped from the results.
#'
#' @return
#' A data frame with the same columns provided by the user and some
#' additional columns containing the suggested taxon name
#' ('suggestedName'), taxon authorship ('suggestedAuthorship'), the
#' corresponding scientific name ('scientificNameFull'), the taxonomic
#' information related to the suggested name (columns 'taxon.rank',
#' tax.notes' and its 'id' in the reference taxonomic backbone).
#'
#' @details
#'   The function first checks taxon names with authorship (if
#'   available), using both exact and fuzzy matching. This is good
#'   practice to avoid multiple matches in the reference backbone
#'   related to homonyms. If a name below the threshold distance
#'   defined in argument `sug.dist` is not found, then the function
#'   does another round of exact and fuzzy matching using only the
#'   taxon names without authorship.
#'
#'   The name check compares all unique names in `x` with all names in
#'   the taxonomic backbones selected. So, the speed of the checking
#'   process depends on the number of name combinations in `x` and in
#'   the backbone, besides the computer's operational system and
#'   specifications. But if the backbone selected has many names (over
#'   hundreds of thousands of names), such as those containing all
#'   plant names for the entire world, the process may take up to a
#'   minute to check a dozen of names.
#'
#'   But if you have many thousands of input names to check and a
#'   large backbone, the function offers the possibility of doing the
#'   name matching by initial letters and/or using parallelization.
#'   See the help of function `nameMatching` which is used internally.
#'
#'   The result of the name check will vary depending on the
#'   completeness of the backbone. And do expect some false positive
#'   matches, e.g. input names returning as misspelled but that
#'   actually are not in the reference backbone (e.g. matching of
#'   exotic names against a local or regional reference backbone). If
#'   this is the case, one should use more restrictive values of
#'   `sug.dist` than the default of 0.9. See examples for an
#'   illustration of these differences.
#'
#'   Currently, there are two options: (1) using the `plantR` internal
#'   backbone for the Brazilian Vascular Flora (the default) or (2)
#'   using user-provided taxonomic backbones. The choice between these
#'   options is declared via the argument `db`, which is passed on to
#'   the function `getTaxBackbone()`.
#'
#'   The companion R package `plantRdata` provides different global
#'   and regional taxonomic backbones in the exact format expected by
#'   `prepSpecies()`. See the help of function `getTaxBackbone()` for
#'   more details on this expected format.
#'
#'   Previous versions of this function used packages __flora__ and
#'   __Taxonstand__ to provide suggested names. These packages are no
#'   longer used, but the function default is to compare names against
#'   the Brazilian Flora Online, the same backbones used by the
#'   package __flora__.
#'
#'   The function does not provide the suggested names from the
#'   different backbones in separate columns. It combines into
#'   the same column ('suggestedName') the suggestions from one or
#'   more backbone. If more than one database is used, the suggested
#'   names are combined following the same order of the database codes
#'   provided in the argument `db`.
#'
#'   If `replace.names` is TRUE, the function replaces synonyms with
#'   its accepted name and replaces bad matches by NA. In the rare
#'   cases where multiple name matches have the same accepted name,
#'   the accepted name for both names is also returned. But note that
#'   the 'id' found in the reference
#'
#'   The function returns a new column named 'tax.notes' which
#'   contains a summary of the result of the name matching for each
#'   taxon name. See the details of the function `getTaxNotes` for
#'   more details on those categories.
#'
#'   By default, the argument `mult.matches` returns all possible
#'   names matched in the taxonomic backbone (mult.matches = 'all'),
#'   which is especially important if the authorship is not provided.
#'   If `mult.matches` is 'best', only one name is returned. Ideally,
#'   this is the accepted name within the multiple matches in the
#'   backbone. Result is a lot cleaner but in many cases this does not
#'   means that the accepted name returned actually is the right
#'   accepted for the input name. So, users should be extra careful
#'   when setting `mult.matches` to 'best'.
#'
#' @author Renato A. Ferreira de Lima
#'
#'
#' @examples
#' sp.list <- c("Casearia sylvestris","Casearia sylvestris",
#'   "Casearia sylvestirs", "Casearia silvestris",
#'   "Casearia sylvestris var. angustifolia", "Casearia attenuata",
#'   "Casearia celtidifolia","Casearia celtidifolia","Casearia celtidifolia",
#'   "Casearia serrulata", "Casearia serrulata", "Casearia serrulata",
#'   "Xylosma ciliatifolium", "Casearia tropicana")
#' aut_list <- c("Sw.", "", "Sw.", "", "Uittien", "Rusby",
#'   "", "Kunth", "Poepp. Eichler", "", "J. Seber ex Griseb.", "Sw.",
#'   "(Clos) Eichler", "L.")
#'
#' df <- data.frame(scientificName.new = sp.list,
#'   scientificNameAuthorship.new = aut_list)
#'
#' prepSpecies(df)
#' prepSpecies(df, clean.names = TRUE)
#' prepSpecies(df, sug.dist = 0.925)
#' prepSpecies(df, mult.matches = "best")
#'
#'
#' @seealso
#'  Functions \link[plantR]{getTaxBackbone},
#'  \link[plantR]{nameMatching} and \link[plantR]{getTaxNotes}.
#'
#' @importFrom stringdist stringdist
#'
#' @export prepSpecies
#'
prepSpecies <- function(x,
                        tax.names = c("scientificName.new",
                                      "scientificNameAuthorship.new"),
                        db = "bfo",
                        use.authors = TRUE,
                        replace.names = TRUE,
                        mult.matches = "all",
                        sug.dist = 0.9,
                        clean.indet = TRUE,
                        clean.names = FALSE,
                        dist.method = "jw",
                        split.letters = FALSE,
                        parallel = FALSE,
                        cores = 2,
                        show.progress = FALSE,
                        drop.cols = c("match_type", "multiple_match",
                                      "fuzzy_dist_name",
                                      "fuzzy_dist_author",
                                      "name.status", "taxon.status",
                                      "accepted.id", "accepted.name",
                                      "accepted.authorship",
                                      "accepted.taxon.rank",
                                      "accepted.taxon.status",
                                      "accepted.name.status",
                                      "taxon.distribution")) {

  if (!inherits(x, "data.frame")) {
    if (inherits(x, "character")) {
      x <- data.frame(name = x, author = NA)
      colnames(x) <- tax.names
    } else {
      stop("Input object 'x' needs to be a data frame or a vector!",
           call. = FALSE)
    }
  }

  if (dim(x)[1] == 0)
    stop("Input data frame is empty!", call. = FALSE)

  if (!tax.names[1] %in% names(x))
    stop("Input data frame must have a column with the species name",
         call. = FALSE)

  if (is.na(tax.names[2])) {
    warning("Column name with species authorities not provided; setting to 'scientificNameAuthorship'")
    tax.names[2] <- 'scientificNameAuthorship.new'
  }

  if (sug.dist < 0 | sug.dist >=1) {
    if (dist.method %in% c("jw"))
      stop("Argument 'sug.dist' must be between 0 and 1",
           call. = FALSE)
  }

  # creating the input data frames
  if (tax.names[2] %in% names(x)) {

    if (all(is.na(x[[tax.names[2]]]))) {
      if (use.authors) {
        use.authors <- FALSE
        warning("Argument 'use.authors' set as TRUE, but column with author names is empty",
                call. = FALSE)
      }
      df <- cbind.data.frame(
        tmp..ordem = seq_along(x[[tax.names[1]]]),
        x[, tax.names])
      df[[3]][df[[3]] %in% c("", "NA", NULL, " ")] <- NA
    } else {
      df <- cbind.data.frame(
        tmp..ordem = seq_along(x[[tax.names[1]]]),
        x[, tax.names])
      df[[3]][df[[3]] %in% c("", "NA", NULL, " ")] <- NA
    }

  } else {
    if (use.authors) {
      use.authors <- FALSE
      warning("Argument 'use.authors' set as TRUE, but column with author names not found",
              call. = FALSE)
    }
    df <- cbind.data.frame(
      tmp..ordem = seq_along(x[[tax.names[1]]]), x[, tax.names[1],
                                                   drop = FALSE])
    df[[tax.names[2]]] <- NA
  }

  # creating the reference dataframe: internal or user provided
  dbs <- getTaxBackbone(db)

  # storing the databases in the order of priority defined in 'db'
  results <- vector("list", length(dbs))
  names(results) <- names(dbs)

  # empty vector of notes
  final.status <- rep("check", dim(df)[1])

  for (i in seq_along(results)) {

    # preparing names
    ref.df <- dbs[[i]]
    if (use.authors) {
      df_unique <- unique(df[, tax.names])
      df_unique <- df_unique[!is.na(df_unique[[1]]), ]
      input_names <- buildName(df_unique, tax.names)

      no_author <- is.na(df_unique[[tax.names[2]]])
      if (any(no_author))
        input_names[no_author] <-
          df_unique[[tax.names[1]]][no_author]

      ref_names <- buildName(ref.df, c("name", "authorship"))

    } else {
      df_unique <- unique(df[, tax.names])
      input_names <- df_unique[[tax.names[1]]]
      no_author <- rep(TRUE, dim(df_unique)[1])
      ref_names <- ref.df[["name"]]
    }

    if (clean.indet) {
      search_names <- input_names
      w_indet <- grepl("\\ssp+\\.", input_names, perl = TRUE)
      if (any(w_indet)) {
        tmp_df_unique <- df_unique[w_indet, ]
        tmp_df_unique[[tax.names[1]]] <-
          sub("\\ssp+\\..*", "", tmp_df_unique[[tax.names[1]]],
              perl = TRUE)
        search_names[w_indet] <-
          tmp_df_unique[[tax.names[1]]]
        input_names[w_indet] <-
          buildName(tmp_df_unique, tax.names)
      }

      if (any(is.na(w_indet)))
        w_indet[is.na(w_indet)] <- FALSE

    } else {
      search_names <- input_names
      w_indet <- rep(FALSE, length(input_names))
    }

    if (clean.names) {
      input_names_clean <- cleanName(input_names)
      ref_names_clean <- cleanName(ref_names)
    } else {
      input_names_clean <- input_names
      ref_names_clean <- ref_names
    }

    match_type <- rep(NA, length(input_names_clean))

    # name matching with authorships
    bb_cols <- c("id", "family", "name", "authorship",
                 "taxon.rank", "name.status","taxon.status",
                 "accepted.id", "accepted.name", "accepted.authorship",
                 "accepted.taxon.rank", "accepted.taxon.status",
                 "accepted.name.status")
    bb_cols <- bb_cols[bb_cols %in% names(ref.df)]

    if (use.authors) {
      # perfect match
      perfect_match <- match(input_names, ref_names)
      check_these <- is.na(perfect_match)
      if (any(!check_these)) {
        match_type[!check_these] <-
          "exact_w_author"

        exact_no_authors <- no_author & !check_these
        if (any(exact_no_authors))
          match_type[no_author & !check_these] <-
          "exact_wout_author_in_backbone"

        exact_wout_indet <- w_indet & !check_these
        if (any(exact_wout_indet))
          match_type[w_indet & !check_these] <-
          "exact_w_author_wout_indet"
      }

      # perfect match with cleaned names
      if (clean.names) {
        if (any(check_these)) {
          perfect_match_clean <- match(input_names_clean, ref_names_clean)
          rep_these <- !is.na(perfect_match_clean) & is.na(perfect_match)

          if (any(rep_these)) {
            perfect_match[rep_these] <- perfect_match_clean[rep_these]
            match_type[rep_these] <- "exact_w_author_clean"
            match_type[no_author & rep_these] <-
              "exact_wout_author_in_backbone_clean"
          }
        }
      }

      # fuzzy matching
      max_dist = 1 - sug.dist
      check_these <- is.na(perfect_match) & !no_author
      if (any(check_these)) {
        fuzzy_match <- nameMatching(input_names_clean[check_these],
                                    ref_names_clean,
                                    match.type = "fuzzy",
                                    clean.names = FALSE,
                                    dist.method = dist.method,
                                    max.dist = max_dist,
                                    split.letters = split.letters,
                                    parallel = parallel,
                                    cores = cores,
                                    show.progress = TRUE)

        rep_these <- !is.na(fuzzy_match)
        if (any(rep_these)) {
          perfect_match[check_these] <- fuzzy_match
          match_type[check_these][rep_these] <-
            "fuzzy_w_author"

          fuzzy_wout_indet <- w_indet & check_these
          if (any(fuzzy_wout_indet)) {
            match_type[w_indet & check_these] <-
              "fuzzy_w_autor_wout_indet"
          }
        }
      }

      result <-
        cbind.data.frame(df_unique, ref.df[perfect_match, bb_cols])

      # multiple match column
      result$match_type <- match_type
      result$multiple_match <- NA
      rep_these <- result$match_type %in%  c("exact_w_author",
                                             "exact_wout_author_in_backbone",
                                             "exact_w_author_clean",
                                             "exact_wout_author_in_backbone_clean",
                                             "exact_w_author_wout_indet",
                                             "fuzzy_w_author",
                                             "fuzzy_w_autor_wout_indet")
      if (any(rep_these))
        result$multiple_match[rep_these] <- FALSE

      # fuzzy distance columns (taxon name and author)
      result$fuzzy_dist_name <- NA
      result$fuzzy_dist_author <- NA
      exatos <- c("exact_w_author",
                  "exact_w_author_clean",
                  "exact_w_author_wout_indet")
      rep_these <- result$match_type %in% exatos
      result$fuzzy_dist_name[rep_these] <-
        result$fuzzy_dist_author[rep_these] <- 0L

      check_these <- result$match_type %in% c("fuzzy_w_author",
                                              "fuzzy_w_autor_wout_indet")
      if (any(check_these)) {
        input_names_temp <- result[[tax.names[1]]][check_these]
        fuzzy_w_indet <- check_these & w_indet
        if (any(fuzzy_w_indet))
          input_names_temp[w_indet[check_these]] <-
          search_names[fuzzy_w_indet]

        name_dist <- stringdist::stringdist(input_names_temp,
                                            result[["name"]][check_these])
        result$fuzzy_dist_name[check_these] <-
          round(name_dist/nchar(input_names_temp), 4)
        aut_dist <- stringdist::stringdist(result[[tax.names[2]]][check_these],
                                           result[["authorship"]][check_these])
        result$fuzzy_dist_author[check_these] <-
          round(aut_dist/nchar(result[[tax.names[2]]][check_these]), 4)
      }
    } else {
      empty_df <- as.data.frame(matrix(NA,
                                nrow = dim(df_unique)[1],
                                ncol = length(bb_cols)))
      colnames(empty_df) <- bb_cols
      empty_df$match_type <- NA
      empty_df$multiple_match <- NA
      empty_df$fuzzy_dist_name <- NA
      empty_df$fuzzy_dist_author <- NA

      result <- cbind.data.frame(df_unique, empty_df)
    }

    # name matching without authorships
    if (any(no_author)) {

      if (clean.names) {
        ref_names_clean1 <- cleanName(ref.df[["name"]])
      } else {
        ref_names_clean1 <- ref.df[["name"]]
      }

      # exact matches
      no_authors_no_match <- no_author & is.na(result$id)
      if (any(no_authors_no_match)) {

        df1 <- result[no_authors_no_match, ]
        tmp.match.col <- "tmp.tax.name"

        if (clean.indet) {
          w_indet_fuzz <- no_authors_no_match & w_indet
          if (any(w_indet_fuzz)) {
            input_names_clean_indet <- search_names[w_indet_fuzz]
            df1[[tax.names[1]]][w_indet[no_authors_no_match]] <-
              input_names_clean_indet
          }
        }

        if (clean.names) {
          df1[[tmp.match.col]] <- cleanName(df1[[tax.names[1]]])
        } else {
          df1[[tmp.match.col]] <- df1[[tax.names[1]]]
        }

        ref.df[[tmp.match.col]] <- ref_names_clean1

        bb_cols1 <- bb_cols[!bb_cols %in% "name"]

        unique_tax <- getTaxUnique(df1, ref.df,
                                match.col = tmp.match.col,
                                orig.col = tax.names[1],
                                name.col = "name",
                                status.col = "taxon.status",
                                type.match.col = "match_type",
                                mult.match.col = "multiple_match",
                                mult.matches = mult.matches,
                                agg.cols = bb_cols1)

        check_these <- !is.na(unique_tax$id)
        if (any(check_these)) {
          result[no_authors_no_match, c(bb_cols, "multiple_match")] <-
            unique_tax[, c(bb_cols, "multiple_match")]
          result$match_type[no_authors_no_match] <- "exact_wout_author"
          result$fuzzy_dist_name[no_authors_no_match] <- 0L
        }

        double_check <- is.na(result$id)
        if (any(double_check)) {
          result$match_type[double_check] <-
            result$multiple_match[double_check] <-
            result$fuzzy_dist_name[double_check] <- NA
        }
      }

      # fuzzy matches
      max_dist = 1 - sug.dist
      no_authors_no_match1 <- no_author & is.na(result$id)
      if (any(no_authors_no_match1)) {

        input_names_clean1 <- search_names[no_authors_no_match1]
        fuzzy_match <- nameMatching(input_names_clean1,
                                    ref_names_clean1,
                                    match.type = "fuzzy",
                                    clean.names = FALSE,
                                    dist.method = dist.method,
                                    max.dist = max_dist,
                                    split.letters = split.letters,
                                    parallel = parallel,
                                    cores = cores,
                                    show.progress = TRUE)

        if (any(!is.na(fuzzy_match))) {
          result[no_authors_no_match1, c(bb_cols)] <-
            ref.df[fuzzy_match, bb_cols]

          result$match_type[no_authors_no_match1] <-
            "fuzzy_wout_author"
          result$multiple_match[no_authors_no_match1] <- FALSE

          result$match_type[no_authors_no_match1 & w_indet] <-
            "fuzzy_wout_author_wout_indet"

          # Calculating the distance between names
          name_dist1 <-
            stringdist::stringdist(
              input_names_clean1,
              result[["name"]][no_authors_no_match1])
          result$fuzzy_dist_name[no_authors_no_match1] <-
            round(name_dist1/
                    nchar(input_names_clean1), 4)

          # Fixing some bad indexing replacements
          double_check <- is.na(result$id)
          if (any(double_check)) {
            result$match_type[double_check] <-
              result$multiple_match[double_check] <-
              result$fuzzy_dist_name[double_check] <- NA
          }
        }
      }
    }

    # no matches
    rep_these <- is.na(result$id)
    if (any(rep_these)) {
      result$match_type[rep_these] <-
        "no_match"
      result[rep_these, bb_cols[3:4]] <-
        result[rep_these, tax.names]
    }

    rownames(result) <- NULL

    select_cols <- c(tax.names,
                     "match_type", "multiple_match",
                     "fuzzy_dist_name", "fuzzy_dist_author",
                     bb_cols)
    output <- result[, select_cols]
    names(output)[which(names(output) == "name")] <-
      "suggestedName"
    names(output)[which(names(output) == "authorship")] <-
      "suggestedAuthorship"

    # flagging fuzzy matches above the threshold
    auth_factor <- 0.5
    max_dist_auth <- max_dist + auth_factor * max_dist
    rep_these <- (output[["fuzzy_dist_name"]] > max_dist &
                    (is.na(output[["fuzzy_dist_author"]]) | output[["fuzzy_dist_author"]] > 0)) |
                  (output[["fuzzy_dist_author"]] > max_dist_auth &
                    output[["fuzzy_dist_name"]] > 0 )
    rep_these[is.na(rep_these)] <- FALSE
    if (any(rep_these))
      output[rep_these, "match_type"] <-
        paste0("bad_", result[rep_these, "match_type"])

    output <- getTaxNotes(output)

    if (replace.names) {

      old.cols <- c("id", "suggestedName", "suggestedAuthorship",
                    "taxon.rank", "name.status")
      new.cols <- c("accepted.id", "accepted.name", "accepted.authorship",
                    "accepted.taxon.rank", "accepted.name.status")

      rep_these <- grepl("synonym", output$notes, perl = TRUE)
      if (any(rep_these)) {
        w_accept_id <- !is.na(output[rep_these, new.cols[1]])
        if (any(w_accept_id)){
          output[rep_these & w_accept_id, old.cols] <-
            output[rep_these & w_accept_id, new.cols]
          output$notes[rep_these & w_accept_id] <-
            gsub("synonym", "replaced synonym",
                 output$notes[rep_these & w_accept_id],
                 perl = TRUE)
        }

        if (any(!w_accept_id)){
          output$notes[rep_these & !w_accept_id] <-
            gsub("synonym", "synonym not replaced",
                 output$notes[rep_these & !w_accept_id],
                 perl = TRUE)
        }

        # output[rep_these, old.cols] <- output[rep_these, new.cols]
        # output$notes[rep_these] <-
        #   gsub("synonym", "replaced synonym", output$notes[rep_these],
        #        perl = TRUE)
      }

      rep_these <- grepl("orthographic", output$notes, perl = TRUE)
      if (any(rep_these)) {
        has_accept <- !is.na(output$accepted.name)
        if (any(has_accept)) {
          output[rep_these & has_accept, old.cols] <-
            output[rep_these & has_accept, new.cols]
          output$notes[rep_these & has_accept] <-
            sub("orthographic variant", "replaced orth. variant",
                 output$notes[rep_these & has_accept],
                 perl = TRUE)
        }
      }

      rep_these <- output$notes %in% "check +1 name"
      if (any(rep_these)) {
        rep_these1 <-
          !grepl("|", output[["accepted.authorship"]][rep_these],
                 fixed = TRUE)
        if (any(rep_these1)) {
          output[rep_these, old.cols][rep_these1, ] <-
            output[rep_these, new.cols][rep_these1, ]
          output$notes[rep_these][rep_these1] <-
            "+1 name, but 1 accepted"
        }
      }

      rep_these <- output$notes %in% "bad match"
      if (any(rep_these)) {
        output[rep_these, old.cols[2:3]] <- output[rep_these, tax.names]
        output[rep_these, c("id", old.cols[4:5], "taxon.status") ] <- NA
        output[rep_these, new.cols] <- NA
        if ("family" %in% names(output))
          output$family[rep_these] <- NA
        output$notes[rep_these] <- "not found"
      }
    }

    if (length(drop.cols) > 0) {
      drop.cols1 <- which(names(output) %in% drop.cols)
      if (length(drop.cols1) > 0) {
        output1 <- output[, -drop.cols1]
      } else {
        output1 <- output
      }
    } else {
      output1 <- output
    }

    keep.cols <- unique(c(tax.names,
                   "family", "suggestedName", "suggestedAuthorship",
                   "taxon.rank", "notes", "id",
                   names(output1)))
    keep.cols <- keep.cols[keep.cols %in% names(output1)]
    output.final <- output1[, keep.cols]
    output.final1 <- dplyr::left_join(df, output.final,
                           by = tax.names)
    output.final1 <- output.final1[order(output.final1$tmp..ordem),]
    # output.final1$tax.source <- db
    results[[i]] <- output.final1
  }

  ## Replacing missing name suggestions in the priority defined by 'db'
  if (length(results) > 1) {
    ids.cols <-
      grep("amily", names(results[[1]])):dim(results[[1]])[2]
    for (i in 1:(length(results) - 1)) {
      ids.lines <-
        results[[1]]$notes %in%
          c("not found", "bad match", "check not resolved", "check +1 name") &
            !is.na(results[[1 + i]]$suggestedName)
      results[[1]][ids.lines , ids.cols] <-
        results[[1 + i]][ids.lines , ids.cols]
    }
  }

  #Final edits
  final.results <- results[[1]]
  final.results <- final.results[order(final.results$tmp..ordem), ]
  final.results$tmp..ordem <- NULL

  final.results[["scientificNameFull"]] <-
    buildName(final.results,
              col.names = c("suggestedName", "suggestedAuthorship"))

  # if (any(w_indet)) {
  #   final.results[["scientificNameFull"]][w_indet]
  #
  # }

  check_these <-
    grep("|", final.results[["scientificNameFull"]], fixed = TRUE)
  if (length(check_these) > 0L) {
    tmp <-
      final.results[check_these, c("suggestedName", "suggestedAuthorship")]
    taxa.split <- strsplit(tmp[[1]], "|", fixed = TRUE)
    auth.split <- strsplit(tmp[[2]], "|", fixed = TRUE)
    names.new <- mapply(paste, taxa.split, auth.split)

    if (inherits(names.new, "matrix")) {
      tmp.names.new <-
        apply(names.new, 2, function(x) paste(x, collapse = "|"))
    } else {
      tmp.names.new <-
        unlist(lapply(names.new, function(x) paste(x, collapse = "|")))
    }
    final.results[["scientificNameFull"]][check_these] <-
      tmp.names.new
  }

  names(final.results)[which(names(final.results) == "notes")] <-
    "tax.notes"
  extra.cols <-
    names(final.results)[!names(final.results) %in% names(x)]
  final.results1 <-
    cbind.data.frame(x, final.results[, extra.cols])

  return(final.results1)
}
