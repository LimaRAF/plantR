#' @title Get Valid Plant Names
#'
#' @description Provides valid plant names or corrects typos and orthographical
#'   variants based on the
#'   \href{http://floradobrasil.jbrj.gov.br/reflora/listaBrasil/ConsultaPublicaUC/ConsultaPublicaUC.do}{Brazilian
#'    Flora 2020} project and on the \href{http://www.theplantlist.org/}{The
#'   Plant List}. It uses the functionalities of other R packages, namely __flora__
#'   (Carvalho 2019) and __Taxonstand__ (Cayuela et al. 2017).
#'
#' @param x a data.frame containing the species scientific name without authors
#'   and their authorship, ideally already passed by the string check in
#'   `fixSpecies`. The authorship is only used for solving homonyms.
#' @param tax.names character. Names of the columns containing the species names
#'   and authors. Defaults to 'scientificName.new' and
#'   'scientificNameAuthorship'.
#' @param db the list of database to be consulted for valid names, in their
#'   preferred order of priority. Only the results from Brazilian Flora 2020
#'   ('bfo'), The Plant List ('tpl') or both are currently implemented.
#' @param sug.dist a fraction expressing the maximum distance allowed between
#'   the original species name and the suggested species name, which is passed
#'   to the arguments `suggestion.distance` of function `flora::get.taxa()` and
#'   `max.distance` of function `Taxonstand::TPL()`. Defaults to 0.9.
#' @param use.authors logical. Should all authors names be verified (takes
#'   longer)? Default to TRUE.
#' @param drop.cols character. Name of columns that should be dropped from the
#'   results.
#'
#' @return
#' A data frame with the same columns provided by the user and some additional
#' columns containing the suggested binomial ('suggestedName'), the corresponding
#' scientific name ('scientific.name'), the notes and the sources used to retrieve
#' the suggested binomial (columns 'notes' and 'source').
#'
#' @details The function currently uses packages __flora__ and __Taxonstand__ to
#'   provide suggested names. Not all information returned by those packages
#'   are returned here, but some of them can be controlled using the argument
#'   `drop.cols`.
#'
#'   The function does not provide the suggested names from the different
#'   databases/sources. It combines into the same column ('suggestedName') the
#'   suggestions from one or more databases. If more than one databases is used,
#'   the suggested names are combined following the same order of the database
#'   codes provided in the argument `db`. See examples for details.
#'
#'
#' @author Renato A. Ferreira de Lima
#'
#' @references
#'
#' Gustavo Carvalho (2019). flora: Tools for Interacting with the Brazilian
#' Flora 2020. R package version 0.3.1. http://www.github.com/gustavobio/flora
#'
#' Luis Cayuela, Anke Stein and Jari Oksanen (2017). Taxonstand: Taxonomic
#' Standardization of Plant Species Names. R package version 2.1.
#' https://CRAN.R-project.org/package=Taxonstand
#'
#'
#' @examples
#' sp.list <- c("Casearia sylvestris","Casearia sylvestirs",
#'   "Casearia sylvestris angustifolia", "Casearia attenuata",
#'   "Casearia celtidifolia", "Casearia celtidifolia","Casearia tropicana")
#' aut_list <- c("Sw.", "Sw.", "Uittien", "Rusby", "Kunth", "de Vriese","L.")
#'
#' df <- data.frame(scientificName.new = sp.list,
#'   scientificNameAuthorship = aut_list)
#'
#' prepSpecies(df)
#' prepSpecies(df, db = c("tpl","bfo"))
#' prepSpecies(df, db = "tpl", sug.dist = 0.85)
#'
#' @seealso
#'  Functions \link[flora]{get.taxa} and \link[Taxonstand]{TPL}.
#'
#' @importFrom flora trim get.taxa fixCase remove.authors
#' @importFrom stringr str_count str_trim
#' @importFrom Taxonstand TPL
#'
#' @export prepSpecies
#'
prepSpecies <- function(x,
                        tax.names = c("scientificName.new","scientificNameAuthorship"),
                        db = c("bfo", "tpl"),
                        sug.dist = 0.9,
                        use.authors = TRUE,
                        drop.cols = c("tmp.ordem","family","verbatimSpecies","author","full_sp","authorship","id")) {

  ## check input
  if (!class(x) == "data.frame")
    stop("input object needs to be a data frame!")

  if (!tax.names[1] %in% names(x))
    stop("Input data frame must have a column with the species name")

  if (is.na(tax.names[2])) {
    warning("Column name with species authorities not provided; setting to 'scientificNameAuthorship'")
    tax.names[2] <- 'scientificNameAuthorship'
  }

  # storing the databases in the order of priority defined in 'db'
  results <- vector("list", length(db))
  names(results) <- db

  # creating the full-length data frames
  if (tax.names[2] %in% names(x)) {
    df <- data.frame(
      tmp.ordem = 1:length(x[, tax.names[1]]),
      verbatimSpecies = x[, tax.names[1]],
      author = x[, tax.names[2]],
      stringsAsFactors = FALSE
    )
    df$full_sp <- paste(df$verbatimSpecies, df$author)
    df$full_sp <- stringr::str_trim(gsub(" NA$", "", df$full_sp))

  } else {
    if (use.authors) {
      use.authors <- FALSE
      warning("Argument 'use.authors' set to TRUE, but column with author names not found", call. = FALSE)
    }
    df <- data.frame(
      tmp.ordem = 1:length(x[, tax.names[1]]),
      verbatimSpecies = x[, tax.names[1]],
      stringsAsFactors = FALSE
    )
  }

  # suggesting a valid name using flora pkg
  if (any(db %in% c("bfo", "fbo"))) {
    # cleaning spaces and removing duplicated names
    trim_sp <- flora::trim(unique(x[,tax.names[1]]))
    # obtaining valid names from FBO-2020
    suggest_sp <- flora::get.taxa(trim_sp, drop = "", suggestion.distance = sug.dist)
    miss_sp <- suggest_sp$original.search[suggest_sp$notes %in% "not found"]

    if (length(miss_sp) > 0) {
      #Are names missing due to lack of the infra-specific rank abbreviation?
      add.rank <- function(x, rank = NULL){
        x_sp <- strsplit(x, " ")
        x1 <- as.character(sapply(x_sp, function(y) paste(y[1], y[2], rank, y[3], collapse = " ")))
        return(x1)
      }
      spcs <- stringr::str_count(miss_sp, " ")
      miss_rank <- rbind(
        cbind(miss_sp[spcs == 2], add.rank(miss_sp[spcs == 2], "var.")),
        cbind(miss_sp[spcs == 2], add.rank(miss_sp[spcs == 2], "subsp.")))

      #Are names missing due to authorships with the binomial?
      no_authors <- sapply(miss_sp[spcs >= 2],
                           function(x) flora::remove.authors(flora::fixCase(x)))

      #Gettting possible missing names
      # miss_spp <- cbind.data.frame(miss_sp, miss_sp)
      if (dim(miss_rank)[1] > 0 | length(no_authors) > 0) {

        if (dim(miss_rank)[1] > 0 & length(no_authors) > 0) {
          miss_spp <- rbind(miss_rank,
                            cbind(miss_sp[spcs >= 2], no_authors))
          miss_spp <- miss_spp[!duplicated(miss_spp[, 2]), , drop = FALSE]
        } else {
          if (dim(miss_rank)[1] > 0)
            miss_spp <- miss_rank
          if (length(no_authors) > 0)
            miss_spp <- cbind(miss_sp[spcs >= 2], no_authors)
        }

        suggest_miss_sp <-
          flora::get.taxa(miss_spp[, 2], drop = "", suggestion.distance = sug.dist)
        suggest_miss_sp <-
          suggest_miss_sp[!suggest_miss_sp$notes %in% "not found", ]

        if (dim(suggest_miss_sp)[1] > 0) {
          suggest_miss_sp$original.search <-
            miss_spp[miss_spp[, 2] %in% suggest_miss_sp$original.search, 1]

          #Merging names found at first and second tries
          found_ids <- suggest_sp$notes %in% "not found" &
            suggest_sp$original.search %in% suggest_miss_sp$original.search
          suggest_sp[found_ids, ] <- suggest_miss_sp
        }
      }
    }

    #Getting the valid name at the right rank
    suggest_sp$suggestedName <-
      paste(suggest_sp$genus, suggest_sp$specific.epiteth, sep = " ")
    suggest_sp$suggestedName[suggest_sp$taxon.rank %in% "subspecies"] <-
      paste(suggest_sp$suggestedName[suggest_sp$taxon.rank %in% "subspecies"],
            suggest_sp$infra.epiteth[suggest_sp$taxon.rank %in% "subspecies"], sep = " subsp. ")
    suggest_sp$suggestedName[suggest_sp$taxon.rank %in% "variety"] <-
      paste(suggest_sp$suggestedName[suggest_sp$taxon.rank %in% "variety"],
            suggest_sp$infra.epiteth[suggest_sp$taxon.rank %in% "variety"], sep = " var. ")
    suggest_sp$suggestedName[suggest_sp$taxon.rank %in% c("genus", "family", "subfamily")] <-
      suggest_sp$search.str[suggest_sp$taxon.rank %in% c("genus", "family", "subfamily")]
    suggest_sp$suggestedName[suggest_sp$taxon.rank %in% NA] <-
      suggest_sp$original.search[suggest_sp$taxon.rank %in% NA]
    suggest_sp$suggestedName[suggest_sp$suggestedName %in% c("NA NA")] <- NA

    #Merge checks with the original data
    suggest_flora <- merge(df, suggest_sp[,c("original.search","family","suggestedName","authorship","scientific.name","notes","id")],
                           by.x = "verbatimSpecies", "original.search", all.x = TRUE, sort = FALSE)
    suggest_flora <- suggest_flora[order(suggest_flora$tmp.ordem),]
    suggest_flora$source <- "BR-Flora"

    results[[which(db %in% c("bfo", "fbo"))]] <- suggest_flora
  }

  if (any(db %in% c("tpl"))) {

    assign("last.warning", NULL, envir = baseenv())
    if (use.authors) {
      # removing duplicated names
      unique_sp <- unique(df$full_sp)
      # Species names, exact match
      #my.TPL <- catchAll(Taxonstand::TPL)
      my.TPL <- catchAll(TPL1)
      temp.obj <- my.TPL(unique_sp, corr = FALSE, author = TRUE)
      exact_sp <- temp.obj[[1]]
      warns <- temp.obj[[2]]
      # exact_sp <- Taxonstand::TPL(unique_sp, corr = FALSE)
      # warns <- warnings()

    } else {
      # removing duplicated names
      unique_sp <- unique(df$verbatimSpecies)
      # Species names, exact match
      #my.TPL <- catchAll(Taxonstand::TPL)
      my.TPL <- catchAll(TPL1)
      temp.obj <- my.TPL(unique_sp, corr = FALSE)
      exact_sp <- temp.obj[[1]]
      warns <- temp.obj[[2]]
      # exact_sp <- Taxonstand::TPL(unique_sp, corr = FALSE)
      # warns <- warnings()
    }

    if (!is.null(warns)) {
      warns <- warns[grepl("more than one valid", warns, fixed = TRUE)]
      # warns <-
      #   names(unlist(warns))[grepl("more than one valid", names(unlist(warns)))]
      warns <- sapply(warns, function(x)
        paste(strsplit(x, " ")[[1]][1:2], collapse = " "))
    } else {
      warns <- NULL
    }

    # Species names, fuzzy match
    miss_sp <- exact_sp$Taxon[(exact_sp$Higher.level & exact_sp$Taxonomic.status %in% "Accepted") |
                                exact_sp$Taxonomic.status %in% ""]
    # my.TPL <- catchAll(Taxonstand::TPL)
    my.TPL <- catchAll(TPL1)
    temp.obj <- my.TPL(miss_sp, corr = TRUE, max.distance = 1 - sug.dist)
    fuzzy_sp <- temp.obj[[1]]
    # fuzzy_sp <- Taxonstand::TPL(miss_sp, corr = TRUE, max.distance = 1 - sug.dist)
    exact_sp[(exact_sp$Higher.level & exact_sp$Taxonomic.status %in% "Accepted") |
               exact_sp$Taxonomic.status %in% "",] <- fuzzy_sp

    # Species names + author, exact match
    if (!use.authors) {
      # removing duplicated names
      unique_sp1 <- unique(df$full_sp[df$verbatimSpecies %in% warns])
      my.TPL <- catchAll(Taxonstand::TPL)
      temp.obj <- my.TPL(unique_sp1, corr = FALSE)
      exact_sp1 <- temp.obj[[1]]
      warns1 <- temp.obj[[1]]
      warns1 <- warns1[grepl("The input author", warns1)]
      # exact_sp1 <- Taxonstand::TPL(unique_sp1, corr = FALSE)
      # warns1 <- warnings()
      # warns1 <- names(unlist(warns1))[grepl("The input author", names(unlist(warns1)))]
      warns1 <- sapply(warns1, function(x) paste(strsplit(x," ")[[1]][1:2], collapse = " "))
    } else {
      warns1 <- NULL
    }

    #Edits before merging
    exact_sp$notes <- ""
    exact_sp$notes[exact_sp$Taxonomic.status %in% "Synonym" &
                     exact_sp$Plant.Name.Index] <- "replaced synonym"
    exact_sp$notes[exact_sp$Taxonomic.status %in% "Synonym" &
                     exact_sp$Higher.level &
                     !exact_sp$Plant.Name.Index] <- "replaced synonym, but at a higher rank"
    exact_sp$notes[exact_sp$Taxonomic.status %in% "Accepted" &
                     exact_sp$Higher.level &
                     !exact_sp$Plant.Name.Index] <- "accepted, but at a higher rank"
    exact_sp$notes[exact_sp$Taxonomic.status %in% "Accepted" &
                     !exact_sp$Higher.level &
                     exact_sp$Typo] <- "was misspelled"
    exact_sp$notes[exact_sp$Taxonomic.status %in% "Synonym" &
                     !exact_sp$Higher.level &
                     exact_sp$Typo] <- "was misspelled|replaced synonym"
    if (!is.null(warns)) {
      exact_sp$notes[exact_sp$Taxon %in% warns] <-
        paste(exact_sp$notes[exact_sp$Taxon %in% warns], "check +1 accepted", sep = "|")
      exact_sp$notes <- gsub("\\|check +1 accepted", "check +1 accepted", exact_sp$notes)
    }

    if (!is.null(warns1)) {
      exact_sp$notes[exact_sp$Taxon %in% warns1] <-
        paste(exact_sp$notes[exact_sp$Taxon %in% warns1], "wrong author: check possible homonym", sep = "|")
      exact_sp$notes <- gsub("\\|wrong author: check possible homonym", "wrong author: check possible homonym", exact_sp$notes)
    }

    exact_sp$notes[exact_sp$Taxonomic.status %in% "" |
                     (!exact_sp$Plant.Name.Index &
                        !exact_sp$Higher.level & !exact_sp$Typo)] <- "not found"

    #Getting the valid name at the right rank
    exact_sp$suggestedName <- paste(exact_sp$New.Genus, exact_sp$New.Species, sep=" ")
    exact_sp$suggestedName[exact_sp$New.Infraspecific.rank %in% "subsp."] <-
      paste(exact_sp$suggestedName[exact_sp$New.Infraspecific.rank %in% "subsp."],
            exact_sp$New.Infraspecific[exact_sp$New.Infraspecific.rank %in% "subsp."], sep=" subsp. ")
    exact_sp$suggestedName[exact_sp$New.Infraspecific.rank %in% "var."] <-
      paste(exact_sp$suggestedName[exact_sp$New.Infraspecific.rank %in% "var."],
            exact_sp$New.Infraspecific[exact_sp$New.Infraspecific.rank %in% "var."], sep=" var. ")
    exact_sp$suggestedName[is.na(exact_sp$New.Species)] <-
      exact_sp$New.Genus[is.na(exact_sp$New.Species)]

    #Getting the scientific name with authorities
    exact_sp$scientific.name <- NA_character_
    auth.ids <- exact_sp$New.Authority %in% ""
    exact_sp$scientific.name[auth.ids] <-
      exact_sp$suggestedName[auth.ids]
    exact_sp$scientific.name[!auth.ids] <-
      paste(exact_sp$suggestedName[!auth.ids], exact_sp$New.Authority[!auth.ids])

    #Merge checks with the original data
    if (use.authors) {
      suggest_TPL <- merge(df, exact_sp[,c("Taxon","Family","suggestedName","New.Authority","scientific.name","notes","New.ID")],
                           by.x = "full_sp", by.y = "Taxon", all.x = TRUE, sort = FALSE)
    } else {
      suggest_TPL <- merge(df, exact_sp[,c("Taxon","Family","suggestedName","New.Authority","scientific.name","notes","New.ID")],
                           by.x = "verbatimSpecies", by.y = "Taxon", all.x = TRUE, sort = FALSE)
    }
    suggest_TPL <- suggest_TPL[order(suggest_TPL$tmp.ordem),]
    suggest_TPL$source <- "TPL"
    names(suggest_TPL)[grepl("Family|Authority|ID", names(suggest_TPL))] <-
      c("family", "authorship", "id")
    results[[which(db %in% "tpl")]] <- suggest_TPL

  }

  ## Replacing missing suggestions from the order of priority defined by 'db'
  if (length(results) > 1) {
    ids.cols <- grep("amily", names(results[[1]])):dim(results[[1]])[2]
    for (i in 1:(length(results) - 1)) {
      ids.lines <- results[[1]]$notes %in% c("not found", "check +1 accepted") &
        !is.na(results[[1 + i]]$suggestedName)
      results[[1]][ids.lines , ids.cols] <-
        results[[1 + i]][ids.lines , ids.cols]
    }
  }

  #Final edits
  final.results <- results[[1]]
  final.results <- final.results[order(final.results$tmp.ordem), ]
  drop.cols1 <- unique(c(drop.cols, "verbatimSpecies", "author"))
  final.results <- final.results[, -which(names(final.results) %in% drop.cols1)]
  final.results1 <- cbind.data.frame(x,
                                     final.results,
                                     stringsAsFactors = FALSE)
  names(final.results1)[which(names(final.results1) == "scientific.name")] <-
    "scientificNameFull"
  names(final.results1)[which(names(final.results1) == "notes")] <-
    "tax.notes"
  names(final.results1)[which(names(final.results1) == "source")] <-
    "tax.source"

  return(final.results1)
}
