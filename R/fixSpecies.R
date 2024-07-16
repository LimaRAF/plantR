#' @title Edit Scientific Name Notation
#'
#' @description Identifies open nomenclature (aff., cf.) in scientific
#'   names, classification under species level (var., subsp., f.) and
#'   standardize the notation of incomplete name identifications. It
#'   creates a new column with the new suggested name and it also
#'   flags names with authors and problematic names (character string
#'   with numbers, wrong case, or other names besides genus and
#'   epithet etc). Names can be returned with or without
#'   infra-specific ranks (var. and subsp.) or abbreviations of
#'   unspecific names (sp. or spp.).
#'
#' @return
#' A data frame the input vector and the new columns `verbatimSpecies`
#' with small edits before flagging, `scientificNameStatus` with the
#' flags in original data and `scientificName.new` with a suggestion
#' for a more correct name. See Details for a description of flags in
#' the column `scientificNameStatus`.
#'
#' @details Possible flags returned in `scientificNameStatus`: \describe{
#' \item{\code{possibly_ok}}{scientific name following the classic pattern
#' 'Genus epithet'}
#' \item{\code{not_Genus_epithet_format}}{scientific name not following
#' the expected pattern Genus epithet}
#' \item{\code{variety}}{scientific name with variety}
#' \item{\code{subspecies}}{scientific name with subspecies}
#' \item{\code{forma}}{scientific name with form}
#' \item{\code{infra_specific}}{scientific name with genus, specific epithet and
#' infra-specific, but no infra-specific rank}
#' \item{\code{hybrid}}{scientific name of a hybrid species}
#' \item{\code{conferre}}{open nomenclature cf. in the scientific name}
#' \item{\code{affinis}}{open nomenclature aff. in the scientific name}
#' \item{\code{indet}}{taxon identified only at genus level}
#' \item{\code{subfamily_as_genus}}{subfamily as genus, not a valid name}
#' \item{\code{family_as_genus}}{family as genus, not a valid name}
#' \item{\code{order_as_genus}}{order as genus, not a valid name}
#' \item{\code{incertae_sedis}}{scientific name of uncertain placement}
#' \item{\code{species_nova}}{species name contains an indication of a new
#' species, possibly not yet a valid name}
#' \item{\code{name_w_authors}}{scientific name has authors}
#' \item{\code{name_w_wrong_case}}{scientific name has upper/lowercase issues}
#' \item{\code{name_w_non_ascii}}{species name has non ASCII characters, not a
#' valid name}
#' \item{\code{abbreviated_genus}}{genus is abbreviated}
#' \item{\code{name_w_digits}}{scientific name has digits, not a valid
#' name} }
#'
#' @param x a vector or data.frame containing the taxon name
#'   information
#' @param tax.name character. The name of the column containing the
#'   taxon name. Defaults to "scientificName"
#' @param author.name character. The name of the column containing the
#'   author of scientific name. Defaults to "scientificNameAuthorship"
#' @param rm.rank logical. Should the infra-specific rank abbreviation
#'   be removed from the name? Defaults to FALSE
#' @param rm.indet logical. Should the abbreviations for unspecific
#'   names (i.e. sp. or spp.) be removed? Defaults to FALSE
#'
#' @author Sara Mortara & Renato A. Ferreira de Lima
#'
#' @references
#' Sigovini, M., Keppel, E. and Tagliapietra, D. (2016) Open
#' Nomenclature in the biodiversity era. Methods in Ecology and
#' Evolution 7(10): 1217-1225.
#'
#' @examples
#' df <- data.frame(scientificName =
#' c("Lindsaea lancea", "Lindsaea lancea (L.) Bedd.",
#' "Lindsaea lancea var. Angulata",
#' "Lindsaea Aff. lancea",
#' "Lindsaea", "Lindsaea sp.", "Lindsaeaceae sp.",
#' "Lindsaea aff. lancea (L.) Bedd.",
#' "Lindsaea ×improvisa K.U.Kramer",
#' "Parablechnum C.Presl",
#' "Blechnum spannagelii Rosenst.",
#' "Blechnum occidentale leopoldense Dutra",
#' "Blechnum austrobrasilianum de la Sota"))
#'
#' fixSpecies(df)
#' fixSpecies(df, rm.rank = TRUE)
#' fixSpecies(df, rm.rank = TRUE, rm.indet = TRUE)
#'
#' @importFrom stringr str_detect regex fixed str_count str_split
#' @importFrom stringi stri_enc_mark
#'
#' @seealso
#'  Functions \link[plantR]{fixAnnotation}, \link[plantR]{fixIndet},
#'  \link[plantR]{fixCase} and \link[plantR]{fixAuthors}
#'
#' @export fixSpecies
#'
#'
fixSpecies <- function(x = NULL,
                       tax.name = "scientificName",
                       author.name = "scientificNameAuthorship",
                       rm.rank = FALSE,
                       rm.indet = FALSE) {

  if (inherits(x, "character")) {
    x <- data.frame(x, check.names = FALSE, fix.empty.names = FALSE)
    colnames(x) <- tax.name
  }

  if (!inherits(x, "data.frame"))
    stop("Input object needs to be a vector or data frame!",
         call. = FALSE)

  if (dim(x)[1] == 0)
    stop("Input data frame is empty!", call. = FALSE)

  if (!tax.name %in% names(x))
    stop("Input data frame must have a column named: ", tax.name,
         call. = FALSE)

  if (!author.name %in% names(x)) {
    x[[author.name]] <- NA
  } else {
    rep_these <- x[[author.name]] %in% c("", " ", "NA", "NULL")
    if (any(rep_these))
      x[[author.name]][rep_these] <- NA
  }

  x1 <- unique(x[, c(tax.name, author.name)])
  species <- as.character(x1[[tax.name]])
  authors <- as.character(x1[[author.name]])

  # preliminary edits
  species <- fixAnnotation(species)
  species <- fixIndet(species)

  check <- data.frame(species = as.character(species),
                      authors = as.character(authors))
  check$species_new <- check$species
  check$authors_new <- check$authors

  # defining regex strings
  aff_string <- "^aff\\.|^aff | aff\\. "
  cf_string <- "^cf\\.|^cf | cf\\. "
  subsp_string <-  " subsp\\.$| subsp\\. "
  var_string <- " var\\.$| var\\. "
  form_string <- " form\\. | fo\\. | f\\. [a-z]| forma "
  author.f_string <- " f\\. ex "
  form.sp_string <- " f. sp. "
  hyb_string <- "\u00d7 | x | \u00d7 "
  inc_string <- "inc\\. sed\\.|Incertae sedis"
  spnov_string <- " sp\\. nov\\.| spec\\. nov\\.| sp\\. n\\.| nov\\. sp\\.| nov\\. spec\\.| n\\. sp\\."
  indet_string <- " sp\\.$| sp$| sp\\.| indet\\.| ind\\.| sp | spp\\.$"

  # Add other possibilities of infraspecific codes
  # "cultivar.", "subvar."
  # Should we deal with other higher-taxa codes? E.g. 'sect.', 'subf.', 'subg.'

  # detecting name modificators, infraspecies, open nomenclature, abbreviations and other cases
  aff <- stringr::str_detect(species, stringr::regex(aff_string,
                                                     ignore_case = TRUE))
  cf <- stringr::str_detect(species, stringr::regex(cf_string,
                                                    ignore_case = TRUE))
  subsp <- stringr::str_detect(species, stringr::regex(subsp_string,
                                                       ignore_case = TRUE))
  var <- stringr::str_detect(species, stringr::regex(var_string,
                                                     ignore_case = TRUE))
  form <- stringr::str_detect(species, stringr::regex(form_string,
                                                      ignore_case = FALSE))
  author.f <- stringr::str_detect(species, stringr::regex(author.f_string))
  form[author.f] <- FALSE

  form.sp <- stringr::str_detect(species, stringr::fixed(form.sp_string))
  form[form.sp] <- FALSE


  hyb <- stringr::str_detect(species, stringr::regex(hyb_string,
                                                     ignore_case = TRUE))
  inc <- stringr::str_detect(species, stringr::regex(inc_string,
                                                     ignore_case = TRUE))
  infra <- stringr::str_count(species,
                              stringr::regex(" [a-z][a-z][a-z] ")) >= 2
  infra[aff | subsp | var | form | inc] <- FALSE
  false_infra <- stringr::str_detect(species,
                              stringr::regex(" [a-z][a-z][a-z] "))
  infra[false_infra] <- FALSE

  fixed_cases <- as.vector(fixCase(species))
  case <- species != fixed_cases

  spnov <- stringr::str_detect(species, stringr::regex(spnov_string,
                                                       ignore_case = TRUE))
  indet <- stringr::str_detect(species, stringr::regex(indet_string,
                                                       ignore_case = TRUE))
  indet[spnov] <- FALSE
  question <- stringr::str_detect(species, "\\?")
  no_sp <- stringr::str_count(species, " ") < 1
  indet[no_sp | question] <- TRUE

  digits <- stringr::str_detect(species, '\\d')
  digits[indet] <- FALSE

  # defining names status
  status <- data.frame(aff, cf, subsp, var, form, infra, hyb, inc,
                       form.sp, case, spnov, indet, digits)
  colnames(status) <- c("affinis", "conferre", "subspecies",
                        "variety", "forma", "infra_specific",
                        "hybrid", "incertae_sedis", "forma_specialis",
                        "name_w_wrong_case", "species_nova", "indet",
                        "name_w_digits")
  status[!status] <- NA
  for (i in seq_along(colnames(status)))
    status[[i]][status[[i]] %in% TRUE] <- colnames(status)[i]

  check$species_status <- apply(status, 1, paste1, collapse = "|")

  # fixing cases
  check$species_new[case] <- fixed_cases[case]

  # recognizing and isolating authorship
  auth_string <- grepl(" [A-Z]| \\(| [a-z][a-z] | [a-z][a-z][a-z] ",
                       check$species_new, perl = TRUE) &
                  !grepl(" [A-Z+]$", check$species_new, perl = TRUE)

  if (any(auth_string)) {
    author_split <- fixAuthors(check$species_new[auth_string])
    tax_name <- author_split$tax.name
    tax_author <- author_split$tax.author
    id_authors <- check$species_new[auth_string] != tax_name

    check$species_status[auth_string][id_authors] <-
      paste(check$species_status[auth_string][id_authors],
            "name_w_authors", sep = "|")
    check$species_status <-
      gsub("^\\|", "", check$species_status, perl = TRUE)
    check$species_new[auth_string][id_authors] <-
      squish(tax_name[id_authors])
    check$authors_new[auth_string][id_authors] <-
      squish(tax_author[id_authors])
  }


  # removing open taxonomy, ranks and hybrid notation
  check$species_new[aff | cf] <- rmOpen(check$species_new[aff | cf])
  check$species_new[hyb] <- rmHyb(check$species_new[hyb])
  check$species_new[subsp | var | form] <-
    rmInfra(check$species_new[subsp | var | form])

  # names not matching genus + epithet pattern
  id_not_gensp <- sapply(stringr::str_split(check$species_new, " "),
                         length) > 2 & check$species_status %in% ""
  if (any(id_not_gensp))
    check$species_status[id_not_gensp] <- "not_Genus_epithet_format"

  # aceae in first string
  gen <- gsub(" .*", "", check$species_new, perl = TRUE)
  id_gen <- endsWith(gen, "aceae")
  check$species_status[id_gen] <- "family_as_genus"

  # order as genus
  id_ord <- endsWith(gen, "ales")
  check$species_status[id_ord] <- "order_as_genus"

  # subfamily as genus
  id_sub <- endsWith(gen, "deae")
  check$species_status[id_sub] <- "subfamily_as_genus"

  # abreviated genus
  abbrev_gen <- gsub("\\.", "", gen, perl = TRUE)
  abbrev_gen <- nchar(abbrev_gen) == 1
  check$species_status[abbrev_gen] <- "abbreviated_genus"

  # possibly ok (none of the categories above)
  check$species_status[check$species_status %in% c("", NA)] <-
    "possibly_ok"

  # non-ascii characters
  string_type <- stringi::stri_enc_mark(check$species_new)
  ascii <- string_type != "ASCII"
  if (any(ascii)) {
    check$species_status[ascii] <- paste(check$species_status[ascii],
                                         "name_w_non_ascii", sep = "|")
    check$species_status <- gsub("^\\|", "", check$species_status,
                                 perl = TRUE)
  }

  # option to return names with or without infra-specific ranks
  if (!rm.rank) {
    rep_these <- status$variety %in% "variety"
    if (any(rep_these))
      check$species_new[rep_these] <-
        addRank(check$species_new[rep_these], "var.")

    rep_these <- status$subspecies %in% "subspecies"
    if (any(rep_these))
        check$species_new[rep_these] <-
          addRank(check$species_new[rep_these], "subsp.")

    rep_these <- status$forma %in% "forma"
    if (any(rep_these))
      check$species_new[rep_these] <-
        addRank(check$species_new[rep_these], "f.")

    rep_these <- status$hybrid %in% "hybrid"
    if (any(rep_these))
      check$species_new[rep_these] <-
        addRank(check$species_new[rep_these], "\u00d7")

    check$species_new <-
      gsub(" NA$", "", check$species_new, perl = TRUE)
  }

  # option to return names with or without unidentified abbreviations
  if (rm.indet) {
    indet.ids <- check$species_status %in%
      c("indet", "family_as_genus", "order_as_genus", "subfamily_as_genus")
    check$species_new[indet.ids] <-
      gsub(" sp\\..*", "", check$species_new, perl = TRUE)[indet.ids]

  } else {
    indet.ids <- check$species_status %in%
      c("indet", "family_as_genus", "order_as_genus", "subfamily_as_genus")
    sp.ids <- grepl(" sp\\.|spp\\.", check$species_new, perl = TRUE)
    if (any(indet.ids & !sp.ids)) {
      check$species_new[indet.ids & !sp.ids] <-
        paste0(check$species_new[indet.ids & !sp.ids], " sp.")
      check$species_new <-
        gsub(" NA sp\\.$", " sp.", check$species_new, perl = TRUE)
    }
    check$species_new <-
      gsub("^na sp\\.$", NA, check$species_new,
           perl = TRUE, ignore.case = TRUE)
  }

  # preparing the output
  names(check)[names(check) == "species"] <-
    'verbatimSpecies'
  names(check)[names(check) == "authors"] <-
    author.name
  names(check)[names(check) == "species_new"] <-
    'scientificName.new'
  names(check)[names(check) == "authors_new"] <-
    'scientificNameAuthorship.new'
  names(check)[names(check) == "species_status"] <-
    'scientificNameStatus'
  check[[tax.name]] <- as.character(x1[[tax.name]])

  check$scientificName.new <-
    squish(check$scientificName.new)
  check$scientificNameAuthorship.new <-
    squish(check$scientificNameAuthorship.new)

  check1 <- suppressMessages(dplyr::left_join(x,
                             check[,c('scientificName.new',
                                      'scientificNameAuthorship.new',
                                      'scientificNameStatus',
                                      tax.name, author.name)],
                             by = c(tax.name, author.name)))

  # Should we use the original authos instead? Make it an argument?
  # rep_these <- !check1[[author.name]] %in% c("", " ", NA, "NA")
  # if (any(rep_these))
  #   check1$scientificNameAuthorship.new[rep_these] <-
  #     check1[[author.name]][rep_these]

  return(check1)
}
