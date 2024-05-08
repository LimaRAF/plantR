#' @title Edit Scientific Name Notation
#'
#' @description Identifies open nomenclature (aff., cf.) in scientific
#'   names, classification under species level (var. and subsp.) and
#'   stadardize the notation of incomplete taxon name identifications.
#'   It creates a new column with the new suggested name and it also
#'   flags problematic names (character string with numbers, authors,
#'   wrong case, or other names besides genus and epithet etc). Names
#'   can be returned with or without infra-specific ranks (var. and
#'   subsp.) or abbreviations of unspecific names (sp. or spp.). In
#'   the case of names with authors, authorship is currently removed
#'   from scientific names.
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
#' \item{\code{form}}{scientific name with form}
#' \item{\code{infra_specific}}{scientific name with genus, specific epiteth and
#' infra-specific, but no infra-specific rank}
#' \item{\code{hybrid_species}}{scientific name of a hybrid species}
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
#' \item{\code{not_name_has_digits}}{scientific name has digits, not a valid
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
#' @importFrom stringr str_detect str_replace str_count fixed str_split
#' @importFrom flora remove.authors
#' @importFrom stringi stri_enc_mark
#'
#' @export fixSpecies
#'
#'
fixSpecies <- function(x = NULL,
                       tax.name = "scientificName",
                       author.name = "scientificNameAuthorship",
                       rm.rank = FALSE,
                       rm.indet = FALSE) {

  if (class(x)[1] == "character") {
    x <- data.frame(x, check.names = FALSE, fix.empty.names = FALSE,
                    stringsAsFactors = FALSE)
    colnames(x) <- tax.name
  }

  if (!class(x)[1] == "data.frame")
    stop("Input object needs to be a vector or data frame!")

  if (dim(x)[1] == 0)
    stop("Input data frame is empty!")

  if (!tax.name %in% names(x)) {
    stop("Input data frame must have a column named: ", tax.name)
  } else {
    species <- as.character(unique(x[, tax.name]))
  }

  if (!author.name %in% names(x)) {
    warning("Input data frame do not have a column named: ", author.name)
    author.name <- NULL
  } else {
    authors <- as.character(unique(x[, author.name]))
  }


  #0. preliminary edits
  species <- fixAnnotation(species)
  species <- fixIndet(species)


  #implement status parasite "f. sp." not f. from forma
  #1. Open nomenclature and infraspecies class ####
  form_string <- "[[:space:]]f\\.$|[[:space:]]form\\.[[:space:]]|[[:space:]]f\\.[[:space:]]"
  inc_string <- "inc\\.[[:space:]]sed\\.|Incertae[[:space:]]sedis"
  aff_string <- "^aff\\.|^aff[[:space:]]|[[:space:]]aff\\.[[:space:]]"
  cf_string <- "^cf\\.|^cf[[:space:]]|[[:space:]]cf\\.[[:space:]]"
  subsp_string <-  "[[:space:]]subsp\\.$|[[:space:]]subsp\\.[[:space:]]"
  var_string <- "[[:space:]]var\\.$|[[:space:]]var\\.[[:space:]]"
  hyb_string <- "\u00d7[[:space:]]|[[:space:]]x[[:space:]]|[[:space:]]\u00d7[[:space:]]"
  aff_cf <- paste(aff_string, cf_string, sep = "|")
  subsp_var <- paste(subsp_string, var_string, form_string, sep = "|")

  # detecting status
  aff <- stringr::str_detect(species, stringr::regex(aff_string, ignore_case = TRUE))
  cf <- stringr::str_detect(species, stringr::regex(cf_string, ignore_case = TRUE))
  subsp <- stringr::str_detect(species, stringr::regex(subsp_string, ignore_case = TRUE))
  var <- stringr::str_detect(species, stringr::regex(var_string, ignore_case = TRUE))
  inc <- stringr::str_detect(species, stringr::regex(inc_string, ignore_case = TRUE))
  form <- stringr::str_detect(species, stringr::regex(form_string, ignore_case = FALSE))
  hyb <- stringr::str_detect(species, stringr::regex(hyb_string, ignore_case = TRUE))

  check <- data.frame(species = as.character(species))

  # defining status
  check$species_status <- NA
  check$species_status[aff] <- "affinis"
  check$species_status[cf] <- "conferre"
  check$species_status[subsp] <- "subspecies"
  check$species_status[var] <- "variety"
  check$species_status[inc] <- "incertae_sedis"
  check$species_status[form] <- "forma"
  check$species_status[hyb & !is.na(check$species_status)] <-
    paste(check$species_status[hyb & !is.na(check$species_status)], "hybrid_species", sep="|")
  check$species_status[hyb & is.na(check$species_status)] <- "hybrid_species"
  undecl.infraspp <- is.na(check$species_status) &
    stringr::str_count(check$species, stringr::regex(" [a-z][a-z][a-z]")) >= 2
  check$species_status[undecl.infraspp] <- "infra_specific"

  #1.1 Fixing cases (not using flora::fixCase anymore)
  check$species_new <- check$species

  case <- as.vector(fixCase(check$species_new))
  id_case <- check$species_new != case
  check$species_status[id_case & !is.na(check$species_status)] <-
    paste(check$species_status[id_case & !is.na(check$species_status)],
          "name_w_wrong_case", sep = "|")
  check$species_status[id_case & is.na(check$species_status)] <-
    "name_w_wrong_case"
  check$species_new[id_case] <- case[id_case]

  ## first filling species_new for all
  # check$species_new <- ifelse(is.na(check$species_new),
  #                             as.character(check$species),
  #                             check$species_new)

  # definindo prevalencia
  prev <- c("affinis", "conferre", "subspecies", "variety", "forma", "infra_specific",
            "subspecies|hybrid_species", "hybrid_species", "incertae_sedis",
            "species_nova", "indet")

  #2. recognizing authors ####
  #Escaping the issues from function flora::remove.authors
  no_authors <- check$species_new
  prob.ids <- grepl(" f\\. | form\\. | \u00D7 ", check$species_new, perl = TRUE) |
    (grepl(" [A-Z]|\\(", check$species_new, perl = TRUE) &
      check$species_status %in% "infra_specific")
  no_authors[prob.ids] <-
    gsub(" [A-Z].*| \\(.*| [a-z][a-z] .*", "", no_authors[prob.ids], perl = TRUE)

  #Other cases where flora::remove.authors works fine
  no_authors[!prob.ids] <- sapply(no_authors[!prob.ids],
                                  function(x) flora::remove.authors(x))
  # no_authors <- sapply(check$species_new,
  #                      function(x) flora::remove.authors(flora::fixCase(x)))
  # aqui aff cf subsp var e indet prevalescem
  ### Sara: nao consegui entender porque a distincao para nomes em nivel de especie ou genero: ambos podem tem autor
  id_authors <- #is.na(check$species_status) | #&
    check$species_new != no_authors &
      grepl(" [A-Z]| \\(", check$species_new, perl = TRUE) &
        !grepl("^cf\\.|^aff\\.", check$species_new, perl = TRUE, ignore.case = TRUE)
    #sapply(strsplit(as.character(check$species), " "), length) > 2
  ### Sara, aqui era o ponto que des-indexava os nomes com autores; deixei comentado por agora
  # id_authors <- id_authors & !check$species_status %in% prev |
  #   id_authors & sapply(strsplit(as.character(no_authors), " "), length) > 2 |
  #   sapply(strsplit(as.character(no_authors), " "), length) == 1 # genus + author
  # removing f. in the end of author name

  # no_authors <- stringr::str_squish(gsub("f\\.$", "", no_authors, perl = TRUE))
  no_authors <- gsub("f\\.$", "", no_authors, perl = TRUE)
  no_authors <- gsub("\\s+", " ", no_authors, perl = TRUE)
  no_authors <- gsub("^ | $", "", no_authors, perl = TRUE)

  ### Sara, aqui tb estava substituindo os nomes com var./subsp.; deixei comentado por agora
  # no_authors <- ifelse(sapply(stringr::str_split(no_authors, " "), length) > 2,
  #                      sapply(stringr::str_split(no_authors, " "), function(x) paste(x[1], x[2])),
  #                      no_authors)
  check$species_status[id_authors & !is.na(check$species_status)] <-
    paste(check$species_status[id_authors & !is.na(check$species_status)], "name_w_authors", sep = "|")
  check$species_status[id_authors & is.na(check$species_status)] <- "name_w_authors"
  check$species_new[id_authors] <- no_authors[id_authors]

  #2.1 Removing open taxonomy, ranks and hybrid notation
  #Symplifying the status column for easier manipulation
  status <- gsub("\\|name_w_authors|\\|name_w_wrong_case", "",
                 check$species_status, perl = TRUE)
  ## cleaning affinis e conferre
  check$species_new[status %in% c("affinis", "conferre")] <-
    rmOpen(check$species_new[status %in% c("affinis", "conferre")])

  ## cleaning hybrids
  check$species_new[status %in% "hybrid_species"] <-
    rmHyb(check$species_new[status %in% "hybrid_species"])

  ## cleaning infra-species
  check$species_new[status %in% c("subspecies", "variety", "forma")] <-
    rmInfra(check$species_new[status %in% c("subspecies", "variety", "forma")])

  #3. sp. nov.####
  #sp. nov., spec. nov., sp. n., nov. sp., nov. spec. or n. sp.
  spnov_regex <- "\\ssp\\.\\snov\\.|\\sspec\\.\\snov\\.|\\ssp\\.\\sn\\.|\\snov\\.\\ssp\\.
  |\\snov\\.\\sspec\\.|\\sn\\.\\sp\\."
  spnov <- stringr::str_detect(check$species,
                               stringr::regex(spnov_regex,
                                              ignore_case = TRUE))
  check$species_status[spnov] <- "species_nova"
  check$species_new[spnov] <- species[spnov]

  #3. sp. or genus only ####
  indet_regex <- "[[:space:]]sp\\.$|[[:space:]]sp$|[[:space:]]sp\\.|[[:space:]]indet\\.|[[:space:]]ind\\.|[[:space:]]sp[[:space:]]"
  no_sp <- sapply(stringr::str_split(check$species_new, " "),
                  length) < 2
  indet <- stringr::str_detect(check$species,
                               stringr::regex(indet_regex,
                                              ignore_case = TRUE)) &
    !check$species_status %in% c(prev, "species_nova")
  question <- stringr::str_detect(check$species, "\\?")
  check$species_status[(no_sp | indet | question) &
                         !is.na(check$species_status)] <-
    paste("indet", check$species_status[(no_sp | indet | question) &
                                 !is.na(check$species_status)], sep = "|")
  check$species_status[(no_sp | indet | question) &
                         is.na(check$species_status)] <- "indet"

  #4. recognizing digits ####
  id_digits <- stringr::str_detect(check$species_new, '\\d') &
                !check$species_status %in% prev
  check$species_status[id_digits] <- "not_name_has_digits"

  #6. names not matching Genus + species pattern ####
  # de novo incluir prevalencia
  id_not_gensp <- sapply(stringr::str_split(check$species_new, " "),
                         length) > 2 &
                          !status %in% c(prev, "species_nova")
  check$species_status[id_not_gensp] <- "not_Genus_epithet_format"

  #7. case ####
  ### Sara, fazendo esse passo lah em cima agora
  # case <- sapply(check$species_new, flora::fixCase)
  # # aff cf subsp var e indet prevalescem
  # id_case <- check$species_new != case &
  #   !check$species_status %in% c(prev, "incertae_sedis")
  # check$species_status[id_case] <- "name_w_wrong_case"
  # check$species_new[id_case] <- case[id_case]

  #8. aceae in first string ####
  gen <- gsub(" .*", "", check$species_new, perl = TRUE)
  # gen <- sapply(stringr::str_split(check$species_new, stringr::fixed(" ")),
  #               function(x) x[1])
  id_gen <- endsWith(gen, "aceae")
  check$species_status[id_gen] <- "family_as_genus"

  #9. order as genus ####
  id_ord <- endsWith(gen, "ales")
  check$species_status[id_ord] <- "order_as_genus"

  #9.5 subfamily as genus ####
  id_sub <- endsWith(gen, "deae")
  check$species_status[id_sub] <- "subfamily_as_genus"


  #10. hybrid
  ### Sara, passei essa parte para cima para ficar coerente com aff., cf., etc
  # hybrid_symbol <- stringr::str_detect(check$species, "\u00D7")
  # hybrid_string <- "[[:space:]]x[[:space:]]"
  # hybrid_x <- stringr::str_detect(check$species,
  #                                 stringr::regex(hybrid_string, ignore_case = TRUE))
  # hybrid <- hybrid_symbol | hybrid_x
  # check$species_status[hybrid] <- "hybrid_species"
  # check$species_new[hybrid] <- as.character(check$species)[hybrid]
  # check$species_new[hybrid] <- gsub(hybrid_string,
  #                                   paste0(" ", "\u00D7"),
  #                                   as.character(check$species))[hybrid]

  # 11 abreviated genus ####
  abbrev_gen <- gsub("\\.", "", gen, perl = TRUE)
  abbrev_gen <- nchar(abbrev_gen) == 1
  check$species_status[abbrev_gen] <- "abbreviated_genus"

  #11. possibly ok ####
  check$species_status[is.na(check$species_status)] <- "possibly_ok"

  #12. non-ascii ####
  string_type <- stringi::stri_enc_mark(check$species_new)
  check$species_status[status %in% c("possibly_ok", "name_w_wrong_case",
                                    "subspecies", "variety", "forma")
                       & string_type != "ASCII"] <-
    paste(check$species_status[status %in% c("possibly_ok", "name_w_wrong_case",
                                       "subspecies", "variety", "forma")
                         & string_type != "ASCII"], "name_w_non_ascii", sep = "|")

  # padronizando estilo de nomenclatura
  names(check)[names(check) == "species"] <- 'verbatimSpecies'
  names(check)[names(check) == "species_new"] <- 'scientificName.new'
  names(check)[names(check) == "species_status"] <- 'scientificNameStatus'
  check[, tax.name] <- as.character(unique(x[, tax.name]))

  #13. option to return names with or without infra-specific ranks
  if (!rm.rank) {
    if (any("variety" %in% status))
      check$scientificName.new[status %in% "variety"] <-
        addRank(check$scientificName.new[status %in% "variety"], "var.")

    if (any("subspecies" %in% status))
      check$scientificName.new[status %in% "subspecies"] <-
        addRank(check$scientificName.new[status %in% "subspecies"], "subsp.")

    if (any("forma" %in% status))
      check$scientificName.new[status %in% "forma"] <-
        addRank(check$scientificName.new[status %in% "forma"], "f.")

    if (any("hybrid_species" %in% status))
      check$scientificName.new[status %in% "hybrid_species"] <-
        addRank(check$scientificName.new[status %in% "hybrid_species"], "\u00d7")

    check$scientificName.new <-
      gsub(" NA$", "", check$scientificName.new, perl = TRUE)
  }

  #14. option to return names with or without unidentified abbreviations
  if (rm.indet) {
    indet.ids <- check$scientificNameStatus %in%
      c("indet", "family_as_genus", "order_as_genus", "subfamily_as_genus")
    check$scientificName.new[indet.ids] <-
      gsub(" sp\\..*", "", check$scientificName.new, perl = TRUE)[indet.ids]

  } else {
    indet.ids <- check$scientificNameStatus %in%
      c("indet", "family_as_genus", "order_as_genus", "subfamily_as_genus")
    sp.ids <- grepl(" sp\\.|spp\\.", check$scientificName.new, perl = TRUE)
    check$scientificName.new[indet.ids & !sp.ids] <-
      paste0(check$scientificName.new[indet.ids & !sp.ids], " sp.")
    check$scientificName.new <-
      gsub(" NA sp\\.$", " sp.", check$scientificName.new, perl = TRUE)
    check$scientificName.new <-
      gsub("^na sp\\.$", NA, check$scientificName.new,
           perl = TRUE, ignore.case = TRUE)
  }

  # preparing the output
  check1 <- suppressMessages(dplyr::left_join(x,
                             check[,c('scientificName.new', 'scientificNameStatus',
                                   tax.name)]))
  # check1$scientificName.new <-
  #   stringr::str_squish(as.character(check1$scientificName.new))
  check1$scientificName.new <-
    as.character(check1$scientificName.new)
  check1$scientificName.new <-
    gsub("\\s+", " ", check1$scientificName.new, perl = TRUE)
  check1$scientificName.new <-
    gsub("^ | $", "", check1$scientificName.new, perl = TRUE)

  return(check1)
}
