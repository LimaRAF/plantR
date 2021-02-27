#' @title Edit Scientific Name
#'
#' @description Identifies open nomenclature (aff., cf.) in species scientific name,
#' classification under species level (var. and subsp.), and common mistakes in
#' the species scientific name. It creates a new column with the original
#' string and the new suggested name. It also flags problematic names (character
#' string with numbers, authors, wrong case, or other names besides genus and
#' epithet etc). Names can be returned with or without infra-specific ranks (var.
#' and subsp.) or abbreviations of unespecific names (sp. or spp.).
#'
#' @return
#' Data frame with `verbatimSpecies` as in the original input, `speciesStatus`
#' with the flags in original data and `species` with a suggestion for a more
#' correct species name. See Details for a description of flags in the column
#' `speciesStatus`.
#'
#' @details Possible flags returned in `species_status`: \describe{
#' \item{\code{possibly_ok}}{scientific name following the expected pattern 'Genus
#' epithet'}
#' \item{\code{not_Genus_epithet_format}}{scientific name not following
#' the expected pattern Genus epithet}
#' \item{\code{variety}}{species scientific name with variety}
#' \item{\code{subspecies}}{species scientific name with subspecies}
#' \item{\code{form}}{species scientific name with form}
#' \item{\code{confer}}{open nomenclature cf. in species scientific name}
#' \item{\code{affinis}}{open nomenclature aff. in species scientific name}
#' \item{\code{name_w_authors}}{species scientific name has authors}
#' \item{\code{not_name_has_digits}}{species scientific name has digits, not a
#'valid name}
#' \item{\code{indet}}{species identified only at genus level}
#' \item{\code{family_as_genus}}{family as genus, not a valid name}
#' \item{\code{order_as_genus}}{order as genus, not a valid name}
#' \item{\code{species_nova}}{species name contains an indication of a new
#' species, possibly not yet a valid name}
#' \item{\code{non_ascii}}{species name
#' has non ASCII characters, not a valid name}
#' \item{\code{hybrid_species}}{hybrid species} }
#'
#' @param x a data.frame containing the species name
#' @param tax.name character. Name of the columns containing the species name.
#'   Default to "scientificName"
#' @param rm.rank logical. Should the infra-specific rank abbreviation be
#'   removed from the name? Default to FALSE
#' @param rm.indet logical. Should the abbreviations for unespecific names (i.e.
#'   sp. or spp.) be removed? Default to FALSE
#'
#' @author Sara Mortara
#'
#' @references
#' Sigovini, M., Keppel, E. and Tagliapietra, D. (2016) Open Nomenclature in the
#' biodiversity era. Methods in Ecology and Evolution 7(10): 1217-1225.
#'
#' @examples
#' df <- data.frame(scientificName =
#' c("Lindsaea lancea", "Lindsaea lancea (L.) Bedd.",
#' "Lindsaea lancea var. angulata",
#' "Lindsaea Aff. lancea",
#' "Lindsaea", "Lindsaea sp.", "Lindsaeaceae sp.",
#' "Lindsaea aff. lancea (L.) Bedd."))
#'
#' fixSpecies(df)
#' fixSpecies(df, rm.rank = TRUE)
#' fixSpecies(df, rm.rank = TRUE, rm.indet = TRUE)
#'
#' @importFrom stringr str_detect str_replace str_split str_trim
#' @importFrom flora remove.authors fixCase trim
#' @importFrom stringi stri_enc_mark
#'
#' @export fixSpecies
#'
fixSpecies <- function(x = NULL,
                       tax.name = "scientificName",
                       rm.rank = FALSE,
                       rm.indet = FALSE) {

  ## check input
  if (!class(x) == "data.frame")
    stop("input object needs to be a data frame!")

  if (!tax.name %in% names(x))
    stop("Input data frame must have a column named: ", tax.name)

  #0. preliminary edits
  species <- x[, tax.name]
  species <- gsub("var\\.", "var. ", species, perl = TRUE)
  species <- gsub("subsp\\.", "subsp. ", species, perl = TRUE)
  species <- gsub("aff\\.", "aff. ", species, perl = TRUE)
  species <- gsub("cf\\.", "cf. ", species, perl = TRUE)

  ### INICIO PARTE INCLUIDA PELO RENATO ###
  species <- gsub(" var ", " var. ", species, fixed = TRUE)
  species <- gsub(" subsp ", " subsp. ", species, fixed = TRUE)
  species <- gsub(" aff ", " aff. ", species, fixed = TRUE)
  species <- gsub(" cf ", " cf. ", species, fixed = TRUE)

  indets <- c("indet", "indeterminada", "unclassified", "undetermined")
  rplc <- "Indet. sp."
  species <- gsub(paste0(paste0("^", indets,"$"), collapse = "|"),
                  rplc, species, perl = TRUE, ignore.case = TRUE)
  species <- gsub(paste0(paste0("^", indets," (?=[0-9])"), collapse = "|"),
                  rplc, species, perl = TRUE, ignore.case = TRUE)
  species <- gsub(paste0(paste0("^", indets,"(?=[0-9])"), collapse = "|"),
                  rplc, species, perl = TRUE, ignore.case = TRUE)
  species <- gsub(paste0(paste0("^", indets," sp(?=[0-9])"), collapse = "|"),
                  rplc, species, perl = TRUE, ignore.case = TRUE)
  species <- gsub(paste0(paste0("^", indets," sp\\."), collapse = "|"),
                  rplc, species, perl = TRUE, ignore.case = TRUE)
  species <- gsub(paste0(paste0("^(", indets[-1],")([A-Z])"), collapse = "|"),
                  "Indet. sp.\\2", species, perl = TRUE, ignore.case = TRUE)
  species <- gsub(paste0(paste0("^(", indets[1],")([A-Z])"), collapse = "|"),
                  "Indet. sp.\\2", species, perl = TRUE, ignore.case = TRUE)
  species <- gsub("^sp\\.(?=[0-9])|^sp(?=[0-9])",
                  "Indet. sp.", species, perl = TRUE, ignore.case = TRUE)

  species <-
    gsub("(eae)([0-9])|(ales)([0-9])", "\\1 sp.\\2", species, perl = TRUE)
  species <-
    gsub("(eae)([A-Z])|(ales)([A-Z])", "\\1 \\2", species, perl = TRUE)

  species <- gsub(" (sp\\.) ([0-9])$", " \\1\\2", species, perl = TRUE)
  species <- gsub(" (sp) ([0-9])$", " \\1.\\2", species, perl = TRUE)
  species <- gsub(" (sp)([0-9])$", " \\1.\\2", species, perl = TRUE)
  species <- gsub(" sp$", " sp.", species, perl = TRUE)

  species <- gsub("  ", " ", species, fixed = TRUE)
  species <- gsub("^ | $", "", species, perl = TRUE)
  ### FIM PARTE INCLUIDA PELO RENATO ###

  # ö: implement status parasite "f. sp." not f. from forma
  # rafl: se quiser diminuir o código acho que dá pra trocar [[:space:]] por '\\s'
  #1. Open nomenclature and infraspecies class ####
  form_string <- "[[:space:]]f\\.[[:space:]]|[[:space:]]form\\.[[:space:]]"
  inc_string <- "inc\\.[[:space:]]sed\\.|Incertae[[:space:]]sedis"
  aff_string <- "^aff\\.|^aff[[:space:]]|[[:space:]]aff\\.|[[:space:]]aff[[:space:]]"
  cf_string <- "^cf\\.|^cf[[:space:]]|[[:space:]]cf\\.|[[:space:]]cf[[:space:]]"
  subsp_string <-  "[[:space:]]ssp\\.|[[:space:]]subsp\\.|[[:space:]]subsp[[:space:]]|[[:space:]]ssp[[:space:]]"
  var_string <- "[[:space:]]var\\.|[[:space:]]var[[:space:]]"
  aff_cf <- paste(aff_string, cf_string, sep = "|")
  subsp_var <- paste(subsp_string, var_string, form_string, sep = "|")

  # detecting status
  aff <- stringr::str_detect(species, stringr::regex(aff_string, ignore_case = TRUE))
  cf <- stringr::str_detect(species, stringr::regex(cf_string, ignore_case = TRUE))
  subsp <- stringr::str_detect(species, stringr::regex(subsp_string, ignore_case = TRUE))
  var <- stringr::str_detect(species, stringr::regex(var_string, ignore_case = TRUE))
  inc <- stringr::str_detect(species, stringr::regex(inc_string, ignore_case = TRUE))
  form <- stringr::str_detect(species, stringr::regex(form_string, ignore_case = FALSE))
  check <- data.frame(species = as.character(species))

  # defining status
  check$species_status <- NA
  check$species_status[aff] <- "affinis"
  check$species_status[cf] <- "conferre"
  check$species_status[subsp] <- "subspecies"
  check$species_status[var] <- "variety"
  check$species_status[inc] <- "incertae_sedis"
  check$species_status[form] <- "forma"

  # accessory functions
  clean_open <- function(x)  {
    x_new <- stringr::str_replace(x, stringr::regex(aff_cf, ignore_case = TRUE), " ")
    x_new <- flora::trim(x_new)
    return(x_new)
  }
  clean_infra <- function(x){
    x_new <- unlist(lapply(stringr::str_split(x, stringr::regex(subsp_var, ignore_case = TRUE)),
                           function(x) x[1]))
    n_strings <- lapply(stringr::str_split(x_new, " "), length)
    infra_authors <- ifelse(n_strings > 2, TRUE, FALSE)
    x_new <- sapply(x_new, flora::remove.authors)
    return(as.character(x_new))
  }

  # providing cleaned name
  check$species_new <- NA

  ## affinis e conferre
  check$species_new[check$species_status
                    %in% c("affinis",
                           "conferre")] <- clean_open(check$species[check$species_status
                                                                    %in% c("affinis",
                                                                           "conferre")])
  ### PARTE RETIRADA PELO RENATO ###
  # Virou uma opção no fim da função, antes de retornar
  # check$species_new[check$species_status
  #                   %in% c("subspecies",
  #                          "variety",
  #                          "forma")] <- clean_infra(check$species[check$species_status
  #                                                                 %in% c("subspecies",
  #                                                                        "variety",
  #                                                                        "forma")])
  check$species_new[var] <- gsub(var_string, "", check$species[var], perl = TRUE)
  check$species_new[subsp] <- gsub(subsp_string, "", check$species[subsp], perl = TRUE)
  check$species_new[form] <- gsub(form_string, "", check$species[form], perl = TRUE)

  # other types of basic cleaning
  ## first filling species_new for all
  check$species_new <- ifelse(is.na(check$species_new),
                              as.character(check$species),
                              check$species_new)

  # definindo prevalencia
  prev <- c("affinis", "conferre", "subspecies", "variety", "forma", "incertae_sedis", "species_nova", "indet")

  #2. recognizig authors ####
  #### SARA: FAZER APENAS PARA OS NOMES DE ESPÉCIES NÃO REPETIDOS PARA DEMORAR MENOS o flora::remove.authors! ####
  no_authors <- sapply(check$species_new,
                       function(x) flora::remove.authors(flora::fixCase(x)))
  # aqui aff cf subsp var e indet prevalescem
  id_authors <- #is.na(check$species_status) | #&
    check$species_new != no_authors &
    sapply(strsplit(as.character(check$species), " "), length) > 2
  id_authors <- id_authors & !check$species_status %in% prev |
    id_authors & sapply(strsplit(as.character(no_authors), " "), length) > 2 |
    sapply(strsplit(as.character(no_authors), " "), length) == 1 # genus + author
  # removing f. in the end of author name
  no_authors <- flora::trim(gsub("f\\.$", "", no_authors))
  no_authors <- ifelse(sapply(stringr::str_split(no_authors, " "), length) > 2,
                       sapply(stringr::str_split(no_authors, " "), function(x) paste(x[1], x[2])),
                       no_authors)
  check$species_status[id_authors] <- "name_w_authors"
  check$species_new[id_authors] <- no_authors[id_authors]

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
  ### ö rafl: separar categorias "indet" de "genus_only"??
  indet_regex <- "[[:space:]]sp\\.$|[[:space:]]sp$|[[:space:]]sp\\.|[[:space:]]indet\\.|[[:space:]]ind\\.|[[:space:]]sp[[:space:]]"
  no_sp <- sapply(stringr::str_split(check$species_new, " "),
                  length) < 2
  indet <- stringr::str_detect(check$species,
                               stringr::regex(indet_regex,
                                              ignore_case = TRUE)) &
    !check$species_status %in% c(prev, "species_nova")
  question <- stringr::str_detect(check$species, "\\?")
  check$species_status[no_sp | indet | question] <- "indet"

  #4. recognizig digits ####
  id_digits <- stringr::str_detect(check$species, '\\d') &
    !check$species_status %in% prev
  check$species_status[id_digits] <- "not_name_has_digits"

  #6. names not matching Genus + species pattern ####
  # de novo incluir prevalencia
  id_not_gensp <- sapply(stringr::str_split(check$species_new, " "),
                         length) > 2 &
    !check$species_status %in% c(prev, "species_nova")
  check$species_status[id_not_gensp] <- "not_Genus_epithet_format"

  #7. case ####
  case <- sapply(check$species_new, flora::fixCase)
  # aff cf subsp var e indet prevalescem
  id_case <- check$species_new != case &
    !check$species_status %in% c(prev, "incertae_sedis")
  check$species_status[id_case] <- "name_w_wrong_case"
  check$species_new[id_case] <- case[id_case]

  #8. aceae in first string ####
  gen <- sapply(stringr::str_split(check$species_new, " "),
                function(x) x[1])
  id_gen <- endsWith(gen, "aceae")
  check$species_status[id_gen] <- "family_as_genus"

  #9. order as genus ####
  ord <- sapply(stringr::str_split(check$species_new, " "),
                function(x) x[1])
  id_ord <- endsWith(gen, "ales")
  check$species_status[id_ord] <- "order_as_genus"

  #9.5 subfamily as genus ####
  # ö: included by renato: ok?
  ord <- sapply(stringr::str_split(check$species_new, " "),
                function(x) x[1])
  id_ord <- endsWith(gen, "deae")
  check$species_status[id_ord] <- "subfamily_as_genus"


  #10. hybrid ####
  hybrid_symbol <- stringr::str_detect(check$species, "\u00D7")
  hybrid_string <- "[[:space:]]x[[:space:]]"
  hybrid_x <- stringr::str_detect(check$species,
                                  stringr::regex(hybrid_string, ignore_case = TRUE))
  hybrid <- hybrid_symbol | hybrid_x
  check$species_status[hybrid] <- "hybrid_species"
  check$species_new[hybrid] <- as.character(check$species)[hybrid]
  check$species_new[hybrid] <- gsub(hybrid_string,
                                    paste0(" ", "\u00D7"),
                                    as.character(check$species))[hybrid]

  # 11 abreviated genus ####
  genus <- sapply(stringr::str_split(check$species_new, " "), function(x) x[1])
  abbrev_gen <- gsub("\\.", "", genus)
  char_gen <- nchar(abbrev_gen)
  abbrev_gen <- char_gen == 1
  check$species_status[abbrev_gen] <- "abbreviated_genus"

  #11. possibly ok ####
  # ö: talvez mudar para algo mais assertivo, tipo 'binomial_ok'??
  check$species_status[is.na(check$species_status)] <- "possibly_ok"

  #12. non-ascii ####
  string_type <- stringi::stri_enc_mark(check$species_new)
  check$species_status[check$species_status %in% c("possibly_ok", "name_w_wrong_case",
                                                   "subspecies", "variety", "forma")
                       & string_type != "ASCII"] <- "name_w_non_ascii"

  ### PARTE ALTERADA PELO RENATO ###
  # padronizando estilo de nomenclatura
  names(check)[names(check) == "species"] <- 'verbatimSpecies'
  names(check)[names(check) == "species_new"] <- 'scientificName.new'
  names(check)[names(check) == "species_status"] <- 'scientificNameStatus'

  # organizando a saída
  check1 <- cbind.data.frame(x,
                             check[,c("scientificName.new", "scientificNameStatus")],
                             stringsAsFactors = FALSE)
  check1$scientificName.new <- as.character(check1$scientificName.new)

  #13. option to return names with or without infra-specific ranks
  if (!rm.rank) {
    add.rank <- function(x, rank = NULL){
      x_sp <- strsplit(x, " ", fixed = TRUE)
      x1 <- as.character(sapply(x_sp, function(y) paste(y[1], y[2], rank, y[3], collapse = " ")))
      return(x1)
    }
    check1$scientificName.new[check1$scientificNameStatus %in% "variety"] <-
      add.rank(check1$scientificName.new[check1$scientificNameStatus %in% "variety"], "var.")
    check1$scientificName.new[check1$scientificNameStatus %in% "subspecies"] <-
      add.rank(check1$scientificName.new[check1$scientificNameStatus %in% "subspecies"], "subsp.")
    check1$scientificName.new[check1$scientificNameStatus %in% "forma"] <-
      add.rank(check1$scientificName.new[check1$scientificNameStatus %in% "forma"], "f.")
    check1$scientificName.new <-
      gsub(" NA$", "", check1$scientificName.new, perl = TRUE)
  }

  #14. option to return names with or without unidentified abbreviations
  if (rm.indet) {
    indet.ids <- check1$scientificNameStatus %in%
      c("indet", "family_as_genus", "order_as_genus", "subfamily_as_genus")
    check1$scientificName.new[indet.ids] <-
      gsub(" sp\\..*", "", check1$scientificName.new, perl = TRUE)[indet.ids]

  } else {
    indet.ids <- check1$scientificNameStatus %in%
      c("indet", "family_as_genus", "order_as_genus", "subfamily_as_genus")
    sp.ids <- grepl(" sp\\.|spp\\.", check1$scientificName.new, perl = TRUE)
    check1$scientificName.new[indet.ids & !sp.ids] <-
      paste0(check1$scientificName.new[indet.ids & !sp.ids], " sp.")
    check1$scientificName.new <-
      gsub(" NA sp\\.$", " sp.", check1$scientificName.new, perl = TRUE)
    check1$scientificName.new <-
      gsub("^na sp\\.$", NA, check1$scientificName.new,
           perl = TRUE, ignore.case = TRUE)

  }

  return(check1)
}
