#' @title Get Species Information from the Brazilian Flora Online
#'
#' @description For a list of species names occurring in Brazil, this function
#'   obtain different informations stored in the
#'   [Brazilian Flora Online](http://floradobrasil.jbrj.gov.br/reflora/listaBrasil),
#'   such as species occurrences by state, vouchers, life form, vegetation
#'   types, etc.
#'
#' @param taxa Vector. A list of taxa names occurring in the Brazilian Flora Online.
#' @param fields Vector. A list of fields to be returned.
#'
#'
#' @keywords internal
#'
#' @return the functions returns
#'
#' @details This function
#'
#' The time to run this function will depend on the umber of taxa to be
#' consulted in the Brazilian Flora database and your internet connection.
#'
#' If the Brazilian Flora server is down, the function will probably return a
#' timeout error.
#'

#### NEED TO FIX SOME ERROS IN:
#tombos: tombos com o estado: sapply(strsplit(vouchers$voucher, ","), function(x) x[1])
#coletores: ver caso de 'Acrocomia aculeata','Akrosida macrophylla'

getFBO <- function(taxa, fields = "vouchers") {

  #Getting the list of IDs from the BFO
  ids <- flora::get.taxa(taxa)$id

  #Downloading the informations for the list of IDs
  all_ids <- lapply(ids, downFBO)
  all_ids_df <- do.call(rbind.data.frame, all_ids)

  #Editing the downloaded information
  all_ids_df$referencias <- gsub("<td>|<br></td>", "", all_ids_df$referencias)
  all_ids_df$bibliografiaReferencia <-
    as.character(gsub("^&lt;p&gt;|&lt;/p&gt;$|A&lt;/p&gt;$|amp;", "", all_ids_df$bibliografiaReferencia))
  all_ids_df$vouchers <- as.character(gsub("<td>|</td>|</a>", "", all_ids_df$vouchers))

  #Getting voucher information
  tmp <- strsplit(all_ids_df$vouchers, "<br>")
  tmp1 <- sapply(tmp, function(x) strsplit(x, ", <a "))

  #Editing collector names and numbers
  cols <- sapply(tmp1, function(x) sapply(x, function(i) i[1]))
  cols <- sapply(cols, stringr::str_trim)
  cols <- lapply(cols, paste, collapse = "|")
  if (any(sapply(cols, is.null)))
    cols[sapply(cols, is.null)] <- lapply(cols[sapply(cols, is.null)],
                                          function(x) x[[1]] <- NA)
  all_ids_df$collectors <- as.character(unlist(cols))

  #Editing vouchers
  tombos <- sapply(tmp1, function(x) sapply(x, function(i) i[2]))
  tombos <- sapply(tombos, function(x) sapply(strsplit(x, "<a href=", perl = TRUE), function(i) i[1]))
  tombos <- sapply(tombos, function(x) sapply(strsplit(x, "blank\">", perl = TRUE), function(i) i[2]))
  tombos <- sapply(tombos, stringr::str_trim)
  tombos <- lapply(tombos, paste, collapse = "|")
  if (any(sapply(tombos, is.null)))
    cols[sapply(tombos, is.null)] <- lapply(tombos[sapply(tombos, is.null)],
                                            function(x) x[[1]] <- NA)
  all_ids_df$tombos <- as.character(unlist(tombos))

  #Editing species description
  all_ids_df$descricaoCamposControlados <-
    as.character(gsub("<strong>|</strong>", "", all_ids_df$descricaoCamposControlados))
  tmp <- strsplit(all_ids_df$descricaoCamposControlados, "\\. ")
  all_ids_df$folha <- as.character(sapply(tmp, function(x) x[grepl("Folha: ", x, perl = TRUE)]))
  all_ids_df$inflor <- as.character(sapply(tmp, function(x) x[grepl("Infloresc", x, perl = TRUE)]))
  all_ids_df$flor <- as.character(sapply(tmp, function(x) x[grepl("Flor: ", x, perl = TRUE)]))
  all_ids_df$fruto <- as.character(sapply(tmp, function(x) x[grepl("Fruto: ", x, perl = TRUE)]))

  all_ids_df1 <- all_ids_df[,-which(names(all_ids_df) %in% c("vouchers","descricaoCamposControlados"))]

  facs <- sapply(all_ids_df1, is.factor)
  all_ids_df1[facs] <- lapply(all_ids_df1[facs], as.character)

  return(all_ids_df1)
}

