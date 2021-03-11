#' @title Download Species Information from the Brazilian Flora Online
#'
#' @description The function makes an online query for informations stored in the
#'   [Brazilian Flora Online](http://floradobrasil.jbrj.gov.br/reflora/listaBrasil).
#'
#' @param id Vector. A list of taxon IDs from the Brazilian Flora database.
#' @param campos Vector. A list of fields to be returned from the Brazilian
#'   Flora database.
#'
#' @keywords internal
#'
#' @importFrom jsonlite fromJSON
#'
downFBO <- function(id, campos = c("estadosCerteza",
                                   "descricaoCamposControlados",
                                   "descricaoLivrePT",
                                   "vouchers",
                                   "endemismo",
                                   "origem",
                                   "substrato",
                                   "dominioFitogeografico",
                                   "formaVida",
                                   "tipoVegetacao",
                                   "id",
                                   "nomeStr",
                                   "bibliografiaReferencia",
                                   "referencias")) {

  if (is.na(id))  {
    info_df <- matrix(
      rep(NA, length(campos)),
      nrow = 1,
      dimnames = list("", campos)
    )

  } else {

    info_flora <-
      jsonlite::fromJSON(
        paste0(
          "http://floradobrasil.jbrj.gov.br/reflora/listaBrasil/ConsultaPublicaUC/ResultadoDaConsultaCarregaTaxonGrupo.do?&idDadosListaBrasil=",
          id
        )
      )
    info_flora <- info_flora[which(names(info_flora) %in% campos)]
    info_flora <- lapply(info_flora, paste, collapse = "|")
    if (any(info_flora %in% ""))
      info_flora[info_flora %in% ""] <- lapply(info_flora[info_flora %in% ""],
                                               function(x) x[[1]] <- NA)
    info_df <- do.call(cbind.data.frame, info_flora)
    if (any(!campos %in% names(info_df))) {
      miss_campos <- campos[!campos %in% names(info_df)]
      info_df[, miss_campos] <- NA
    }

    info_df <- info_df[, campos]

  }
  return(info_df)
}
