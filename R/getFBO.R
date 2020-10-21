#### NEED TO FIX SOME ERROS IN:
#tombos: tombos com o estado: sapply(strsplit(vouchers$voucher, ","), function(x) x[1])
#coletores: ver caso de 'Acrocomia aculeata','Akrosida macrophylla'

getFBO <- function(taxa){
  ids <- flora::get.taxa(taxa)$id
  campos <-
    c("estadosCerteza",
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
      "referencias"
    )

  get_info <- function(id) {
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

  all_ids <- lapply(ids, get_info)
  all_ids_df <- do.call(rbind.data.frame, all_ids)
  all_ids_df$referencias <- gsub("<td>|<br></td>", "", all_ids_df$referencias)
  all_ids_df$bibliografiaReferencia <-
    as.character(gsub("^&lt;p&gt;|&lt;/p&gt;$|A&lt;/p&gt;$|amp;", "", all_ids_df$bibliografiaReferencia))
  all_ids_df$vouchers <- as.character(gsub("<td>|</td>|</a>", "", all_ids_df$vouchers))

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
  tombos <- sapply(tombos, function(x) sapply(strsplit(x, "<a href="), function(i) i[1]))
  tombos <- sapply(tombos, function(x) sapply(strsplit(x, "blank\">"), function(i) i[2]))
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
  all_ids_df$folha <- as.character(sapply(tmp, function(x) x[grepl("Folha: ", x)]))
  all_ids_df$inflor <- as.character(sapply(tmp, function(x) x[grepl("Infloresc", x)]))
  all_ids_df$flor <- as.character(sapply(tmp, function(x) x[grepl("Flor: ", x)]))
  all_ids_df$fruto <- as.character(sapply(tmp, function(x) x[grepl("Fruto: ", x)]))

  all_ids_df1 <- all_ids_df[,-which(names(all_ids_df) %in% c("vouchers","descricaoCamposControlados"))]

  facs <- sapply(all_ids_df1, is.factor)
  all_ids_df1[facs] <- lapply(all_ids_df1[facs], as.character)

  return(all_ids_df1)
}

