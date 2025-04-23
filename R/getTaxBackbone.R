#'
#' @title Get Reference Taxonomy
#'
#' @description Users can chose between the taxonomic backbone for
#'   vascular plants of the Brazilian Flora and Funga (the internal
#'   `plantR` dataset called `bfoNames` - the default) or to provide a
#'   data frame (or a list of data frames) with the taxonomic
#'   backbone(s) of their choice.
#'
#' @param db a character with the taxonomic backbone acronym or a data
#'   frame or list of data frames containing the taxonomic backbones.
#'
#' @return a list with as many elements as the length of db
#'
#' @details
#'   The expected format of the user-provided backbone is a data frame
#'   or a list of data frame that must contain at least the following
#'   columns: 'tax.name' and 'tax.authorship'. But ideally, it should have
#'   more columns such as the backbone taxon ID (column 'id'), other
#'   information on the taxa (i.e. columns 'phylum', 'family',
#'   'taxon.rank', 'taxon.status', 'name.status') and on the possibly
#'   accepted taxa (i.e. columns 'accepted.tax.name', 'accepted.tax.authorship',
#'   'accepted.taxon.rank', 'accepted.taxon.status',
#'   'accepted.name.status'). See the object 'bfoNames' for an example
#'   of the expected format of the taxonomic backbone.
#'
#'
#' @examples
#'
#' dbs <- getTaxBackbone("bfo")
#'
#'
#' @export
#'
getTaxBackbone <- function(db = NULL) {

  if (is.null(db))
    stop("The argument 'db' cannot be empty")

  if (inherits(db, "character")) {

    if (length(db) == 0)
      stop("The argument 'db' cannot be empty")

    if (any(db %in% c("tpl")))
      stop("Name checking using 'tpl' (The Plant List) is no longer supported",
           call. = FALSE)

    options.db <- c("bfo", "fbo") #, "wfo", "wcvp", "gbif", "lcvp")
    if (any(!db %in% options.db))
      stop("Please select one of the following options: ",
           paste(options.db, collapse = ", "), call. = FALSE)

    dbs <- vector("list", length(db))
    names(dbs) <- db

    if (length(db) == 1) {
      if (any(tolower(db) == "bfo" | tolower(db) == "fbo")) {
        dbs[[1]] <- plantR::bfoNames
      }

      ## Use a function to load backbones from plantRdata/data or to
      #download it
      # if (tolower(db) == "wfo")
      #   dbs[[1]] <- plantRdata:::wfoNames
      #
      # if (tolower(db) == "wcvp")
      #   dbs[[1]] <- plantRdata:::wcvpNames
      #
      # if (tolower(db) == "gbif")
      #   dbs[[1]] <- plantRdata:::gbifNamesPlantae

    }

    if (length(db) > 1) {

      for (i in seq_along(db)) {
        db.i <- db[i]

        if (any(tolower(db.i) == "bfo" | tolower(db.i) == "fbo"))
          dbs[[i]] <- plantR::bfoNames

        ## Use a function to load backbones from plantRdata/data or to
        #download it
        # if (tolower(db.i) == "wfo")
        #   dbs[[i]] <- plantRdata:::wfoNames
        #
        # if (tolower(db.i) == "wcvp")
        #   dbs[[i]] <- plantRdata:::wcvpNames
        #
        # if (tolower(db.i) == "gbif")
        #   dbs[[i]] <- plantRdata:::gbifNamesPlantae
      }
    }
  }

  if (inherits(db, "data.frame")) {

    dbs <- vector("list", 1)
    names(dbs) <- "user-provided"
    ref.cols <- colnames(plantR::bfoNames)

    if (dim(db)[1] == 0)
      stop("The reference 'db' data frame cannot be empty!",
           call. = FALSE)

    key.cols <- c('tax.name', 'tax.authorship')
    if (any(!key.cols %in% colnames(db)))
      stop("The reference 'db' data frame must contain the columns: ",
           paste(key.cols, collapse = ", "), call. = FALSE)

    opt.col <-  c('id', 'family', 'taxon.rank', 'taxon.status',
                  'name.status', 'accepted.tax.name', 'accepted.tax.authorship',
                  'accepted.taxon.rank', 'accepted.taxon.status',
                  'accepted.name.status')
    if (any(!opt.col %in% colnames(db))) {
      miss.col <- opt.col[!opt.col %in% colnames(db)]
      if ('id' %in% miss.col) {
        db[['id']] <- paste0("ref_db-", 1:dim(db)[1])
        miss.col <- miss.col[!miss.col %in% 'id']
      }
      for(i in miss.col) db[[i]] <- NA

      order.ids <-
        match(ref.cols, colnames(db), nomatch = 0)
      order.cols <- colnames(db)[order.ids]
      other.cols <- colnames(db)[!colnames(db) %in% order.cols]
      db <- db[, c(order.cols, other.cols)]
    }

    dbs[[1]] <- db
  }

  if (inherits(db, "list")) {

    n.bb <- length(db)
    dbs <- vector("list", n.bb)

    names.bb <- names(db)
    if (!is.null(names.bb)) {
      names(dbs) <- names.bb
    } else {
      names(dbs) <- make.unique(rep("user-provided", n.bb))
    }

    for (i in seq_len(n.bb)) {

      db.i <- db[[i]]

      if (dim(db.i)[1] == 0)
        stop("The reference 'db' data frame cannot be empty!",
             call. = FALSE)

      key.cols <- c('tax.name', 'tax.authorship')
      if (any(!key.cols %in% colnames(db.i)))
        stop("The reference 'db' data frame must contain the columns: ",
             paste(key.cols, collapse = ", "), call. = FALSE)

      opt.col <-  c('id', 'family', 'taxon.rank', 'taxon.status',
                    'name.status', 'accepted.tax.name', 'accepted.tax.authorship',
                    'accepted.taxon.rank', 'accepted.taxon.status',
                    'accepted.name.status')
      if (any(!opt.col %in% colnames(db.i))) {
        miss.col <- opt.col[!opt.col %in% colnames(db.i)]
        if ('id' %in% miss.col) {
          db.i[['id']] <- paste0("ref_db-", 1:dim(db.i)[1])
          miss.col <- miss.col[!miss.col %in% 'id']
        }
        for(j in miss.col) db.i[[j]] <- NA

        order.ids <-
          match(ref.cols, colnames(db.i), nomatch = 0)
        order.cols <- colnames(db.i)[order.ids]
        other.cols <- colnames(db.i)[!colnames(db.i) %in% order.cols]
        db.i <- db.i[, c(order.cols, other.cols)]
      }

      dbs[[i]] <- db.i
    }
  }

  return(dbs)
}
