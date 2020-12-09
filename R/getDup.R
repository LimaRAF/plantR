#' @title Search For Duplicate Specimens Among Collections
#'
#' @description This function ...
#'
#' @param df a data frame with the colection code (first column) and the strings
#'   to be used for the duplicate search (second and other columns), typically
#'   the exit of the function `prepDup`.
#' @param flag.ind logical. Should duplicates based only on indirect duplicates
#' be flagged with brackets? Default to TRUE.
#'
#' @author Renato A. F. de Lima
#'
#' @details The input data frame \code{df} must contain at least the columns ...
#'
#' @return
#'
#' see: https://stackoverflow.com/questions/56740990/optimizing-grepl-and-other-operations-inside-a-loop-using-data-table
#'
#'
#' @importFrom stringr str_trim str_count
#' @import data.table
#' @importFrom igraph graph_from_data_frame components
#'
#' @export getDup
#'
getDup <- function(df, flag.ind = TRUE, ...) {

  # check input
  if (!class(df) == "data.frame")
    stop("Input object needs to be a data frame!")

  #Escaping R CMD check notes from using data.table syntax
  numTombo <- value <- i.memb <- new_id <- NULL
  dup.srch.str1 <- dup.srch.str2 <- dup.srch.str3 <- dup.srch.str4 <- NULL

  #Getting the search string names
  str_names <- names(df)[-1]

  #Creating the new columns used in the search
  df$dup.check <- NA
  df$dup.numb <- NA
  df$dup.prop <- NA
  df$dup.ID <- NA

  ##Checking for the duplicates (top down and bottom up)
  for(i in 1:length(str_names)) {
    col_name <- paste0("dup.check",i)
    forward <- duplicated(df[,str_names[i]], incomparables = NA)
    reverse <- duplicated(df[,str_names[i]], incomparables = NA, fromLast = TRUE)
    df[,col_name] <- forward | reverse
  }

  #### Number of duplicated strings found ####
  df$dup.numb[apply(df[,grepl("dup.srch.str[0-9]", names(df)), drop = FALSE], 1, function(x) all(is.na(x)))] <- "cc"
  df$dup.numb[apply(df[,grepl("dup.check[0-9]", names(df)), drop = FALSE], 1, function(x) all(x))] <- length(str_names)
  df$dup.numb[is.na(df$dup.numb)] <-
    apply(df[is.na(df$dup.numb), grepl("dup.check[0-9]", names(df)), drop = FALSE], 1, sum, na.rm = TRUE)

  #### Proportion of duplicated strings found among te possible non-NA strings (max. 1) ####
  tmp <- df[, grepl("dup.check[0-9]", names(df)), drop = FALSE]
  tmp1 <- apply(tmp, 1, function(x)
    sum(!is.na(x)))
  tmp1[!df$dup.numb %in% "cc"] <-
    as.double(df$dup.numb[!df$dup.numb %in% "cc"]) /
    tmp1[!df$dup.numb %in% "cc"]
  tmp1[df$dup.numb %in% "cc"] <- "cc"
  df$dup.prop <- tmp1

  #### Assigning a unique column with occurrences with any indication of duplicates ####
  df$dup.check[!df$dup.numb %in% "cc"] <-
    df$dup.numb[!df$dup.numb  %in% "cc"] > 0
  df$dup.check[df$dup.numb %in% "cc"] <- "cannot_check"
  # rm(list=ls()[! ls() %in% c("dados")])

  #### Defining the unique duplicated code ####
  ## Filtering data with any evidence of duplicates
  df1 <- df[df$dup.check %in% "TRUE", ]  # only data with any indication of duplicates
  df1$ordem <- 1:dim(df1)[1]

  ## Filtering the dataset and converting to the data.table format
  cols <- c("ordem","numTombo", names(df)[grepl("dup.srch.str[0-9]", names(df))])
  dt <- df1[, cols]  # only data with any indication of duplicates
  dt <- data.table::data.table(dt)

  #Dealing with duplicated values of 'numTombo'
  dt[duplicated(numTombo), numTombo := paste(numTombo, 1:.N, sep="_dup"), by = numTombo]

  ## Finding indirect duplicated search strings
  ## HENRIK SOLUTION: both direct and indirect matches at once
  # melt data to long format (need to remove duplicated 'numTombo')
  d <- melt(dt[,-1], id.vars = "numTombo", na.rm = TRUE)
  # d <- data.table::melt.data.table(
  #       data.table:::unique.data.table(dt, by = "numTombo")[, -1],
  #       id.vars = "numTombo",
  #       na.rm = TRUE)

  # convert to graph
  g <- igraph::graph_from_data_frame(d[ , .(numTombo, value)])

  # get components
  memb <- igraph::components(g)$membership
  memb <- data.table::data.table(numTombo = names(memb),
                                 memb = memb)

  # add component id to original data
  data.table::setDT(dt)[memb, memb := i.memb, on = c(numTombo = "numTombo")]

  # putting duplicated numTombo back on their original format
  dt[numTombo %like% "_dup[0-9]", numTombo := gsub("_dup[0-9]", "", numTombo)]

  # concatenate 'id' by 'memb' column
  data.table::setkeyv(dt, c("memb"))
  dt[!is.na(memb), new_id := as.character(paste0(sort(numTombo), collapse = "|")), by = memb]

  ## Flagging groups of duplicates based only on indirect duplicates
  if (flag.ind) {
    if (length(str_names) == 2)
      dt[!is.na(new_id), new_id := ifelse(
        anyDuplicated(dup.srch.str1, incomparables = NA) > 0 |
          anyDuplicated(dup.srch.str2, incomparables = NA) > 0,
        new_id, paste0("[",new_id,"]",collapse="")), by = new_id]

    if (length(str_names) == 3)
      dt[!is.na(new_id), new_id := ifelse(
        anyDuplicated(dup.srch.str1, incomparables = NA) > 0 |
          anyDuplicated(dup.srch.str2, incomparables = NA) > 0 |
          anyDuplicated(dup.srch.str3, incomparables = NA) > 0,
        new_id, NA_character_), by = new_id]
    if (length(str_names) == 4)
      dt[!is.na(new_id), new_id := ifelse(
        anyDuplicated(dup.srch.str1, incomparables = NA) > 0 |
          anyDuplicated(dup.srch.str2, incomparables = NA) > 0 |
          anyDuplicated(dup.srch.str3, incomparables = NA) > 0 |
          anyDuplicated(dup.srch.str4, incomparables = NA) > 0,
        new_id, NA_character_), by = new_id]
  }

  # for groups of length one, set 'memb' to NA
  dt[dt[, .I[.N == 1], by = memb]$V1, new_id := NA]

  #Saving the duplicated IDs in the main data frame
  setkeyv(dt, c("ordem")) #re-ordering the data.table
  #table(df1$ordem == dt$ordem)
  df$dup.ID[df$dup.check %in% "TRUE"] <- as.character(dt$new_id)
  drop.cols <- names(df)[grepl("dup.check", names(df))]
  df1 <- df[,-which(names(df) %in% drop.cols)]

  return(df1)
}
