#' @title Search For Duplicate Specimens Among Collections
#'
#' @description This function searches for duplicate specimens among collections, based
#' on duplicate search strings.
#'
#' @param df a data frame with the unique record identifier (in the first
#'   column) and the strings to be used for the duplicate search (second and
#'   other columns). See Examples.
#' @param flag.ind logical. Should duplicates based on indirect matches be
#'   flagged with brackets? Default to TRUE.
#'
#' @author Renato A. F. de Lima
#'
#' @return The input data frame with new columns containing the number and
#'   proportion of duplicated search strings found (i.e. 'dup.numb'and
#'   'dup.prop') and the duplicated ID string ('dup.ID'). If no duplicate was
#'   found dup.ID = NA.
#'
#' @details The function searches for duplicated specimens within and across
#'   collections based on one or more duplicate search strings, typically the
#'   output of the __plantR__ function `prepDup()`. These strings combine
#'   different types of information (i.e. taxonomy, collection and locality).
#'   For instance, a string combining information on taxonomy, collector last
#'   name and number, and collection locality would look like:
#'   ‘Myrtaceae_Silva_110_Curitiba’.
#'
#'   Strings too flexible (e.g. 'Silva_110') return more duplicated records but
#'   many may be false duplicates. Strings too strict, on the other hand, may
#'   miss true duplicates if some of the collections have not entered all search
#'   fields or if they were entered using different notation standards. Finding
#'   all existing duplicates requires that all collections are available and
#'   that all search fields are complete, filled in without typos and using the
#'   same notation standards. This is rarely the case, so the list of duplicates
#'   returned should be considered incomplete in many cases. Moreover, to make
#'   sure missing information does not prevent the retrieval of duplicates,
#'   using more than two combinations of fields is advised. See Lima et al.
#'   (2020) for an example of a conservative usage of different combinations
#'   of strings to find duplicated specimens.
#'
#'   The function returns only direct matches for up to one search string. If
#'   two or more search strings are provided, the search of duplicates uses tools
#'   from network analysis to find both direct and indirect matches of strings
#'   between records. If records are grouped under the same duplicated ID string
#'   ('dup.ID') but only based on indirect matches with other records, 'dup.ID'
#'   is returned between brackets (the default of argument `flag.ind`). These IDs
#'   may need to be inspected more closely to detect possible spurious matches.
#'
#'   Besides the duplicated ID, the function returns the number and proportion
#'   of duplicated search strings found for each record within its group of
#'   duplicates (i.e. 'dup.numb'and 'dup.prop'). These values can be used to assess
#'   the confidence level that records are indeed true duplicates within its group.
#'   The higher the 'dup.prop', the greater the chances that the record is indeed
#'   a duplicate. To calculate the proportion of duplicates found within the
#'   number of available search strings, mismatches due to different or to
#'   missing strings are treated the same.
#'
#'
#' @examples
#'
#' df <- data.frame(id=c("a_1","b_3","c_7","d_5","e_3",
#' "f_4","g_2","h_8","i_9","j_6","k_7","l_1"),
#' str1=c("a","b","c","l","l","p","p","p",NA,NA,"x","y"),
#' str2=c("d","d","e","k","k","o","o","o",NA,NA,"v","w"),
#' str3=c("f","g","f","n","n","s","r","s","t","t","z","u"),
#' str4=c("h","i","j","m","m","q","q","q",NA,NA,"ab","ac"))
#'
#' getDup(df)
#'
#' @references
#' Lima, R.A.F. et al. 2020. Defining endemism levels for biodiversity
#' conservation: Tree species in the Atlantic Forest hotspot. Biological
#' Conservation, 252: 108825.
#'
#' @seealso
#'  \link[plantR]{prepDup}.
#'
#' @importFrom stringr str_trim str_count
#' @import data.table
#' @importFrom igraph graph_from_data_frame components
#'
#' @export getDup
#'
getDup <- function(df = NULL, flag.ind = TRUE) {

  # check input
  if (!class(df) == "data.frame")
    stop("Input object needs to be a data frame!")

  #Escaping R CMD check notes from using data.table syntax
  tmp.ordem <- numTombo <- value <- i.memb <- new_id <- dup.ind.test <- . <- NULL

  #Getting the search string names
  rec.ID <- names(df)[1]
  str_names <- names(df)[-1]

  #Making sure the data.table does not contains factors
  changeCols <- sapply(df, class) == "factor"
  if (any(changeCols)) {
    changeCols <- names(changeCols)[changeCols]
    for(i in 1:length(changeCols))
      df[,changeCols[i]] <- as.character(df[,changeCols[i]])
  }

  #Creating the new columns used in the search
  df$dup.check <- NA
  df$dup.numb <- NA
  df$dup.prop <- NA
  df$dup.ID <- NA

  ##Checking for the duplicates (top down and bottom up)
  for(i in 1:length(str_names)) {
    col_name <- paste0("dup.check", i)
    forward <- duplicated(df[,str_names[i]], incomparables = NA)
    reverse <- duplicated(df[,str_names[i]], incomparables = NA, fromLast = TRUE)
    df[, col_name] <- forward | reverse
  }

  #### Number of duplicated strings found ####
  df$dup.numb[apply(df[, names(df) %in% str_names, drop = FALSE], 1,
                    function(x) all(is.na(x)))] <- "cc"
  df$dup.numb[apply(df[,grepl("dup.check[0-9]", names(df)), drop = FALSE], 1,
                    function(x) all(x))] <- length(str_names)
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

  #### Defining the unique duplicated code ####
  ## Filtering data with any evidence of duplicates
  df1 <- df[df$dup.check %in% "TRUE", ]  # only data with any indication of duplicates

  ## Filtering the dataset and converting to the data.table format
  dt <- data.table::data.table(df1)  # only data with any indication of duplicates
  dt[, tmp.ordem := .I,]

  #Dealing with duplicated values of 'numTombo'
  data.table::setnames(dt, rec.ID, "numTombo")
  dt[duplicated(numTombo), numTombo := paste(numTombo, 1:.N, sep="_dup"), by = numTombo]

  ## Finding indirect duplicated search strings
  # melt data to long format (need to remove duplicated 'numTombo')
  cols <- c(rec.ID, str_names)
  d <- data.table::melt.data.table(dt[, 1:(length(cols)),], id.vars = "numTombo", na.rm = TRUE)

  # convert to graph
  g <- igraph::graph_from_data_frame(d[ , .(numTombo, value)])

  # get components
  memb <- igraph::components(g)$membership
  memb <- data.table::data.table(numTombo = names(memb),
                                 memb = memb)

  # add component id to original data
  data.table::setDT(dt)[memb, memb := i.memb, on = c(numTombo = "numTombo")]

  # putting duplicated numTombo back on their original format
  `%like.ic%` <- function (x, pattern) {
    grepl(pattern, x, perl = TRUE, ignore.case = TRUE)
  }
  dt[numTombo %like.ic% "_dup[0-9]", numTombo := gsub("_dup[0-9]", "", numTombo, perl = TRUE)]

  # concatenate 'id' by 'memb' column
  data.table::setkeyv(dt, c("memb"))
  dt[!is.na(memb), new_id := as.character(paste0(sort(numTombo), collapse = "|")), by = memb]

  ## Flagging groups of duplicates based only on indirect duplicates
  if (flag.ind) {
    dup.cols <- data.table::copy(names(dt))
    dup.cols <- dup.cols[grepl("dup.check[1-9]", dup.cols, perl = TRUE)]
    for(i in 1:length(str_names)) {
      data.table::setkeyv(dt, c(str_names[i]))
      dt[NA_character_, (dup.cols[i]) := NA]
    }
    data.table::setkey(dt, new_id)
    dt[, dup.ind.test := lapply(.SD, function(x) any(sum(x) == 0, na.rm = TRUE)),
       by = new_id, .SDcols = c(dup.cols)]
    dt[!is.na(new_id), new_id := ifelse(dup.ind.test, paste0("[",new_id,"]",collapse=""), new_id), by = new_id]
  }

  # for groups of length one, set 'memb' to NA
  dt[dt[, .I[.N == 1], by = memb]$V1, new_id := NA]

  #Saving the duplicated IDs in the main data frame
  setkeyv(dt, c("tmp.ordem")) #re-ordering the data.table
  df$dup.ID[df$dup.check %in% "TRUE"] <- as.character(dt$new_id)

  #Filtering out unnecessary columns and returning
  drop.cols <- names(df)[grepl("dup.check", names(df))]
  df1 <- df[,-which(names(df) %in% drop.cols)]

  return(df1)
}
