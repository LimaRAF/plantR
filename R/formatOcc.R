#' @title Format Names, Numbers and Dates
#'
#' @description This function standardizes names, number and dates from data obtained from on-line data bases such as GBIF and speciesLink
#'
#' @param x a data frame or data table
#' @param noNumb standard for missing data in Number
#' @param noYear standard for missing data in Year
#' @param noName standard for missing data in Name
#'
#' @import data.table
#'
#' @export formatOcc
#'
formatOcc = function(x, noNumb, noYear, noName) {
  #require(data.table)
 "recordedBy.new" <- "recordedBy" <- "identifiedBy.new" <- NULL
 "identifiedBy" <- "recordNumber.new" <- "recordNumber" <- NULL
 "year.new" <- "dateIdentified.new" <- "dateIdentified" <- NULL
 "recordedBy.aux" <- "identifiedBy.aux" <- "last.name" <- NULL
  x$order = 1:dim(x)[1]
  occs = data.table::data.table(x)
  data.table::setkeyv(occs, c("order"))

  #For the year of collection, sometimes the information is stored on the field 'evenDate' and not on the field 'year'
  occs$year[is.na(occs$year) & !is.na(occs$eventDate)] = occs$eventDate[is.na(occs$year) & !is.na(occs$eventDate)]

  #We then prepare the new fields for further processing
  occs[, recordedBy.new := fixName(recordedBy, special.char = FALSE),  by = order]
  occs[, identifiedBy.new := fixName(identifiedBy, special.char = FALSE),  by = order]
  occs[, recordNumber.new := colNumber(recordNumber, noNumb = "s.n."),  by = order]
  occs[, year.new := getYear(year, noYear = "n.d."),  by = order]
  occs[, dateIdentified.new := getYear(dateIdentified, noYear = "n.d."),  by = order]

  #Next, we format the names
  occs[, recordedBy.new := formatName(recordedBy.new), by = order]
  occs[, identifiedBy.new := formatName(identifiedBy.new), by = order]

  #We then create the auxiliary names list and convert auxiliary and first names to the TDWG format (function ``` tdwgNames ```)
  occs[, recordedBy.aux := as.character(tdwgNames(recordedBy.new, out="aux", sep.out = "; ")), by = order]
  occs[, identifiedBy.aux := as.character(tdwgNames(identifiedBy.new, out="aux", sep.out = "; ")), by = order]
  occs[, recordedBy.new := tdwgNames(recordedBy.new, out="first"), by = order]
  occs[, identifiedBy.new := tdwgNames(identifiedBy.new, out="first"), by = order]

  #It is also useful for the validation process to standardize the notation for missing collector and identificator name
  occs[, recordedBy.new := missName(recordedBy.new, type = "collector", noName = "s.n."), by = order]
  occs[, identifiedBy.new := missName(identifiedBy.new, type = "identificator", noName = "s.n."), by = order]

  #And to extract the last name of the collector
  occs[, last.name := lastName(recordedBy.new), by = order]

  #Removing the ordering column created
  occs[, order := NULL,]

  #(Add a final correctionsfunction)
  #   #Final corrections
  #   nomes = gsub('\\. \\.',".",nomes)
  #   nomes = gsub('\\(\\.\\)',".",nomes)
  #   nomes = gsub("\\.\\'\\.",".",nomes)
  #   nomes = gsub('\\.\\.\\.',".",nomes)
  #   nomes = gsub('\\.\\.',".",nomes)
  #   nomes = gsub('\\/,',",",nomes)
  #   nomes = gsub('\\/-,',',',nomes)
  #   nomes = gsub(',,',",",nomes)
  #   nomes = str_trim(nomes)
  #   #final edits
  #   nomes = gsub('a?',"o",nomes)
  #   nomes = gsub('a?|a?',"e",nomes)
  #   nomes = gsub('a?',"o",nomes)
  #   nomes = gsub("a?","a",nomes)
  #   nomes = gsub('a?|a?|a?',"u",nomes)
  #   nomes = gsub("a?","c",nomes)
  #   nomes = gsub("a?","o",nomes)
  #   nomes = gsub("a?|a?|a?|a?","a",nomes)

  df <- as.data.frame(occs)
  return(df)
}
