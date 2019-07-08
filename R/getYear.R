#' @title Get Year
#'
#' @description Extracts the year of collection or determination from a field containing dates.
#'
#' @param x the character string.
#'
#' @return the character string \code{x}
#'
#' @details The function puts...
#'
#' @author Lima, R.A.F.
#'
#' @references Willemse, L.P., van Welzen, P.C. & Mols, J.B. (2008). Standardisation in data-entry across databases: Avoiding Babylonian confusion. Taxon 57(2): 343-345.
#'
#' @export getYear
#'
#' @examples
#' # A vector with some typical examples of formats found in herbarium labels
#' dates = c("25/03/1981","25-03-1981","03/1981","03-1981","1981","1.981","1,1981",
#' "25/III/1981","25-III-1981","III/1981","III-1981","25031981","25 03 1981","'81",
#' "March 1981","Mar. 1981","n.d.","s.d.","s/d","",NA)
#' # Using the function to extract the year
#' getYear(dates)
#' # Using the function with a different character to indicate missing dates
#' getYear(dates, noYear = "n.d.")
#'
#' #not working for
#' dates = c("31.12.1982","1982.31.12", '31/12')
#' getYear(dates, noYear = 'pouette')
#'
#'
#'
getYear = function (x, noYear = "s.d.") {

  ### tmp = gsub('Data:|Date:',"", tmp, ignore.case=T)
  ### month.name

  tmp = x
  # Converting NA entries
  tmp[is.na(tmp) | tmp %in% ""] = noYear

  # Converting dates with no numbers
  tmp1 = tmp[!grepl('\\d',tmp)]
  tmp1 = noYear
  if(length(tmp1)>0) tmp[!grepl('\\d',tmp)] = tmp1

  # Converting names of months (add month names in Spanish)
  meses = c(month.name,
            "Janeiro","Fevereiro","Março","Abril","Maio","Junho","Julho","Agosto","Setembro","Outubro","Novembro","Dezembro")
  meses1 = paste(meses," ", sep= "")
  meses2 = paste(unique(substr(meses,1,3)),"\\. ", sep= "")
  meses3 = paste(unique(substr(meses,1,3))," ", sep= "")
  tmp[grepl(paste(meses1, collapse='|'),tmp)] =
    gsub(paste(meses1, collapse='|'), '', tmp[grepl(paste(meses1, collapse='|'),tmp)])
  tmp[grepl(paste(meses2, collapse='|'),tmp)] =
    gsub(paste(meses2, collapse='|'), '', tmp[grepl(paste(meses2, collapse='|'),tmp)])
  tmp[grepl(paste(meses3, collapse='|'),tmp)] =
    gsub(paste(meses3, collapse='|'), '', tmp[grepl(paste(meses3, collapse='|'),tmp)])

  #Romain months
  tmp = gsub('XII','12',tmp); tmp = gsub('XI','11',tmp);tmp = gsub('IX','09',tmp);tmp = gsub('X','10',tmp)
  tmp = gsub('VIII','08',tmp); tmp = gsub('VII','07',tmp); tmp = gsub('VI','06',tmp); tmp = gsub('IV','04',tmp); tmp = gsub('V','05',tmp)
  tmp = gsub('III','03',tmp); tmp = gsub('II','02',tmp); tmp = gsub('I','01',tmp)

  #years separated by points or commas
  tmp1 = tmp[grepl('^[0-9]\\.|^[0-9],',tmp)]
  tmp1 = as.character(sapply(strsplit(tmp1,"\\.|,"), function(x) paste(x,collapse = "")))
  if(length(tmp1)>0) tmp[grepl('^[0-9]\\.|^[0-9],',tmp)] = tmp1

  #complete dates, separated by slashs
  tmp1 = tmp[grepl('\\/',tmp)]
  tmp1 = as.character(sapply(strsplit(tmp1,"\\/"), function(x) x[nchar(x)>=4]))
  if(length(tmp1)>0) tmp[grepl('\\/',tmp)] = tmp1

  #complete dates in a sequence, not separated by '-'
  tmp1 = tmp[nchar(tmp)%in%8 & !grepl("-",tmp)]
  tmp1 = substr(tmp1,5,8)
  if(length(tmp1)>0) tmp[nchar(tmp)%in%8 & !grepl("-",tmp)] = tmp1
  tmp1 = tmp[nchar(tmp)%in%7]
  tmp1 = substr(tmp1,4,7)
  if(length(tmp1)>0) tmp[nchar(tmp)%in%7 & !grepl("-",tmp)] = tmp1
  tmp1 = tmp[nchar(tmp)%in%6]
  tmp1 = substr(tmp1,3,6)
  if(length(tmp1)>0) tmp[nchar(tmp)%in%6 & !grepl("-",tmp)] = tmp1
  tmp1 = tmp[nchar(tmp)%in%5]
  tmp1 = substr(tmp1,2,5)
  if(length(tmp1)>0) tmp[nchar(tmp)%in%5 & !grepl("-",tmp)] = tmp1

  #datea separeted by '-'
  tmp1 = tmp[grepl("-",tmp)]
  tmp1 = as.character(sapply(strsplit(tmp1,"-"), function(x) unique(x[nchar(str_trim(x))>=4])))
  if(length(tmp1)>0) tmp[grepl("-",tmp)] = tmp1

  #dates separated by spaces
  tmp1 = tmp[grepl(" ",tmp)]
  tmp1 = as.character(sapply(strsplit(tmp1," "), function(x) unique(x[nchar(x)>=4])))
  if(length(tmp1)>0) tmp[grepl(" ",tmp)] = tmp1

  return(tmp)
}
