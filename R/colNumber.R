#' @title Format Collector Number
#'
#' @description Cleans the the collector number field from herbarium labels.
#'
#' @param x the character string.
#'
#' @return the character string \code{x}
#'
#' @details The function puts
#'
#' @author Lima, R.A.F.
#'
#' @references Willemse, L.P., van Welzen, P.C. & Mols, J.B. (2008). Standardisation in data-entry across databases: Avoiding Babylonian confusion. Taxon 57(2): 343-345.
#'
#' @export colNumber
#'
#' @examples
#' # A vector with some typical examples of formats found in herbarium labels
#' numbers = c("3467","3467 "," 3467","ALCB3467","Gentry 3467","ALCB-3467","3467a","3467A","3467 A",
#' "3467-A","PL671","57-685","685 - 4724","1-80","-4724","(80)","(80","80)","32-3-77",
#' "s/n.","s.n.","s.n","s/nº","",NA)
#' # Using the function defaults
#' colNumber(numbers)
#' # Using the function to remove the collection code from the collector number
#' colNumber(numbers, colCodes = c("ALCB","ESA"))
#' # Defining user-specific abbreviations for the specimens without collector number
#' colNumber(numbers, colCodes = c("ALCB","ESA"), noNumb = "n.a.")
#'
colNumber = function(x, colCodes = NULL, noNumb = "s.n.") {
  ###REMOVER DATA DA COLETA DO INICIO DO NUMERO
  ###REVER MAIS POSSIBLIDADES DE EDICAO
  # check input:
  #if (length(x)>1) { stop("input 'name' cannot be a vector of strings!") }

  # first edits
  x = gsub("  ","",x)
  x = gsub("  ","",x)
  numbs = x

  # Number not given
  numbs[is.na(numbs)] = "SemNumero"
  numbs[numbs %in% 0] = "SemNumero"
  numbs = gsub('s\\.n\\.|s\\.n|s/nº|S/N|S\\.N\\.|s/nº|s/n°|^s/n$|^s/n\\.$',"SemNumero",numbs)
  numbs[numbs %in% "sn"] = "SemNumero"
  numbs[is.na(numbs)|numbs %in% c("")] = "SemNumero"
  numbs[numbs %in% c("Number unspecified")] = "SemNumero"
  numbs[is.na(numbs)&grepl(" s.n. ",numbs)] = "SemNumero"
  # Removing the collection code from the beggining of the collection number
  if(!is.null(colCodes)) numbs[!is.na(numbs) & grepl(paste("^", colCodes, collapse = "|", sep=""),numbs, ignore.case = TRUE)] =
    gsub(paste("^",colCodes,collapse = "|",sep=""),"",numbs[!is.na(numbs) & grepl(paste("^", colCodes, collapse = "|", sep=""), numbs, ignore.case = TRUE)])
  # Removing names of collectors and others codes from the beginning of the numbers
  numbs[!is.na(numbs)&grepl("[a-z][a-z][a-z] ",numbs, ignore.case = TRUE)] = as.character(sapply(strsplit(numbs[!is.na(numbs)&grepl("[a-z][a-z][a-z] ",numbs,ignore.case = TRUE)], " "),function(x) x[grepl('[0-9]|SemNumero',x)]))
  numbs[!is.na(numbs)&grepl("SemNumero",numbs)] = "SemNumero"
  numbs[!is.na(numbs)&grepl("character\\(0\\)",numbs)] = "SemNumero"
  #Removing unwanted characters ans spacing
  numbs = gsub(' - ',"-",numbs)

  #Removing misplaced parenteses
  numbs = gsub(' \\(',"\\(",numbs)
  numbs = gsub('\\) ',"\\)",numbs)
  numbs[grepl('^\\(',numbs) & ! grepl('\\)$',numbs)] = gsub('^\\(', '', numbs[grepl('^\\(',numbs) & ! grepl('\\)$',numbs)])
  numbs[!grepl('^\\(',numbs) &  grepl('\\)$',numbs)] = gsub('\\)$', '', numbs[!grepl('^\\(',numbs) &  grepl('\\)$',numbs)])

  #Replacing orfan spaces by separators
  numbs = gsub(' ',"-",numbs)

  #Including separators between number qualificators
  numbs[grepl('[0-9] [A-Z]', numbs, ignore.case = TRUE)] = gsub(' ',"-",numbs[grepl('[0-9] [A-Z]', numbs, ignore.case = TRUE)])
  #PUT THIS FUNCTION IN PACKAGE DOCUMENTATION?
  f1 = function(x) {
    x1 = strsplit(x,"")[[1]]
    x2 = as.character(paste(x1[grepl('[0-9]',x1)],sep="",collapse = ""))
    x3 = as.character(paste(x1[grepl('[a-z]',x1, ignore.case = TRUE)],sep="",collapse = ""))
    x4 = paste(x2,toupper(x3),sep="-")
  }
  numbs[grepl('[0-9][A-Z]', numbs, ignore.case = TRUE)] = sapply(numbs[grepl('[0-9][A-Z]', numbs, ignore.case = TRUE)], FUN = f1)

  numbs = gsub(' e ',", ",numbs)
  numbs = gsub('#|\\?|\\!|\\.',"",numbs)
  numbs = gsub(", ",",",numbs)
  numbs = gsub("Collector Number:","",numbs)
  numbs = gsub("NANA","SemNumero",numbs)
  numbs = gsub('^--$',"SemNumero",numbs)
  numbs = gsub('^-',"",numbs)
  numbs = gsub('-$',"",numbs)
  numbs[!grepl('[0-9]',numbs)] = "SemNumero"

  # Replacing the missing number by a standard code, provided as an argument in the function
  numbs = gsub("SemNumero",noNumb, numbs)

  # Final edits
  numbs = gsub("--","-",numbs)
  numbs = gsub("&nf;","",numbs)
  numbs = str_trim(numbs)
  #numb = gsub('[a-z]-[0-9]','',numb, ignore.case=TRUE) ##CHECK

  return(numbs)
}
