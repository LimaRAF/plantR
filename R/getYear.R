#' @title Get Year From Date
#'
#' @description This function extracts the year from dates associated with
#' the collection or identifiation of biological records.
#'
#' @param x the character string or vector containing the dates.
#' @param noYear character. Standard notation for missing date information.
#'   Default to "n.d.".
#'
#' @return The year contained in the character string \code{x}.
#'
#' @details The function extracts the year from a given date in various
#'   different formats, but not all of them (see examples below). So, please
#'   consider putting your vector of dates in a standardized format prior to the
#'   year extraction (see note below).
#'
#'   For some types of format, the function assumes internaly that years are
#'   given as numbers with 4 digits or with 2 digits but higher than 31. If
#'   the year is not given in one of these formats, the function returns the
#'   date as it is provided.
#'
#' @author Renato A. F. de Lima
#'
#' @references
#'
#' Conn, Barry J. (ed.) (1996). HISPID 3 - Herbarium Information Standards and
#'   Protocols for Interchange of Data. Herbarium Information Systems Committee'
#'   (HISCOM). https://www.tdwg.org/standards/hispid3/
#'
#' Willemse, L.P., van Welzen, P.C. & Mols, J.B. (2008).
#'   Standardisation in data-entry across databases: Avoiding Babylonian
#'   confusion. Taxon 57(2): 343-345.
#'
#' @importFrom stringr str_trim
#'
#' @export getYear
#'
#' @examples
#' # A vector with some typical examples of formats found in herbarium labels
#' dates <- c("25/03/1981","25-03-1981","03/1981","03-1981","1981","1.981","1,981",
#'          "25/III/1981","25-III-1981","III/1981","III-1981","25031981","25 03 1981","'81",
#'          "March 1981","Mar. 1981","Mar 1981","25 Mar 1981","n.d.","s.d.","s/d","",NA,
#'          "1981-03-25", "81/01/25", "25/03/81", "21/12/21")
#'
#' # Using the function to extract the year
#' getYear(dates)
#'
#' # Using the function with a different character to indicate missing dates
#' getYear(dates, noYear = "n.d.")
#'
#' # Currently, the function does not work for (needs checking):
#' dates <- c("31.12.1982","1982.31.12", '31/12')
#' getYear(dates, noYear = 'pouette')
#'
#' @note If your dates are all stored in some specific format, you can convert
#' them to the ISO 8601 format (YYYY-MM-DD, e.g. 1981-03-25), which is the
#' standard format suggested by the Biodiversity Information Standards (TDWG).
#' To do so you have to provide your vector of dates and its format to the
#' as.Date function from the base R package. Some examples:
#'
#' 1) The date format in this case is '%d/%m/%y', i.e. day, month, two-digits
#' year, separated by slashs:
#'
#' dates <- c("25/03/81", "05/12/81")
#' as.Date(dates, format = "%d/%m/%y")
#'
#' 2) Date format is "%d/%b/%Y", i.e. day, month, four-digits year, separated
#' by slashs:
#'
#' dates <- c("25/03/1981", "05/12/1981")
#' as.Date(dates, format = "%d/%m/%Y")
#'
#' 3) Date format is '%d %b %y', i.e. day, abbreviated month, four-digits year,
#' separated by spaces:
#'
#' dates <- c("25 Mar 1981", "05 Dec 1981")
#' as.Date(dates, format = "%d %b %Y")
#'
#'
getYear <- function (x, noYear = "s.d.") {

  # Preliminary edits
  tmp <- gsub('Data:|Date:', "", x, perl = TRUE, ignore.case = TRUE)
  tmp <- gsub('Transcribed d/m/y: ', "", x, perl = TRUE, ignore.case = TRUE)
  tmp <- gsub('([0-9])(T).*', "\\1", tmp, perl = TRUE)
  tmp <- gsub('\\s[0-9]:.*', "\\1", tmp, perl = TRUE)
  tmp <- gsub('\\s[0-9][0-9]:.*', "\\1", tmp, perl = TRUE)
  tmp <- gsub('-NA', "-01", tmp, perl = TRUE)
  tmp <- gsub(' to ', ";;;", tmp, fixed = TRUE)

  # Converting NA entries
  tmp[tmp %in% c("", " ", NA, "T00:00:00Z", "0/0/0", "00/00/0000")] <- noYear

  # Converting dates with no numbers
  tmp1 <- tmp[!grepl('\\d', tmp, perl = TRUE)]
  if (length(tmp1) > 0)
    tmp[!grepl('\\d', tmp, perl = TRUE)] <- noYear

  # Removing names of months
  meses <- c(month.name,
            "Janeiro", "Fevereiro", "Mar\u00e7o", "Abril", "Maio", "Junho", "Julho",
            "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro",
            "Enero", "Febrero", "Marzo", "Mayo", "Junio", "Julio",
            "Septiembre", "Octubre", "Noviembre", "Deciembre")
  meses1 <- paste(meses, " ", sep = "")
  meses2 <- paste(unique(substr(meses, 1, 3)), "\\. ", sep = "")
  meses3 <- paste(unique(substr(meses, 1, 3)), " ", sep = "")
  tmp[grepl(paste(meses1, collapse = '|'), tmp, perl = TRUE)] <-
    gsub(paste(meses1, collapse = '|'), '',
         tmp[grepl(paste(meses1, collapse = '|'), tmp, perl = TRUE)])
  tmp[grepl(paste(meses2, collapse = '|'), tmp, perl = TRUE)] <-
    gsub(paste(meses2, collapse = '|'), '',
         tmp[grepl(paste(meses2, collapse = '|'), tmp, perl = TRUE)])
  tmp[grepl(paste(meses3, collapse = '|'), tmp, perl = TRUE)] <-
    gsub(paste(meses3, collapse = '|'), '',
         tmp[grepl(paste(meses3, collapse = '|'), tmp, perl = TRUE)])

  #Romain months (order of replacement matters...)
  tmp <- gsub('XII', '12', tmp, fixed = TRUE)
  tmp <- gsub('XI', '11', tmp, fixed = TRUE)
  tmp <- gsub('IX', '09', tmp, fixed = TRUE)
  tmp <- gsub('X', '10', tmp, fixed = TRUE)
  tmp <- gsub('VIII', '08', tmp, fixed = TRUE)
  tmp <- gsub('VII', '07', tmp, fixed = TRUE)
  tmp <- gsub('VI', '06', tmp, fixed = TRUE)
  tmp <- gsub('IV', '04', tmp, fixed = TRUE)
  tmp <- gsub('V', '05', tmp, fixed = TRUE)
  tmp <- gsub('III', '03', tmp, fixed = TRUE)
  tmp <- gsub('II', '02', tmp, fixed = TRUE)
  tmp <- gsub('I', '01', tmp, fixed = TRUE)

  #years separated by points or commas
  check_these <- grepl('^[0-9]\\.|^[0-9],', tmp, perl = TRUE)
  tmp1 <- as.character(sapply(strsplit(tmp[check_these], "\\.|,", perl = TRUE),
                              function(x) paste(x, collapse = "")))
  if (length(tmp1) > 0)
    tmp[check_these] <- tmp1

  #complete dates, separated by slashs (and no '-')
  check_these <- grepl('\\/', tmp, perl = TRUE) &
                  !grepl('\\-', tmp, perl = TRUE)
  tmp1 <- as.character(sapply(strsplit(tmp[check_these], "\\/|;;;", perl = TRUE),
                              function(x) paste0(unique(x[nchar(x) >= 4]),
                                                 collapse = "-")))
  if (any(tmp1 %in% ""))
    tmp1[tmp1 %in% ""] <-
      as.character(sapply(strsplit(tmp[check_these][tmp1 %in% ""], "\\/|;;;", perl = TRUE),
                        function(x) paste0(unique(
                          x[!is.na(x) & !x %in% "" & as.double(x) > 31]),
                          collapse = "-")))
  if (any(tmp1 %in% ""))
    tmp1[tmp1 %in% ""] <- tmp[check_these][tmp1 %in% ""]

  if (length(tmp1) > 0)
    tmp[check_these] <- tmp1

  #complete dates, separated by slashs and '-'
  check_these <- grepl('\\/', tmp, perl = TRUE) & grepl('\\-', tmp, perl = TRUE)
  tmp1 <- as.character(sapply(strsplit(tmp[check_these], "\\/|\\-", perl = TRUE),
                              function(x) paste0(unique(x[nchar(x) >= 4]),
                                                 collapse = "-")))
  if (length(tmp1) > 0)
    tmp[check_these] <- tmp1

  #complete dates in a sequence, not separated by '-'
  avoid_these <- !grepl("-", tmp, fixed = TRUE) &
                    !grepl("\\[", tmp, perl = TRUE) &
                      !grepl("\\/", tmp, perl = TRUE)
  check_these <- nchar(tmp) %in% 8 & avoid_these
  tmp1 <- substr(tmp[check_these], 5, 8)
  if (length(tmp1) > 0)
    tmp[check_these] <- tmp1

  check_these <- nchar(tmp) %in% 7 & avoid_these
  tmp1 <- substr(tmp[check_these], 4, 7)
  if (length(tmp1) > 0)
    tmp[check_these] <- tmp1

  check_these <- nchar(tmp) %in% 6 & avoid_these
  tmp1 <- substr(tmp[check_these], 3, 6)
  if (length(tmp1) > 0)
    tmp[check_these] <- tmp1

  check_these <- nchar(tmp) %in% 5 & avoid_these
  tmp1 <- substr(tmp[check_these], 2, 5)
  if (length(tmp1) > 0)
    tmp[check_these] <- tmp1

  #dates separeted by '-' (only numbers)
  check_these <- grepl("-", tmp, fixed = TRUE) &
                  !grepl('[a-z]', tmp, perl = TRUE, ignore.case = TRUE)
  tmp1 <- as.character(sapply(strsplit(tmp[check_these], "-", fixed = TRUE),
                              function(x) paste0(unique(x[nchar(stringr::str_trim(x)) >= 4]),
                                                 collapse = "-")))
  if (length(tmp1) > 0)
    tmp[check_these] <- tmp1

  #dates separeted by '-' (numbers and letters)
  check_these <- grepl("-", tmp, fixed = TRUE) &
                  grepl('[a-z]', tmp, ignore.case = TRUE)
  tmp1 <- as.character(sapply(strsplit(tmp[check_these], "-", fixed = TRUE),
                              function(x) x[grepl('\\d', x, perl = TRUE, ignore.case = TRUE)]))
  if (length(tmp1) > 0)
    tmp[check_these] <- tmp1

  #dates separated by spaces
  check_these <- grepl(" ", tmp, fixed = TRUE)
  tmp1 <- as.character(sapply(strsplit(tmp[check_these], " ", fixed = TRUE),
                              function(x) paste0(unique(x[nchar(x) >= 4]),
                                                 collapse = "-")))
  if (length(tmp1) > 0)
    tmp[check_these] <- tmp1

  return(tmp)
}
