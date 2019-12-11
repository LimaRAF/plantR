#' @title Fixing and Formatting the Herbarium Fields
#'
#' @description Put the occurrence data frame in the right format for data processing and validation
#'
#' @param x a data frame
#' @param origin collection which the data comes from
#'
#' @return the data frame \code{x} in the proper format. It also returns the
#' required field names that are missing and those that were replaced or dropped.
#'
#' @details
#'
#' @author Lima, R.A.F.
#'
#' @importFrom countrycode countrycode
#'
#' @export fixField
#'
#' @examples
#'
#'
fixField = function(x, origin = NULL) {
  # check input:
  if (!class(x) == "data.frame") { stop("input object needs to be a data frame!") }

  # reading the necessary packages #DON'T
  #require(countrycode)

  # Getting the fields that are essential for the validation
  fields = read.csv("./dictionaries/field_names.csv", as.is = TRUE, na.string = c(""," ",NA), encoding = "UTF-8")

  # Checking if all essential fields are provided in x
  df.names = colnames(x)
  miss.essential = !fields$standard_name[fields$plantR_status %in% "input_required"] %in% df.names

  if(any(miss.essential)) {
    good.names = fields[fields$standard_name %in% fields$standard_name[fields$plantR_status %in% "input_required"][miss.essential],]
    if(is.null(origin)) {
      stop("please provide one of the following origins for your occurrence data: gbif, splink,	jabot, splink2gbif or jabot_old")
    } else {
      good.names = good.names[,c("standard_name", origin)]
      problem.names = df.names[df.names %in% good.names[,origin]]
      problem.order = match(df.names, good.names[,origin])
      problem.order = problem.order[!is.na(problem.order)]
      problem.names = problem.names[problem.order]
      replace.names = good.names[, "standard_name"]
      warning(paste("Require field(s) '", paste(problem.names, collapse=" "),"' was(were) replaced by '", paste(replace.names, collapse=" "),"'",sep="",collapse = ""))
      df.names[df.names %in% good.names[,origin]] = replace.names[problem.order]
    }
  }

  # To do: Include fuzzy grep in case all required field are not found!

  # Checking if all optional fields are provided in x
  miss.optional = !fields$standard_name[fields$plantR_status %in% "input_optional"] %in% df.names
  if(any(miss.optional)) {
    good.names = fields[fields$standard_name %in% fields$standard_name[fields$plantR_status %in% "input_optional"][miss.optional], ]
    if(is.null(origin)) {
      stop("please provide one of the following origins for your occurrence data: gbif, splink,	jabot, splink2gbif or jabot_old")
    } else {
      good.names = good.names[!is.na(good.names[,origin]),c("standard_name", origin)]
      problem.names = df.names[df.names %in% good.names[,origin]]
      if(length(problem.names) == 0 ) {
        comment(x) <- ("No substituion of names for optional fields!")
      } else  {
        problem.order = match(df.names, good.names[,origin])
        problem.order = problem.order[!is.na(problem.order)]
        replace.names = good.names[problem.order, "standard_name"]
        warning(paste("Optional field(s) '", paste(problem.names, collapse=" "),"' was(were) replaced by '", paste(replace.names, collapse=" "),"'",sep="",collapse = ""))
        df.names[df.names %in% good.names[,origin]] = replace.names
        other.names = good.names[-(problem.order), "standard_name"]
        if(length(other.names)>0) warning(paste("The following optional field(s) was(were) not found: \n", paste(other.names, collapse="\n"),sep="",collapse = ""))
      }
    }
  }

  ## Replacing and removing overlapping fiels
  #replacing  'countryCode' by 'country'
  if (sum(df.names %in% c("county","municipality"))==2) {
    code.dig = nchar(x$countryCode)
    x$country[is.na(x$country) & !is.na(x$countryCode) & code.dig %in% 2] =
      countrycode::countrycode(as.character(x$countryCode[is.na(x$country) & !is.na(x$countryCode) & code.dig %in% 2]), 'iso2c', 'country.name')
    x$country[is.na(x$country) & !is.na(x$countryCode) & code.dig %in% 3] =
      countrycode::countrycode(as.character(x$countryCode[is.na(x$country) & !is.na(x$countryCode) & code.dig %in% 2]), 'iso3c', 'country.name')
    x = x[,-which(names(x) == "countryCode")]
  }

  #Replacing/removing locality fields: 'county' by 'municipality'
  if (sum(df.names %in% c("county","municipality"))==2) {
    x$county[is.na(x$county) & !is.na(x$municipality)] = x$municipality[is.na(x$county) & !is.na(x$municipality)]
    x$municipality[!is.na(x$county) & is.na(x$municipality)] = x$county[!is.na(x$county) & is.na(x$municipality)]
    x = x[,-which(names(x) == "county")]
  }

  ## Filtering and dropping unnecessary columns
  include = df.names %in% fields$standard_name
  remove = df.names[!include]

  colnames(x) = df.names
  new.df = x[, include]
  if(length(remove)>0) warning(paste("The following field(s) was(were) removed: \n", paste(remove, collapse="\n"),sep="",collapse = ""))

  return(new.df)
}
