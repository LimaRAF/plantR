# Download SouthAmerica GADM data

# country codes
{
iso3 <- countrycode::countrycode(
  c("Anguilla",
    "Antigua and Barbuda",
    "Argentina",
    "Aruba",
    "Bahamas",
    "Barbados",
    "Belize",
    "Bermuda",
    "Bolivia",
    "Brazil",
    "British Virgin Islands",
    "Caribbean Netherlands",
    "Cayman Islands",
    "Chile",
    "Colombia",
    "Costa Rica",
    "Cuba",
    "Curaçao",
    "Dominica",
    "Dominican Republic",
    "Ecuador",
    "El Salvador",
    "Falkland Islands",
    "French Guiana",
    "Grenada",
    "Guadeloupe",
    "Guatemala",
    "Guyana",
    "Haiti",
    "Honduras",
    "Jamaica",
    "Martinique",
    "Mexico",
    "Montserrat",
    "Nicaragua",
    "Panama",
    "Paraguay",
    "Peru",
    "Puerto Rico",
    "Saint Barthélemy",
    "Saint Kitts and Nevis",
    "Saint Lucia",
    "Saint Martin (French part)",
    "Saint Vincent and the Grenadines",
    "Sint Maarten",
    "Suriname",
    "Trinidad and Tobago",
    "Turks and Caicos Islands",
    "United States Virgin Islands",
    "Uruguay",
    "Venezuela"
  ),
  "country.name",
  "iso3c"
)
}

# function to download that is not getData from raster because we want sf files
# gotta hate trycatch https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r
# I did this for levels 0, 1, 2, 3 Level 4 does not exist
getGADM <- function(cod, level = 4) {
  out <- tryCatch(
    {
      # Just to highlight: if you want to use more than one
      # R expression in the "try" part then you'll have to
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression
      # in case the "try" part was completed successfully

      message(paste("Downloading", cod, level))
      file <- paste0(cod, "_", level, "_sf.rds")
      url <- paste0("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_", file)
      download.file(url, destfile = paste0("./data-raw/GADM/", file))
      print(paste(cod, level, "OK"))
      # You don't need to state the return value via `return()` as code
      # in the "try" part is not wrapped insided a function (unlike that
      # for the condition handlers for warnings and error below)
    },
    error = function(e) {
      message(paste("URL does not seem to exist:", url))
      message("Here's the original error message:")
      message(e)
      # Choose a return value in case of error

    },
    warning = function(w) {
      message(paste("URL caused a warning:", url))
      message("Here's the original warning message:")
      message(w)
      # Choose a return value in case of warning

    },
    finally = {
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>'
      #message(paste("Processed URL:", url))
      message("Next")
    }
  )
  return(out)
}

GADM <- purrr::map(iso3, ~getGADM(.))

