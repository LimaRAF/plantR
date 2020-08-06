# Download SouthAmerica GADM data
library(tmap)
library(dplyr)
# country codes
library(countrycode)
# isto deveria ser assim mas um monte de ilhas no caribe (neerlandês, acho) ficam de fora
#am <- codelist_panel %>% filter(continent == "Americas") %>%
 # select("country.name.en","iso3c")
#fora <- c("United States", "Canada")
#countries <- setdiff(am$country.name.en, fora)
#iso3 <- am %>% filter(country.name.en %in% countries) %>% distinct()

iso3 <- countrycode::countrycode(
  c("Anguilla",
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


# function to download that is not getData from raster because we want sf files
# gotta hate trycatch https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r
# I did this for levels 0, 1, 2, 3 Level 4 does not exist
getGADM <- function(cod,
                    level = 4,
                    destfolder = "./data-raw/GADM/") {
  out <- tryCatch(
    {
      message(paste("Downloading", cod, level))
      file <- paste0(cod, "_", level, "_sf.rds")
      this <- paste0(destfolder, file)
      upper <- paste0(destfolder, cod, "_", level + 1:4, "_sf.rds")

      if (any(file.exists(upper))) {
        message("better resolution file already exists") } else {
      if (file.exists(this)) {
        message("file already exists")
      } else {
      url <- paste0("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_", file)
      download.file(url,
                    destfile = paste0(destfolder, file))
      message("downloading", paste(cod, level, "OK"))
      }
    }
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
      message("Deleting empty files")
      file.inf <- list.files(destfolder, full.names = T)
      empty <- file.inf[file.info(file.inf)[["size"]] == 0]
      unlink(empty)
    }
  )
  return(out)
}
# Afinal vai fazer download em niveis descrescentes e apagar arquivos vazios, e só vai fazer download de niveis menores se o nível maior não existir
cbind(rep(iso3, each = 5),
      rep(4:0, 50))
GADM <- purrr::walk2(.x = rep(iso3, each = 5),
                     .y = rep(4:0, 50),
                    ~getGADM(cod = .x,
                             level = .y))
# Brazil should be 2 for sure.
unlink("./data-raw/GADM/BRA_3_sf.rds")
getGADM(cod = "BRA", level = 2)
#for now there's no other reason to delete any other level but this should be done here bedore going to GADM_join
