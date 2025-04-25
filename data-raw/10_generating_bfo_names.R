#### Script to read BFO names and create shapefiles, edit and generate the data/worldMap.rds ####

# List of Plant Names from the BFO

## paths from plantRdata
orig.path <- here::here()
orig.file.data <- file.path(paste0(orig.path, "data"), "data",
                            "bfoNamesTracheophyta.rda")
orig.path.dim <- file.path(paste0(orig.path, "data"), "data-raw", "bfo")

## loading and editing info
load(orig.file.data)
bfoNames <-
  bfoNamesTracheophyta[bfoNamesTracheophyta$phylum %in% "Tracheophyta", ]
bfoNames$kingdom <- NULL
bfoNames$phylum <- NULL

## saving
save(bfoNames, file = "./data/bfoNames.rda", compress = "xz")

last_update <- readLines(file.path(orig.path.dim, "last_update.txt"))
path_to_save <- file.path(here::here(), "data-raw", #"raw_dictionaries",
                          "last_update_bfo.txt")
write(last_update, path_to_save)

dimensions <-
  paste0(dim(bfoNames)[1], " rows and ", dim(bfoNames)[2], " columns")
path_to_save <- file.path(here::here(), "data-raw", #"raw_dictionaries",
                          "df_dim_bfo.txt")
write(dimensions, path_to_save)
