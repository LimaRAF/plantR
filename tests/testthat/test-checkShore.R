## New col
lat1 <- c(-23.475389, -47.123768)
lon1 <- c(-47.123768, -23.475389)
df <- data.frame(decimalLongitude.new = lon1,
                 decimalLatitude.new = lat1,
                 loc.correct = rep("brazil_sao paulo_sao roque", 2),
                 origin.coord = rep("original", 2),
                 resolution.gazetteer = rep("county",2))

lat <- "decimalLatitude.new"
lon <- "decimalLongitude.new"
country.shape <- "NAME_0"
country.gazetteer <- "country.gazet"
tax.name <- "scientificName.new"
output <- "new.col"

brMap <- plantR::worldMap[plantR::worldMap$NAME_0 %in% "brazil", ]
spMap <- plantR::latamMap$brazil[plantR::latamMap$brazil$NAME_1 %in% "sao paulo", ]

df1 <- checkCoord(df,
                 low.map = brMap,
                 high.map = spMap,
                 keep.cols = c("geo.check", country.shape, country.gazetteer))

df2 <- checkBorders(df1,
                   geo.check = "geo.check",
                   country.shape = country.shape,
                   country.gazetteer = country.gazetteer,
                   output = output)

df2.1 <- df2
df2.1$shore.check <- c(NA, FALSE)

test_that("checkShore works", {
  expect_equal(checkShore(df2), df2.1)
})

## Same col
output <- "same.col"

df1 <- checkCoord(df,
                  low.map = brMap,
                  high.map = spMap,
                  keep.cols = c("geo.check", country.shape, country.gazetteer))

df2 <- checkBorders(df1,
                    geo.check = "geo.check",
                    country.shape = country.shape,
                    country.gazetteer = country.gazetteer,
                    output = output)

df2.1 <- df2
df2.1$geo.check <- c("ok_county", "open_sea")

test_that("checkShore works", {
  expect_equal(checkShore(df2, output = output), df2.1)
})
