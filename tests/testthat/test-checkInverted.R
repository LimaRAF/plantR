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

df3 <- checkShore(df2, geo.check = "geo.check", output = output)

df3.1 <- df3
df3.1$geo.check.new <- c("ok_county", "ok_country[transposed]")
df3.1$border.check.new <- c(FALSE, NA)
df3.1$shore.check.new <- c(NA, NA)
df3.1$decimalLongitude.new.new <- -47.123768
df3.1$decimalLatitude.new.new <- -23.475389

test_that("checkInverted works", {
  expect_error(checkInverted(df))
  expect_equal(checkInverted(df3, world.map = brMap), df3.1)
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

df3 <- checkShore(df2, geo.check = "geo.check", output = output)

df3.1 <- df3
df3.1$geo.check <- c("ok_county", "ok_country[transposed]")
df3.1$decimalLongitude.new <- -47.123768
df3.1$decimalLatitude.new <- -23.475389

test_that("checkInverted works", {
  expect_error(checkInverted(TRUE))
  expect_error(checkInverted(data.frame(character())))
  expect_error(checkInverted(data.frame(xxxx = c("Aa bb", "Bb cc"))))

  expect_equal(checkInverted(df3, output = output, world.map = brMap),
               df3.1)
})
