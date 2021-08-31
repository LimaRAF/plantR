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

df1 <- checkCoord(df,
                 low.map = "plantR",
                 high.map = "plantR",
                 keep.cols = c("geo.check", country.shape, country.gazetteer))

checkBorders(df1,
                   geo.check = "geo.check",
                   country.shape = country.shape,
                   country.gazetteer = country.gazetteer,
                   output = output)

df1.1 <- df1
df1.1$border.check <- c(FALSE, FALSE)

test_that("checkBorders works", {
  expect_equal(checkBorders(df1,
                            geo.check = "geo.check",
                            country.shape = country.shape,
                            country.gazetteer = country.gazetteer,
                            output = output), df1.1)
})

## Same col
output <- "same.col"

df1.1 <- df1
df1.1$geo.check <- c("ok_county", "sea")

test_that("checkBorders works", {
  expect_equal(checkBorders(df1,
                            geo.check = "geo.check",
                            country.shape = country.shape,
                            country.gazetteer = country.gazetteer,
                            output = output), df1.1)
})
