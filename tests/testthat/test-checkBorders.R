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

df1 <- plantR::checkCoord(df,
                 low.map = brMap,
                 high.map = spMap,
                 keep.cols = c("geo.check", country.shape, country.gazetteer))

# checkBorders(df1,
#                    geo.check = "geo.check",
#                    country.shape = country.shape,
#                    country.gazetteer = country.gazetteer,
#                    output = output)

df1.1 <- df1
df1.1$border.check <- c(FALSE, FALSE)

test_that("checkBorders works", {
  expect_error(checkBorders(TRUE))
  expect_error(checkBorders(data.frame(character())))
  expect_error(checkBorders(data.frame(xxxx = c("Aa bb", "Bb cc"))))

  expect_error(checkBorders(df1[,1:2]))
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

df2 <- df1
df2$NAME_0[2] <- "argentina"
df2$geo.check[2] <- "bad_country"
output <- "new.col"

test_that("checkBorders works", {
  res <- checkBorders(df2[2,],
               geo.check = "geo.check",
               country.shape = country.shape,
               country.gazetteer = country.gazetteer,
               output = output)
  expect_equal(res$border.check, TRUE)
})


test_that("shares_border works", {
  expect_error(plantR:::shares_border("xuxu", "xoxo"))

  expect_equal(plantR:::shares_border("brazil", "argentina"), TRUE)
  expect_equal(plantR:::shares_border("brazil", "mexico"), FALSE)
})

