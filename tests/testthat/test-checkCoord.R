## Creating some geographical coordinates
lat <- c(-23.475389, -47.123768)
lon <- c(-47.123768, -23.475389)
df <- data.frame(decimalLongitude.new = lon,
		decimalLatitude.new = lat,
		loc.correct = rep("brazil_sao paulo_sao roque", 2),
    origin.coord = rep("original", 2))

# Objects with the expected results
df1 <- cbind.data.frame(df, geo.check = c("ok_county", "sea"))

brMap <- plantR::worldMap[plantR::worldMap$NAME_0 %in% "brazil", ]
spMap <- plantR::latamMap$brazil[plantR::latamMap$brazil$NAME_1 %in% "sao paulo", ]

# Tests
test_that("checkCoord works", {
  expect_error(checkCoord(df[,1:2]))
  expect_equal(checkCoord(df, low.map = brMap, high.map = spMap),
               df1)
})
