## Creating some geographical coordinates
lat <- c(-23.475389, -47.123768)
lon <- c(-47.123768, -23.475389)
df <- data.frame(decimalLongitude.new = lon,
		decimalLatitude.new = lat,
		loc.correct = rep("brazil_sao paulo_sao roque", 2),
    origin.coord = rep("original", 2))

# Objects with the expected results
df1 <- cbind.data.frame(df, geo.check = c("ok_county", "sea"))

# Tests
test_that("checkCoord works", {
  expect_equal(checkCoord(df), df1)
})
