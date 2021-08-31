## Creating some geographical coordinates
df <- data.frame(decimalLatitude = c(-23.5, -23.475389, NA),
                     decimalLongitude = c(-47.166667, -47.123768, NA),
                     latitude.gazetteer = c(-23.524052, -23.524052, -23.524052),
                     longitude.gazetteer = c(-47.122207, -47.122207, -47.122207),
                     resolution.gazetteer = c("county", "county", "county"),
                     decimalLatitude.new = c(-23.5, -23.475389, NA),
                     decimalLongitude.new = c(-47.166667,-47.123768, NA))

# Objects with the expected results
res0 <- df
res0[3, c(6:7)] <- res0[3, c(3:4)]
res0$origin.coord <- c(rep("coord_original", 2), "coord_gazet")
res0$precision.coord <- c("minutes", "miliseconds", "seconds_centroid")
res1 <- res0[, -c(3,4)]

# Tests
test_that("getCoord works", {
  expect_equal(getCoord(df), res0)
  expect_equal(getCoord(df, rm.gazet = TRUE), res1)
})
