## Creating some geographical coordinates in different formats
coords <- data.frame(
  decimalLatitude = c(-23.475389, "-23,475389","23o 28' 31.40\"S",
                      -93.475389, NA, 0, "missing", "blocked", NA,
                      "23° 28.5233'S","283088.52 E","-23,475,389", NA),
  decimalLongitude = c(-47.123768, "-47,123768", "47o 07' 25.56\"W",
                       -47.123768, 185.578, 0, "missing","blocked", NA,
                       "47° 07.4260'W","7402251.30 S","-47,123,768", NA),
  verbatimLatitude = c(rep(NA, 12), -23.475389),
  verbatimLongitude = c(rep(NA, 12), -47.123768),
  stringsAsFactors = FALSE)

# Objects with the expected results
res0 <- coords
res0$decimalLatitude.new <-
  c(rep(-23.475389, 3), rep(NA, 6), -23.475383, NA, -23.475389, -23.475389)
res0$decimalLongitude.new <-
  c(rep(-47.123768, 2), -47.12375, rep(NA, 6), -47.1237667, NA, -47.123768,
    -47.123768)
res0$coord.check <- c(TRUE, rep(FALSE, 7), TRUE, rep(FALSE, 3), TRUE)

# Tests
test_that("prepCoord works", {
  expect_equal(prepCoord(coords), res0)
  expect_equal(prepCoord(coords, flag = FALSE), res0[,c(1:6)])
})
