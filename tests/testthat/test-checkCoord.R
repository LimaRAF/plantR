## Creating some geographical coordinates
lat <- c(-23.475389, -47.123768,
         -10.74257, -22.18913, -23.14822, -23.30001, NA, NA, NA)
lon <- c(-47.123768, -23.475389,
         -53.08032, -42.65292, -44.70691, -44.65605, NA, NA, NA)
df <- data.frame(decimalLongitude.new = lon,
		decimalLatitude.new = lat,
		loc.correct = c(rep("brazil_sao paulo_sao roque", 2),
		                "brazil", "brazil_rio janeiro",
		                "brazil_rio janeiro_paraty", "brazil_rio janeiro_paraty_parati mirim",
		                NA, NA, "brazil_rio janeiro_paraty"),
    origin.coord = rep("coord_original", 9))

# Objects with the expected results
df1 <- cbind.data.frame(df,
                        geo.check = c("ok_county", "sea",
                                      "ok_country", "ok_state", "ok_county", "ok_locality",
                                      "no_cannot_check", "no_cannot_check", "no_cannot_check"))

brMap <- plantR::worldMap[plantR::worldMap$NAME_0 %in% "brazil", ]
spMap <- plantR::latamMap$brazil[plantR::latamMap$brazil$NAME_1 %in%
                                   c("sao paulo","rio janeiro"), ]

# Tests
test_that("checkCoord works", {
  expect_error(checkCoord(1))
  expect_error(checkCoord(data.frame()))
  expect_error(checkCoord(df[,1:2]))

  res <- checkCoord(df, low.map = brMap, high.map = spMap)
  expect_equal(res, df1)
})
