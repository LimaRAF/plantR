## Creating some locality information
df <- data.frame(resol.orig = c("municipality","locality"),
                 loc.string = c("brazil_rio janeiro_parati",
                                "brazil_rio janeiro_paraty",
                                "brazil_sao paolo_estrada limeira_mogi guacu",
                                "brazil_sao paolo",
                                "brazil_sao paulo_moxi guacu",
                                "brazil_sao paulo_estrada limeira_mogi guacu"),
                 loc.string1 = c(NA, "brazil_rio janeiro_paraty_parati mirim",
                                 NA, NA, NA, NA),
                 stringsAsFactors = FALSE)

# Objects with the expected results
df.out <- data.frame(loc = c("brazil_rio janeiro_parati",
                             "brazil_rio janeiro_paraty_parati mirim",
                             "brazil_sao paolo_estrada limeira_mogi guacu",
                             "brazil_sao paolo",
                             "brazil_sao paulo_moxi guacu",
                             "brazil_sao paulo_estrada limeira_mogi guacu"),
                     loc.correct = c("brazil_rio janeiro_paraty",
                                     "brazil_rio janeiro_paraty_parati mirim",
                                     "brazil", "brazil",
                                     "brazil_sao paulo", "brazil_sao paulo"),
                     latitude.gazetteer = c(-23.14821837, -23.24055556,
                                            -10.74257447, -10.74257447,
                                            -22.2584562, -22.2584562),
                     longitude.gazetteer = c(-44.70690993, -44.63416667,
                                             -53.08032013, -53.08032013,
                                             -48.7420254, -48.7420254),
                     resolution.gazetteer = c("county","locality",
                                              "country", "country",
                                              "state", "state"),
                     stringsAsFactors = FALSE)
res0 <- cbind.data.frame(df, df.out)

# Tests
test_that("getLoc works", {
  expect_equal(getLoc(df), res0)
  expect_equal(getLoc(df, orig.names = TRUE), res0)
})
