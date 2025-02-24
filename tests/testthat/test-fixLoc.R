# Creating a data frame with locality information
df <- data.frame(country = c("BR", "Brazil", "BR", "Brasil", "BRA", NA),
        stateProvince = c("RJ", "Rio de Janeiro", "MG", "estado de Minas Gerais",
                  "Minas Geraes", "Minas Gerais"),
        municipality = c("Parati", "Paraty", "Lavras", "lavras", NA, "Lavras"),
        locality = c(NA,"Paraty-Mirim", NA, "UFLA", "municipio de Lavras, campus UFLA", NA))

# Objects with the expected resolutions
res0 <- c("municipality", "locality", "municipality", "locality", "locality", "municipality")
res1 <- c("municipality", "locality", "municipality", "locality", "stateProvince", "municipality")
res2 <- c("stateProvince", "stateProvince", "stateProvince", "stateProvince", "stateProvince", "no_info")
res3 <- c("parati", "paraty", "lavras", "lavras", "lavras", "lavras")
res4 <- rep("brazil", 6)

# Tests
test_that("fixLoc works", {
  run_test <- fixLoc(df)
  expect_equal(run_test$resol.orig, res0)
  expect_equal(fixLoc(df, scrap = FALSE)$resol.orig, res1)
  expect_equal(fixLoc(df, loc.levels = c("country", "stateProvince"))$resol.orig, res2)
  expect_equal(run_test$municipality.new, res3)
  expect_equal(run_test$country.new, res4)
})
