# Creating a data frame with locality information
df <- data.frame(country = c("BR", "Brazil", "BR", "Brasil", "BRA", NA,
                             "Brazil", "Brazil", "Brasil", "Mexico"),
        stateProvince = c("RJ", "Rio de Janeiro", "MG", "estado de Minas Gerais",
                  "Minas Geraes", "Minas Gerais", NA, NA, "Bahia", "San luis potosi"),
        municipality = c("Parati", "Paraty", "Lavras", "lavras", NA, "Lavras",
                         NA, NA, NA, NA),
        locality = c(NA,"Paraty-Mirim", NA, "UFLA", "municipio de Lavras, campus UFLA", NA,
                     "Estado de Sao Paulo: Cunha",
                     "Sao Paulo State : Guaruja",
                     "Barreiras mun., Aeroporto de Barreiras.",
                     "Lanim, San Antonio municipio"))


# Objects with the expected resolutions
res0 <- c("municipality", "locality", "municipality", "locality", "locality", "municipality",
          rep("locality", 4))
res1 <- c("municipality", "locality", "municipality", "locality", "stateProvince", "municipality",
          "country", "country", "stateProvince", "stateProvince")
res2 <- c("stateProvince", "stateProvince", "stateProvince", "stateProvince", "stateProvince", "no_info",
          "country", "country", "stateProvince", "stateProvince")
res3 <- c("parati", "paraty", "lavras", "lavras", "lavras", "lavras",
          "cunha", "guaruja", "barreiras", "san antonio")
res4 <- c("rio janeiro", "rio de janeiro", "minas gerais", "minas gerais", "minas gerais", "minas gerais",
          "sao paulo", "sao paulo", "bahia", "san luis potosi")

res.country <- c(rep("brazil", dim(df)[1]-1), "mexico")

# Tests
test_that("fixLoc works", {
  run_test <- fixLoc(df)
  expect_equal(run_test$country.new, res.country)
  expect_equal(run_test$resol.orig, res0)
  expect_equal(fixLoc(df, scrap = FALSE)$resol.orig, res1)
  expect_equal(fixLoc(df, loc.levels = c("country", "stateProvince"))$resol.orig, res2)
  expect_equal(run_test$municipality.new, res3)
  expect_equal(run_test$stateProvince.new, res4)
})

