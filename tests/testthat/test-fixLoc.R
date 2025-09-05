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

df <- data.frame(
  country = c(NA, NA, rep("Brazil", 6), "Brasil", "Brazil"),
  stateProvince = c("Minas Gerais", "Minas Gerais state", "state of Minas Gerais",
                    NA, NA, NA, NA, NA, NA,
                    "estado: São Paulo"),
  municipality = NA,
  locality = c(NA, NA, NA,
               "Estado do Rio de Janeiro: Paraty",
               "Brasil – Estado de São Paulo: Paranapiacaba, Estação Biológica.",
               "State of Sao Paulo. Municipio de Sao Palo: Parque do Estado",
               "São Paulo ad urbem Santos in prov. S.Pauli",
               "Mun. de Cunha - prov. de São Paulo",
               "Municipio de Cunha na provincia de São Paulo",
               "Municipio de Sao Palo: Parque do Estado"))

res1 <- c("minas gerais", "minas gerais", "minas gerais", "rio de janeiro",
          "sao paulo", "sao paulo", NA, "sao paulo", "sao paulo", "sao paulo")


test_that("fixLoc works", {
  run_test <- fixLoc(df)
  expect_equal(run_test$country.new, rep("brazil", dim(df)[1]))
  expect_equal(run_test$stateProvince.new, res1)
})
