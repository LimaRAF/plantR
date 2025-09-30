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

expected.resolution <- c("county", "locality", "county", "county", "county", "county",
                        "county", "county", "county", "county")

# Tests
test_that("formatLoc works", {
  run_test <- formatLoc(df)
  expect_equal(run_test$country.new, res.country)
  expect_equal(run_test$resol.orig, res0)
  expect_equal(fixLoc(df, scrap = FALSE)$resol.orig, res1)
  expect_equal(fixLoc(df, loc.levels = c("country", "stateProvince"))$resol.orig, res2)
  expect_equal(run_test$municipality.new, res3)
  expect_equal(run_test$stateProvince.new, res4)
  expect_equal(run_test$resolution.gazetteer, expected.resolution)
})

df <- data.frame(
  country = NA,
  stateProvince = c("Minas Gerais", "Minas Gerais state"),
  municipality = NA,
  locality = NA)

expected.state <- c("minas gerais", "minas gerais")
expected.resolution <- c("no_info", "no_info")

test_that("formatLoc with no country", {
  run_test <- formatLoc(df)
  expect_equal(run_test$stateProvince.new, expected.state)
  expect_equal(run_test$resolution.gazetteer, expected.resolution)
})

df <- data.frame(
  country = "BR",
  stateProvince = c(NA, NA, NA, NA, "estado: São Paulo"),
  municipality = NA,
  locality = c("Estado do Rio de Janeiro: Paraty",
               "Brasil – Estado de São Paulo",
               "State of Sao Paulo. Municipio de Sao Paulo: Parque do Estado",
               "Mun. de Cunha - prov. de São Paulo",
               "Municipio de Sao Paulo: Parque do Estado"))

expected.state <- c("rio de janeiro", "sao paulo", "sao paulo", "sao paulo", "sao paulo")
expected.resolution <- c("county", "state", "locality", "county", "locality")


test_that("formatLoc extracts state/mun. info from locality", {
  run_test <- formatLoc(df)
  expect_equal(run_test$country.new, rep("brazil", dim(df)[1]))
  expect_equal(run_test$stateProvince.new, expected.state)
  expect_equal(run_test$resolution.gazetteer, expected.resolution)
})

df$municipality <- df$locality
df$locality <- NA

test_that("formatLoc extracts state/mun/loc info from municipality", {
  run_test <- formatLoc(df)
  expect_equal(run_test$country.new, rep("brazil", dim(df)[1]))
  expect_equal(run_test$stateProvince.new, expected.state)
  expect_equal(run_test$resolution.gazetteer, expected.resolution)
})
