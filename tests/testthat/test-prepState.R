# Creating a vector with country information
estados <- c("RJ", "Rio de Janeiro", "MG", "estado de Minas Gerais",
             "Minas Gerais state", "state of Minas Gerais", "Minas Gerais",
             NA, "", "Minas Gerais", "MI", "South Caicos and East Caicos",
             "St. John", "São Paulo state")
paises <- c(rep("Brazil", 9), "", "Argentina", "Turks and Caicos Islands",
            "Jamaica", "Brazil")
df <- data.frame(country = paises, stateProvince = estados)

# Objects with the expected resolutions
res0 <- c("rio janeiro", "rio de janeiro", "minas gerais",
          "minas gerais", "minas gerais", "minas gerais",
          "minas gerais", NA, NA, "minas gerais",
          "misiones", "south caicos east caicos", "saint john",
          "sao paulo")
res1 <- c("Rio Janeiro", "Rio de Janeiro", "Minas Gerais",
          "Minas Gerais", "Minas Gerais", "Minas Gerais",
          "Minas Gerais", NA, NA, "Minas Gerais",
          "Misiones", "South Caicos East Caicos", "Saint John",
          "Sao Paulo")
res2 <- c("rio janeiro", "rio de janeiro", "minas gerais",
          "minas gerais", "minas gerais", "minas gerais",
          "minas gerais", NA, NA, "minas gerais",
          "misiones", "south caicos east caicos", "st. john",
          "sao paulo")
res3 <- c("rio janeiro", "rio de janeiro", "minas gerais",
          "minas gerais", "minas gerais", "minas gerais",
          "minas gerais", NA, NA, "minas gerais",
          "misiones", "south caicos east caicos", "saint john",
          "são paulo")
res4 <- c("Rio Janeiro", "Rio de Janeiro", "Minas Gerais",
          "Minas Gerais", "Minas Gerais", "Minas Gerais",
          "Minas Gerais", NA, NA, "Minas Gerais",
          "Misiones", "South Caicos East Caicos", "St. John",
          "São Paulo")


# Tests
test_that("prepState works", {
  expect_error(prepState(x = 1L))
  expect_error(prepState(x = data.frame(stateProvince = character(0))))
  expect_error(prepState(x = data.frame(state = "Minas Gerais")))

  expect_equal(prepState(x = data.frame(stateProvince = NA)), data.frame(stateProvince = NA))

  expect_equal(prepState(x = data.frame(stateProvince = "Minas Gerais")),
                 data.frame(stateProvince = "minas gerais", country = NA))

  expect_equal(prepState("Minas Gerais"), "minas gerais")

  expect_equal(prepState(df)$stateProvince, res0)
  expect_equal(prepState(df, to.lower = FALSE)$stateProvince, res1)
  expect_equal(prepState(df, rm.abbrev = FALSE)$stateProvince, res2)
  expect_equal(prepState(df, special.char = TRUE)$stateProvince, res3)
  expect_equal(prepState(df, to.lower = FALSE, rm.abbrev = FALSE, special.char = TRUE)$stateProvince,
               res4)

})
