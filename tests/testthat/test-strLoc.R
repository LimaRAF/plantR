# Creating a data frame with locality information
df <- data.frame(country.new = c("brazil", "brazil", "brazil", "brazil"),
  stateProvince.new = c("rio de janeiro", "rio de janeiro", "rio de janeiro", NA),
  municipality.new = c("parati", "paraty", "paraty", NA),
  locality.new = c(NA,"paraty-mirim", NA, "paraty-mirim"),
  locality.scrap = c(NA, NA, "trindade", NA),
  resol.orig = c("county","locality","locality","country"))

# Expected result
res <- data.frame(loc.string = c("brazil_rio de janeiro_parati",
                                 "brazil_rio de janeiro_paraty",
                                 "brazil_rio de janeiro_paraty", "brazil"),
                  loc.string1 = c(NA, "brazil_rio de janeiro_paraty_paraty-mirim",
                                  NA, NA),
                  loc.string2 = c(NA, NA, "brazil_rio de janeiro_trindade",
                                  "brazil_NA_NA_paraty-mirim"))

# Tests
test_that("strLoc works", {
  expect_error(strLoc(df$country.new))
  expect_equal(strLoc(df)[,7:9], res)
})

