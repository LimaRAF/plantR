## Creating some locality strings
str <- c("paraguay_paraguari",
         "brazil_parana_paranagua",
         "brazil_rio janeiro_parati", # an example of a variant in the locality name
         "brazil_rio janeiro_paraty",
         "brazil_sao paulo_sao miguel arcanjo_pe carlos botelho",
         "united states_florida", # valid location but not in the default gazetteer
         "brazil_brasil_sao paulo")

# Objects with the expected results
res0 <- data.frame(
  loc.correct = c("paraguay_paraguari", "brazil_parana_paranagua",
                  "brazil_rio janeiro", "brazil_rio janeiro_paraty",
                  "brazil_sao paulo_sao miguel arcanjo", "united states",
                  "brazil"),
  NAME_0 = c("Paraguay", "Brazil", "Brazil", "Brazil", "Brazil", "United States", "Brazil"),
  NAME_1 = c("Paraguarí", "Paraná", "Rio de Janeiro", "Rio de Janeiro", "São Paulo", NA, NA),
  NAME_2 = c(NA, "Paranaguá", NA, "Paraty", "São Miguel Arcanjo", NA, NA),
  NAME_3 = rep(NA_character_, 7),
  source = c("gdam", "ibge", "gdam", "ibge", "ibge", "gdam", "gdam"))

# Tests
test_that("getAdmin works", {
  expect_error(getAdmin(TRUE))
  expect_error(getAdmin(data.frame(character())))
  expect_error(getAdmin(data.frame(xxxx = c("Aa bb", "Bb cc"))))

  expect_error(getAdmin(str, gazet = "xxxx"))

  expect_equal(getAdmin(str), res0)
  expect_equal(getAdmin(str, gazet = plantR:::admin), res0)

})
