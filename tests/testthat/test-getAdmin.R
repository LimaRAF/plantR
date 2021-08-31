## Creating some locality strings
str <- c("paraguay_paraguari",
         "brazil_parana_paranagua",
         "brazil_rio janeiro_parati", # an example of a variant in the locality name
         "brazil_rio janeiro_paraty",
         "brazil_sao paulo_sao miguel arcanjo_pe carlos botelho",
         "united states_florida") # valid location but not in the default gazetteer

# Objects with the expected results
res0 <- data.frame(
  loc.correct = c("paraguay_paraguari", "brazil_parana_paranagua",
                  "brazil_rio janeiro", "brazil_rio janeiro_paraty",
                  "brazil_sao paulo_sao miguel arcanjo", "united states"),
  NAME_0 = c("Paraguay", "Brazil", "Brazil", "Brazil", "Brazil", "United States"),
  NAME_1 = c("Paraguarí", "Paraná", "Rio de Janeiro", "Rio de Janeiro", "São Paulo", NA),
  NAME_2 = c(NA, "Paranaguá", NA, "Paraty", "São Miguel Arcanjo", NA),
  NAME_3 = rep(NA_character_, 6),
  source = c("gdam", "ibge", "gdam", "ibge", "ibge", "gdam"))

# Tests
test_that("getAdmin works", {
  expect_equal(getAdmin(str), res0)
})
