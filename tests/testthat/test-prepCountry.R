# Creating a vector with country information
paises <- c("VC", "VCT", "St. Vincent and the Grenadines",
            "St. Vincent & Grenadines", "Saint-Martin", "Falkland Is.", NA,
            "Colômbia")

# Objects with the expected resolutions
res0 <- c("saint vincent grenadines", "saint vincent grenadines",
          "saint vincent grenadines", "saint vincent grenadines",
          "saint martin", "falkland islands", NA, "colombia")
res1 <- stringr::str_to_title(res0)
res2 <- c("st. vincent grenadines", "st. vincent grenadines",
          "st. vincent grenadines", "st. vincent grenadines",
          "saint martin", "falkland is.", NA, "colombia")
res3 <- c("saint vincent grenadines", "saint vincent grenadines",
          "saint vincent grenadines", "saint vincent grenadines",
          "saint martin", "falkland islands", NA, "colômbia")

# Tests
test_that("prepCountry works", {
  expect_equal(prepCountry(paises), res0)
  expect_equal(prepCountry(paises, to.lower = FALSE), res1)
  expect_equal(prepCountry(paises, rm.abbrev = FALSE), res2)
  expect_equal(prepCountry(paises, special.char = TRUE), res3)
})
