test_that("capName works", {
  expect_equal(capName("gentry"), "Gentry")
  expect_equal(capName("GENTRY"), "Gentry")
  expect_equal(capName("saint-hilaire"), "Saint-Hilaire")
  expect_equal(capName("saint-hilaire"), "Saint-Hilaire")
  expect_equal(capName("o'brien"), "O'Brien")
})
