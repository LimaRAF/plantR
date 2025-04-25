test_that("buildName works", {

  df <- data.frame(genus = c("Aa", "Ab", "Ac"),
                   specificEpithet = c("aa", "bb", NA))
  expect_equal(buildName(df), c("Aa aa","Ab bb", "Ac"))

  expect_error(buildName(c("Aa aa","Ab bb", "Ac")))

  df <- data.frame(jojo = c("Aa", "Ab", "Ac"),
                   toto = c("aa", "bb", NA))
  expect_error(buildName(df))


})
