test_that("buildName works", {

  df <- data.frame(genus = c("Aa", "Ab", "Ac"),
                   specificEpithet = c("aa", "bb", NA))
  expect_equal(buildName(df), c("Aa aa","Ab bb", "Ac"))
})
