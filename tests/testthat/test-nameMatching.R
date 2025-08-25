test_that("nameMatching works", {

  names <- c("Ailton", "Highlander", "Hell", "Hallo", "Agnes-Lee")
  refs <- c("Agnes Lee", "Hell", "Hellinger", "Hello", "Hill", "Hilton")

  expect_equal(nameMatching(names, refs),
               c(6, 3, 2, 4, 1))
  expect_equal(nameMatching(names, refs, match.type = c("exact")),
               c(NA, NA, 2, NA, NA))
  expect_equal(nameMatching(names, refs, match.type = c("exact"), clean.names = TRUE),
               c(NA, NA, 2, NA, 1))
  expect_equal(nameMatching(names, refs, max.dist = 0.15),
               c(6, NA, 2, 4, 1))
  expect_equal(nameMatching(names, refs, split = TRUE),
               c(NA, 3, 2, 4, 1))
  expect_equal(nameMatching(names, refs, parallel = TRUE),
               c(6, 3, 2, 4, 1))
  # expect_equal(nameMatching(names, refs, parallel = TRUE, clean.names = TRUE),
  #              c(6, 3, 2, 4, 1))
  expect_equal(nameMatching(names, refs, split = TRUE, parallel = FALSE),
               c(NA, 3, 2, 4, 1))

})
