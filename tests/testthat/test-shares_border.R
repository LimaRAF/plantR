test_that("shares_border works", {
  expect_equal(shares_border("Colombia", "Brazil"), TRUE)
  expect_equal(shares_border("Colombia", "Argentina"), FALSE)
  expect_equal(shares_border("France", "germany"), TRUE)
})
