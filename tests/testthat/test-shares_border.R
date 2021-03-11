test_that("shares_border works", {
  expect_equal(shares_border("Colombia", "Brazil"), TRUE)
  expect_equal(shares_border("Colombia", "Argentina"), FALSE)
  expect_equal(shares_border("France", "germany"), TRUE)

})

test_that("shares_border works with non-standard names", {
  #expect_equal(shares_border("Cote  ivoire", "Brazil"), FALSE)# won't work!
  expect_equal(shares_border("Cote d'ivoire", "Brazil"), FALSE)
  expect_equal(shares_border("Cote d'ivoire", "Burkina faso"), TRUE)
  expect_equal(shares_border("timor leste", "Burkina faso"), FALSE)
})
# we neeeeeeed to standardize this.
test_that("the french guiana problem", {
  expect_equal(shares_border("brazil", "french guiana"), TRUE)
})
