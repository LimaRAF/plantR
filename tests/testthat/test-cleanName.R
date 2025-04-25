test_that("cleanName works", {

  expect_equal(cleanName("Aa a-a 1989"), "aaaa")
})
