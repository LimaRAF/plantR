test_that("squish works", {
  expect_equal(squish(c(" A   a ")), c("A a"))
})


test_that("fixSpaces works", {
  expect_equal(fixSpaces(c("A\U0020 a", "Aa\U3000")), c("A  a", "Aa "))
})


test_that("paste1 works", {
  expect_equal(paste1(c("A", "a"), collapse = " "), c("A a"))
  expect_equal(paste1(c("A", "", NA,"a"), collapse = " "), c("A a"))
})

# test_that("readScript works", {
# })

test_that("checkColNames works", {

  df <- data.frame(scientificName = c("A a", "A b"),
                   scientificNameAuthorship  = c("C.D.", "C.C"))
  expect_no_warning(checkColNames(df))
  df <- data.frame(scientificName = c("A a", "A b"),
           geo.check = c("ok", "bad"))
  expect_warning(checkColNames(df))


})


