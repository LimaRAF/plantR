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
  expect_no_warning(checkColNames(df, group = "format.tax"))
  df <- data.frame(scientificName = c("A a", "A b"),
           geo.check = c("ok", "bad"))
  expect_warning(checkColNames(df))
  expect_no_warning(checkColNames(df, group = "format.tax"))

})


df <- data.frame(numTombo = c("a1","b2","c3","c3","d5","d5","e7","f4","g9",
                              "h7","i10"),
                  dup.ID = c("a1|b2","a1|b2","c3|c3","c3|c3","d5|d5|e7",
                             "d5|d5|e7","d5|d5|e7","f4",NA, "h7|i10", "h7|i10"),
                  dup.prop = c(1, 1, 1, 1, 0.5, 1, 1, 1, NA, 0.5, 1))
res <- c(rep(TRUE, 8), NA, FALSE, TRUE)

test_that("getMergeCat works", {

  expect_error(getMergeCat(logical()))
  expect_error(getMergeCat(df[,-2]))

  expect_equal(getMergeCat(df)$dup.merge, res)

})
