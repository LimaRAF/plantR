test_that("ColNumber works", {
  numbers <- c("3467", "3467 ", " 3467",
               "ALCB3467", "Gentry 3467",
               "ALCB - 3467", "3467a", "3467A", "3467 A", "3467 - A",
               "PL671", "57 - 685", "685 - 4724", "1 - 80", "-4724",
               "(3467)", "(3467", "3467)", "32 - 3 - 77",
               "s / n.", "s.n.", "s.n", "", NA)
  clean <-  c("3467",  "3467",  "3467", "ALCB3467", "3467",
              "ALCB-3467",  "3467-A", "3467-A" , "3467-A", "3467-A",
              "PL671", "57-685", "685-4724", "1-80",  "4724",
              "(3467)" ,"3467",  "3467",  "32-3-77", NA,
              NA,  NA,  NA,  NA)
  df_num <- colNumber(numbers, noNumb = NA)
  expect_equal(df_num, clean)
  expect_equal(colNumber("347A"), "347-A")
  expect_equal(colNumber("COL347A", colCodes = "COL"), "347-A")
})
