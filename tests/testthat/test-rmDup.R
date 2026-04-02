# Creating a data frame with duplicates
df <- data.frame(numTombo = c("a1", "b2", "c3", "c3", "d5", "d5", "e7", "f4", "g9",
                              "", NA, ""),
                  dup.ID = c("a1|b2", "a1|b2", "c3|c3", "c3|c3", "d5|d5|e7",
                             "d5|d5|e7", "d5|d5|e7", "f4", NA, NA, NA, ""),
                  dup.prop = c(1, 1, 1, 1, 0.5, 0.5, 1, 1, NA, NA, NA, NA))

# Objects with the expected resolutions
res0 <- df[-c(4,6),]
row.names(res0) <- 1:10
res1 <- df[-c(2,4,5,6),]
row.names(res1) <- 1:8

# Tests
test_that("rmDup works", {

  expect_error(rmDup(df = "", print.rm = FALSE))
  expect_error(rmDup(df = data.frame(character()), print.rm = FALSE))
  expect_error(rmDup(df[,-2], print.rm = FALSE))
  # expect_warning(rmDup(df[,-3], print.rm = FALSE),
  #                "Classification requires a column with the proportion of duplicates. Assuming to be 1")
  expect_warning(rmDup(df[,-1], rm.all = TRUE, print.rm = FALSE))
  expect_error(rmDup(df[,-1], rm.all = FALSE, print.rm = FALSE))

  expect_equal(rmDup(df, print.rm = FALSE), res0)
  expect_equal(rmDup(df, rm.all = TRUE, print.rm = FALSE), res1)
})

