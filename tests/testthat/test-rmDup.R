# Creating a data frame with duplicates
df <- data.frame(numTombo = c("a1", "b2", "c3", "c3", "d5", "d5", "e7", "f4", "g9"),
                  dup.ID = c("a1|b2", "a1|b2", "c3|c3", "c3|c3", "d5|d5|e7",
                             "d5|d5|e7", "d5|d5|e7", "f4", NA),
                  dup.prop = c(1, 1, 1, 1, 0.5, 0.5, 1, 1, NA),
                  stringsAsFactors = FALSE)

# Objects with the expected resolutions
res0 <- df[-c(4,6),]
row.names(res0) <- 1:7
res1 <- df[-c(2,4,5,6),]
row.names(res1) <- 1:5

# Tests
test_that("rmDup works", {
  expect_equal(rmDup(df, print.rm = FALSE), res0)
  expect_equal(rmDup(df, rm.all = TRUE, print.rm = FALSE), res1)
})

