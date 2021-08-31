## Creating some occurrence information
df <- data.frame(
  occurrenceRemarks = c("tree, 10 m", "Frutos Roxos. Cultivada", NA, "Tree"),
  locality = c("pastagem cultivada", NA, "Itatiaia, cultivada perto da sede", "Brazil"),
  habitat = c(NA, "Floresta", "Mata", "Cultivated"), stringsAsFactors = FALSE)

# Objects with the expected results
df.out <- data.frame(cult.check = c(NA,"prob_cultivated","prob_cultivated","cultivated"))
res0 <- cbind.data.frame(df, df.out)

# Tests
test_that("getCult works", {
  expect_equal(getCult(df), res0)
})
