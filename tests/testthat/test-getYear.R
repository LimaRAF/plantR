## Creating some dates in different formats
dates <- c("25/03/1981","25-03-1981","03/1981","03-1981","1981","1.981","1,981",
         "25/III/1981","25-III-1981","III/1981","III-1981","25031981","25 03 1981","'81",
         "March 1981","Mar. 1981","Mar 1981","25 Mar 1981","n.d.","s.d.","s/d","",NA,
         "1981-03-25", "81/01/25", "25/03/81", "21/12/21")

# Objects with the expected years
res0 <- c(rep("1981", 13), "'81", rep("1981", 4), rep("s.d.", 5),
          rep("1981", 1), rep("81", 2), "21/12/21")
res1 <- gsub("s.d.", "n.d.", res0)

# Tests
test_that("getYear works", {
  expect_equal(getYear(dates), res0)
  expect_equal(getYear(dates, noYear = "n.d."), res1)
})
