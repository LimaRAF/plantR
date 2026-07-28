
## Creating some taxonomic information for preparation
df <- data.frame(
  numTombo = paste0(rep(c("R", "P"), each = 3), 1:3),
  family.new = "AA",
  scientificName.new = rep(c("Aa aa","Aa ab"), each = 3),
  recordedBy.new = "Glaziou, A.F.M.",
  last.name = "Glaziou",
  recordNumber.new = rep(c(11323, 11324), each = 3),
  year.new = 1876,
  municipality.new = c(rep("parati", 4), NA, NA),
  loc.correct = c(rep("brazil_rio janeiro_parati", 5), "brazil_rio janeiro")
)

res1.1 <- apply(df[,c(2,5,6,8)], 1, paste0, collapse = "_")
res1.1[5] <- res1.1[4]
res1 <- res1.1; res1[6] <- NA
res1.2 <- res1.1
res1.2[6] <- sub("_NA", "_rio janeiro", res1.2[6])

res2.1 <- apply(df[,c(2,7,6,8)], 1, paste0, collapse = "_")
res2.1[5] <- res2.1[4]
res2 <- res2.1; res2[6] <- NA
res2.2 <- res2.1
res2.2[6] <- sub("_NA", "_rio janeiro", res2.2[6])

res3.1 <- apply(df[,c(3,5,6,7)], 1, paste0, collapse = "_")
res3.1[5] <- res3.1[4]
res3 <- res3.1

res4.1 <- apply(df[,c(7,5,6,8)], 1, paste0, collapse = "_")
res4.1[5] <- res4.1[4];
res4 <- res4.1; res4[6] <- NA
res4.2 <- res4.1
res4.2[6] <- sub("_NA", "_rio janeiro", res4.2[6])



# Tests
test_that("prepDup works", {

  expect_error(prepDup(TRUE))
  expect_error(prepDup(data.frame(character())))
  expect_error(prepDup(data.frame(xxxx = c("Aa bb", "Bb cc"))))
  expect_error(prepDup(data.frame(numTombo = c("Aa bb", "Bb cc"))))

  result <- prepDup(df)
  expect_equal(result$dup.srch.str1, res1)
  expect_equal(result$dup.srch.str2, res2)
  expect_equal(result$dup.srch.str3, res3)
  expect_equal(result$dup.srch.str4, res4)

  result1 <- prepDup(df, ignore.miss = FALSE)
  expect_equal(result1$dup.srch.str1, res1.1)
  expect_equal(result1$dup.srch.str2, res2.1)
  expect_equal(result1$dup.srch.str3, res3.1)
  expect_equal(result1$dup.srch.str4, res4.1)

  result2 <- prepDup(df, ignore.miss = TRUE, loc.miss = TRUE)
  expect_equal(result2$dup.srch.str1, res1.2)
  expect_equal(result2$dup.srch.str2, res2.2)
  expect_equal(result2$dup.srch.str3, res3.1)
  expect_equal(result2$dup.srch.str4, res4.2)

})

