## Creating some geographical coordinates in different formats
df <- data.frame(id = c("a_1","b_3","c_7","d_5","e_3",
                      "f_4","g_2","h_8","i_9","j_6","k_7","l_1"),
      str1 = c("a","b","c","l","l","p","p","p",NA,NA,"x","y"),
      str2 = c("d","d","e","k","k","o","o","o",NA,NA,"v","w"),
      str3 = c("f","g","f","n","n","s","r","s","t","t","z","u"),
      str4 = c("h","i","j","m","m","q","q","q",NA,NA,"ab","ac"))

# Objects with the expected results
df.out <- data.frame(dup.numb = c("2","1","1","4","4","4","3","4","1","1","0","0"),
                     dup.prop = c("0.5","0.25","0.25","1","1","1","0.75","1","0.25","0.25","0","0"),
                     dup.ID = c(rep("[a_1|b_3|c_7]", 3), rep("d_5|e_3", 2),
                                rep("f_4|g_2|h_8", 3), rep("i_9|j_6", 2),
                                NA, NA))
res0 <- cbind.data.frame(df, df.out)
res1 <- res0
res1$dup.ID <- gsub("\\[|\\]", "", res1$dup.ID, perl = TRUE)

# Tests
test_that("getDup works", {
  expect_equal(getDup(df), res0)
  expect_equal(getDup(df, flag.ind = FALSE), res1)
})
