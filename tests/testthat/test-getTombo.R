## Creating some collection and acession codes
colls <- c("P", "P", "NY", "G", "P", "P", "NY", "G",
          "P", "P", "NY", "G", NA, NA)
access <- c("1427", "P001427", "NY1427", "G-G-1427/1",
        "1427A", "1427 a", "1427-A", "U.1427", "P1427a", ""," ", NA, "1427", NA)

# Objects with the expected results
res0 <- c("P_1427", "P_1427", "NY_1427", "G_1427/1", "P_1427A", "P_1427a",
          "NY_1427A", "G_U.1427", "P_1427a", "P_NA", "NY_NA", "G_NA", "NA_1427",
          NA)
res1 <- c("p_1427", "p_1427", "ny_1427", "g_1427/1", "p_1427a", "p_1427a",
          "ny_1427a", "g_u.1427", "p_1427a", "p_NA", "ny_NA", "g_NA", "NA_1427",
          NA)
res2 <- c("P_1427", "P_1427", "NY_1427", "G_1427/1", "P_1427A", "P_1427a",
          "NY_1427A", "G_1427", "P_1427a", "P_NA", "NY_NA", "G_NA", "NA_1427",
          NA)

# Tests
test_that("getTombo works", {
  expect_equal(getTombo(colls, access), res0)
  expect_equal(getTombo(colls, access, to.lower = TRUE), res1)
  expect_equal(getTombo(colls, access, by.coll = FALSE), res2)
})
