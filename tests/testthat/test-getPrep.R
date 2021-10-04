## Creating a vector of names
names <- c("ter Braak, Hans", "Braak, Hans ter", "Hans ter Braak",
           "Braak, Hanster", "Hanster Braak",
           "Silva, Maria A. Pereirada", "Maria A. Pereirada Silva",
           "Silva, Maria A. Pereira da", "da Silva, Maria A. Pereira",
           "Maria A. Pereira da Silva", "da Silva", "Silva")

# Objects with the expected results
res0 <- matrix(c("Silva", "Maria", "da"),
               nrow = 1, ncol = 3,
               dimnames = list(NULL,
                               c("last.name", "first.names", "prep")))
res1 <- matrix(c(rep("Braak", 5), rep("Silva", 7),
                 rep("Hans", 3), rep("Hanster", 2),  rep("Maria A. Pereirada", 2),
                 rep("Maria A. Pereira", 3), rep("", 2), rep("ter", 3),
                 rep("", 4), rep("da", 4), ""),
               nrow = 12, ncol = 3,
               dimnames = list(NULL,
                               c("last.name", "first.names", "prep")))
res2 <- res1[, 1:2]
res3 <- c(rep("Braak, Hans ter", 3), rep("Braak, Hanster", 2),
          rep("Silva, Maria A. Pereirada", 2),
          rep("Silva, Maria A. Pereira da", 3),
          "Silva, da", "Silva")
res4 <- c(rep("ter Braak, Hans", 3), rep("Braak, Hanster", 2),
          rep("Silva, Maria A. Pereirada", 2),
          rep("da Silva, Maria A. Pereira", 3),
          "da Silva", "Silva")
res5 <- c(rep("Hans ter Braak", 3), rep("Hanster Braak", 2),
          rep("Maria A. Pereirada Silva", 2),
          rep("Maria A. Pereira da Silva", 3),
          "da Silva", "Silva")

# Tests
test_that("getPrep works", {
  expect_equal(getPrep("Maria da Silva"), res0)
  expect_equal(getPrep("MARIA DA SILVA", output = "vector", lower = FALSE),
               "SILVA, MARIA DA")
  expect_equal(getPrep("MARIA DA SILVA", output = "vector", lower = FALSE, rm.prep = TRUE),
               "SILVA, MARIA")
  expect_equal(getPrep("MARIA DA SILVA", output = "vector", lower = TRUE, rm.prep = FALSE),
               "SILVA, MARIA da")
  expect_equal(getPrep(names), res1)
  expect_equal(getPrep(names, rm.prep = TRUE), res2)
  expect_equal(getPrep(names, output = "vector"), res3)
  expect_equal(getPrep(names, output = "vector", format = "prep_last_init"),
               res4)
  expect_equal(getPrep(names, output = "vector", format = "init_last"),
               res5)
})
