# Creating a data frame with locality information
df <- data.frame(
  family = c("Ulmaceae", "Cannabaceae", "Salicaceae", "Flacourtiaceae", "Vivianiaceae", ""),
  genus = c("Trema", "Trema", "Casearia", "Casearia", "Casearia", ""),
  scientificName = c("Trema micrantha", "Trema micrantha", "Casearia sylvestris",
                  "Casearia sylvestris","Casearia sylvestris","Casearia sylvestris"),
  stringsAsFactors = FALSE)

# Objects with the expected resolutions
res0 <- c("Cannabaceae", "Cannabaceae", "Salicaceae", "Salicaceae",
          "Salicaceae", "Salicaceae")

# Tests
test_that("prepFamily works", {
  expect_equal(prepFamily(df, print = FALSE)$family.new, res0)
  expect_equal(prepFamily(df, print = TRUE)$family.new, res0)
  expect_equal(prepFamily(df[,c(1,3)], print = FALSE)$family.new, res0)
})
