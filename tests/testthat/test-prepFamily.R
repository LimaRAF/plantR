# Creating a data frame with locality information
df <- data.frame(
  family = c("Ulmaceae", "Cannabaceae", "Salicaceae", "Flacourtiaceae",
             "Vivianiaceae", "", "Turneraceae", "Turneraceae",
             "Geraniaceae", "Vivianiaceae", "", "",
             "", "", "", "",
             "Goodeniaceae|Campanulaceae", "Orobanchaceae|Acanthaceae"),
  genus = c("Trema", "Trema", "Casearia", "Casearia",
            "Casearia", "", "Turnera", "",
            "Viviania", "Viviania", "", "",
            "", "Indet.", "", "Indet.",
            "Lobelia", "Gerardia"),
  scientificName = c("Trema micrantha", "Trema micrantha", "Casearia sylvestris", "Casearia sylvestris",
                     "Casearia sylvestris","Casearia sylvestris", "Turnera orientalis","Turnera uleana",
                     "Viviania albiflora", "Viviania albiflora", "Xylosma prockia", "Xyloma prockia",
                     "Xuxu zao", "sp.1", "Indeterminada1", "Indet. sp.",
                     "Lobelia", "Gerardia"))

# Objects with the expected resolutions
res0 <- c("Cannabaceae", "Cannabaceae", "Salicaceae", "Salicaceae",
          "Salicaceae", "Salicaceae", "Passifloraceae", "Passifloraceae",
          "Francoaceae", "Francoaceae", "Salicaceae", "Salicaceae",
          NA, NA, NA, NA,
          "Goodeniaceae|Campanulaceae", "Orobanchaceae|Acanthaceae")

# Tests
test_that("prepFamily works", {
  expect_error(prepFamily(df[[1]]))
  expect_error(prepFamily(data.frame()))
  expect_error(prepFamily(df[,2, drop = FALSE]))

  expect_equal(prepFamily(df, print = FALSE)$family.new, res0)
  expect_equal(prepFamily(df[,c(1,3)], print = FALSE)$family.new, res0)
  expect_equal(prepFamily(df[,c(1,3)], print = TRUE)$family.new, res0)
})
