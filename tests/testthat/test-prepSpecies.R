# Creating a data frame with locality information
sp.list <- c("Casearia sylvestris","Casearia sylvestirs",
  "Casearia sylvestris angustifolia", "Casearia attenuata",
  "Casearia celtidifolia", "Casearia celtidifolia","Casearia tropicana")
aut_list <- c("Sw.", "Sw.",
              "Uittien", "Rusby",
              "Kunth", "Poepp. Eichler","L.")

df <- data.frame(scientificName.new = sp.list,
  scientificNameAuthorship.new = aut_list)

# Objects with the expected resolutions
res0 <- c("Casearia sylvestris Sw.", "Casearia sylvestris Sw.", NA,
          "Casearia sylvestris Sw.", "Casearia ulmifolia Vahl ex Vent.",
          "Casearia sylvestris Sw.", "Casearia tropicana")
res1 <- c("Casearia sylvestris Sw.", "Casearia sylvestris Sw.",
          "Casearia sylvestris Sw.", "Casearia sylvestris Sw.",
          "Casearia ulmifolia Vahl ex Vent.",
          "Casearia ulmifolia Vahl ex Vent.", NA)

# Tests
test_that("prepSpecies works", {
  expect_equal(prepSpecies(df, sug.dist = 0.93)$scientificNameFull, res1)
  expect_equal(prepSpecies(df[, 1, drop = FALSE], use.authors = FALSE,
                           sug.dist = 0.93)$scientificNameFull, res1)
  expect_equal(prepSpecies(df, use.authors = FALSE, sug.dist = 0.93)$scientificNameFull, res1)
})
