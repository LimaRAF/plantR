# Creating a data frame with locality information
sp.list <- c("Casearia sylvestris","Casearia sylvestirs",
  "Casearia sylvestris var. angustifolia", "Casearia attenuata",
  "Casearia celtidifolia", "Casearia celtidifolia","Casearia tropicana",
  "Oreodaphne porosa", "Oreodaphne porosa",
  "Ocotea porosa", "Ocotea porosa",
  "Ocotea sp.", "Chrysophylum sp.",
  "Chrysophyllum mexicanum")
aut_list <- c("Sw.", "Sw.",
              "Uittien", "Rusby",
              "Kunth", "Poepp. Eichler","L.",
              "", "Nees & Mart.",
              "", "(Nees & Mart.) Barroso",
              "", "", "")

df <- data.frame(scientificName.new = sp.list,
  scientificNameAuthorship.new = aut_list)

# Objects with the expected resolutions
res1 <- c("Casearia sylvestris Sw.", "Casearia sylvestris Sw.",
          "Casearia sylvestris Sw.", "Casearia sylvestris Sw.",
          "Casearia ulmifolia Vahl ex Vent.",
          "Casearia ulmifolia Vahl ex Vent.", NA,
          "Ocotea porosa (Nees & Mart.) Barroso",
          "Ocotea porosa (Nees & Mart.) Barroso",
          "Ocotea porosa (Nees & Mart.) Barroso",
          "Ocotea porosa (Nees & Mart.) Barroso",
          "Ocotea Aubl.",
          "Chrysophyllum L.",
          "Chrysophyllum mexicanum")
res3 <- res2 <- res1
res2[which(sp.list %in% "Casearia tropicana")] <- "Casearia tropicana"
res3[which(sp.list %in% "Casearia tropicana")] <- "Casearia tropicana L."

# Tests
test_that("prepSpecies works", {
  expect_equal(prepSpecies(df)$scientificNameFull,
               res1)
  expect_equal(prepSpecies(df[, 1, drop = FALSE], use.authors = FALSE)$scientificNameFull,
               res2)
  expect_equal(prepSpecies(df, use.authors = FALSE)$scientificNameFull,
               res3)
})
