# Creating a data frame with locality information
sp.list <- c("Casearia sylvestris","Casearia silvestris",
             "Casearia sylvestris",
             "Casearia sylvestris var. angustifolia", "Casearia attenuata",
             "Casearia celtidifolia", "Casearia celtidifolia",
             "Casearia celtidifolia", "Casearia tropicana",
             "Casearia serrulata", "Casearia serrulata", "Casearia serrulata",
             "Oreodaphne porosa", "Oreodaphne porosa",
             "Ocotea porosa", "Ocotea porosa",
             "Ocotea sp.", "Chrysophylum sp.",
             "Ocotea sp.", "Chrysophylum sp.",
             "Chrysophyllum mexicanum",
             "Trichilia clausenii",
             "Tabebuia heterophylla",
             "Cola acuminata",
             "Xylosma ciliatifolium",
             "Cinnamomum camphora")
aut_list <- c("Sw.", "Sw.", "",
              "Uittien", "Rusby",
              "Kunth", "Poepp. Eichler","", "L.",
              "", "J. Seber ex Griseb.", "Sw.",
              "", "Nees & Mart.",
              "", "(Nees & Mart.) Barroso",
              "", "",
              "Aubl.", "L.",
              "", "", "", "",
              "(Clos) Eichler",
              "")

df <- data.frame(scientificName.new = sp.list,
  scientificNameAuthorship.new = aut_list)

# Objects with the expected resolutions
res1 <- c("Casearia sylvestris Sw.",
          "Casearia sylvestris Sw.",
          "Casearia sylvestris Sw.",
          "Casearia sylvestris Sw.",
          "Casearia sylvestris Sw.",
          "Casearia ulmifolia Vahl ex Vent.",
          "Casearia ulmifolia Vahl ex Vent.",
          "Casearia ulmifolia Vahl ex Vent.",
          "Casearia tropicana L.",
          "Casearia serrulata J. Seber ex Griseb.|Casearia serrulata Sw.",
          "Casearia decandra Jacq.",
          "Casearia sylvestris Sw.",
          "Ocotea porosa (Nees & Mart.) Barroso",
          "Ocotea porosa (Nees & Mart.) Barroso",
          "Ocotea porosa (Nees & Mart.) Barroso",
          "Ocotea porosa (Nees & Mart.) Barroso",
          "Ocotea Aubl.",
          "Chrysophyllum L.",
          "Ocotea Aubl.",
          "Chrysophyllum L.",
          "Chrysophyllum mexicanum",
          "Trichilia clausenii",
          "Tabebuia heterophylla",
          "Cola acuminata",
          "Xylosma ciliatifolia (Clos) Eichler",
          "Cinnamomum camphora (L.) J.Presl")
notes <- c("name accepted", "name misspelled", "name accepted",
           "replaced synonym", "replaced synonym", "replaced synonym",
           "replaced synonym", "+1 name, but 1 accepted", "not found",
           "check +1 name", "replaced synonym", "replaced synonym",
           "replaced synonym", "replaced synonym", "name accepted",
           "name accepted", "name accepted", "name misspelled",
           "name accepted", "name misspelled",
           "not found", "not found", "not found",
           "not found", "replaced variant",
           "+1 name, but 1 accepted")
match.type <- c("exact_w_author", "fuzzy_w_author", "exact_wout_author",
                "exact_w_author", "exact_w_author", "exact_w_author",
                "exact_w_author", "exact_wout_author", "no_match",
                "exact_wout_author", "exact_w_author", "exact_w_author",
                "exact_wout_author", "exact_w_author", "exact_wout_author",
                "exact_w_author", "exact_wout_author",
                "fuzzy_wout_author_wout_indet",
                "exact_w_author_wout_indet",
                "fuzzy_w_autor_wout_indet",
                "bad_fuzzy_wout_author", "bad_fuzzy_wout_author", "bad_fuzzy_wout_author",
                "no_match", "exact_w_author", "exact_wout_author")

res2 <- res1
res2[which(sp.list %in% "Casearia tropicana")] <- "Casearia tropicana"
# res2[which(sp.list %in% "Casearia serrulata" &
#               aut_list %in% "")] <- "Casearia decandra Jacq."
res2[which(sp.list %in% "Casearia serrulata" &
             aut_list %in% "Sw.")] <- "Casearia serrulata J. Seber ex Griseb.|Casearia serrulata Sw."
res2[which(sp.list %in% "Casearia serrulata" &
             aut_list %in% "J. Seber ex Griseb.")] <- "Casearia serrulata J. Seber ex Griseb.|Casearia serrulata Sw."
res3 <- res2
res3[which(sp.list %in% "Casearia tropicana")] <- "Casearia tropicana L."



# Tests
test_that("prepSpecies works", {
  run_test <- prepSpecies(df, drop = "")
  expect_equal(run_test$scientificNameFull, res1)
  expect_equal(run_test$match_type, match.type)
  expect_equal(run_test$tax.notes, notes)

  run_test <- prepSpecies(df[, 1, drop = FALSE], use.authors = FALSE)
  expect_equal(run_test$scientificNameFull, res2)

  run_test <- prepSpecies(df, use.authors = FALSE)
  expect_equal(run_test$scientificNameFull, res3)
})
