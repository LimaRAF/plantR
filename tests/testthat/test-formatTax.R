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
             "Cinnamomum camphora",
             "Lafoensia pacari subsp. petiolata var. hemisphaerica f. latifolia Koehne",
             "Lafoensia pacari subsp. petiolata var. hemisphaerica Koehne",
             "Lafoensia pacari subsp. petiolata Koehne")
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
              "", "", "", "")

df <- data.frame(scientificName = sp.list,
                 scientificNameAuthorship = aut_list)

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
          "Cinnamomum camphora (L.) J.Presl",
          "Lafoensia vandelliana Cham. & Schltdl.",
          "Lafoensia vandelliana Cham. & Schltdl.",
          "Lafoensia vandelliana Cham. & Schltdl.")
notes <- c("name accepted", "name misspelled", "name accepted",
           "replaced synonym", "replaced synonym", "replaced synonym",
           "replaced synonym", "+1 name, but 1 accepted", "not found",
           "check +1 name", "replaced synonym", "replaced synonym",
           "replaced synonym", "replaced synonym", "name accepted",
           "name accepted", "name accepted", "name misspelled",
           "name accepted", "name misspelled",
           "not found", "not found", "not found",
           "not found", "replaced orth. variant",
           "+1 name, but 1 accepted",
           "replaced synonym", "replaced synonym", "replaced synonym")
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
                "no_match", "exact_w_author", "exact_wout_author",
                "exact_w_author", "exact_w_author", "exact_w_author")
status <- c(rep("possibly_ok", 3), "variety", rep("possibly_ok", 12),
            rep("indet", 4), rep("possibly_ok", 6),
            "subspecies|variety|forma|name_w_authors",
            "subspecies|variety|name_w_authors",
            "subspecies|name_w_authors")

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

res4 <- c("Casearia sylvestris Sw.",
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
          "Cinnamomum camphora (L.) J.Presl",
          "Lafoensia vandelliana Cham. & Schltdl.",
          "Lafoensia vandelliana Cham. & Schltdl.",
          "Lafoensia vandelliana Cham. & Schltdl.")
change.i <- c(4:7, 11:16, 25)
for(i in change.i)
  res4[i] <- squish(paste(sp.list[i], aut_list[i]))

res4[8] <- "Casearia celtidifolia Kunth|Casearia celtidifolia Poepp. Eichler"
res4[10] <- "Casearia serrulata J. Seber ex Griseb.|Casearia serrulata Sw."
res4[13] <- "Oreodaphne porosa Nees & Mart."
res4[15] <- "Ocotea porosa (Nees & Mart.) Barroso"
res4[26] <- "Cinnamomum camphora (L.) Nees & Eberm.|Cinnamomum camphora (L.) J.Presl"
res4[27:29] <- sp.list[27:29]
res4[27:29] <- sp.list[27:29]

# Tests
test_that("formatTax works", {

  expect_error(formatTax(x = 1))
  expect_error(formatTax(data.frame(scientificName = character())))
  expect_error(formatTax("Casearia sylvestris"))

  expect_equal(formatTax(df[1,])$scientificNameFull, "Casearia sylvestris Sw.")

  run_test <- formatTax(df, drop = "taxon.distribution")
  expect_equal(run_test$scientificNameFull, res1)
  expect_equal(run_test$match_type, match.type)
  expect_equal(run_test$tax.notes, notes)
  expect_equal(run_test$scientificNameStatus, status)

  run_test <- formatTax(df[, 1, drop = FALSE], use.authors = FALSE)
  expect_equal(run_test$scientificNameFull, res2)

  run_test <- formatTax(df, use.authors = FALSE)
  expect_equal(run_test$scientificNameFull, res3)

  run_test <- formatTax(df, replace.names = FALSE, drop = "taxon.distribution")
  expect_equal(run_test$scientificNameFull, res4)


  df <- data.frame(
    scientificName = c(
      "Amaioua intermedia",
      "Sida glaziovii",
      "Rhynchospora Vahl"
    ),
    scientificNameAuthorship = c(
      "ex Schult. & Schult. f. & Mart.",
      "(Sims) Bureau & K.Schum.",
      NA
    )
  )

  res <- formatTax(df)
  expect_equal(res$suggestedName, c(df$scientificName[1:2], "Rhynchospora"))
  expect_equal(res$taxon.rank, c("species","species","genus"))
  expect_equal(res$tax.notes, c("author misspelled","author misspelled",
                                "name accepted"))

})
