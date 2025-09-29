# Creating a data frame with locality information
sp.list <- c("Casearia sylvestris",
             "Casearia silvestris",
             "Casearia sylvestris",
             "Casearia sylvestris var. angustifolia",
             "Casearia attenuata",
             "Casearia celtidifolia",
             "Casearia celtidifolia",
             "Casearia celtidifolia",
             "Casearia tropicana",
             "Casearia serrulata",
             "Casearia serrulata",
             "Casearia serrulata",
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
aut_list <- c("Sw.",
              "Sw.",
              "",
              "Uittien",
              "Rusby",
              "Kunth",
              "Poepp. Eichler",
              "",
              "L.",
              "",
              "J. Seber ex Griseb.",
              "Sw.",
              "", "Nees & Mart.",
              "", "(Nees & Mart.) Barroso",
              "", "",
              "Aubl.", "L.",
              "", "", "", "",
              "(Clos) Eichler",
              "")

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
          "Casearia decandra Jacq.|Casearia sylvestris Sw.",
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
notes <- c("name accepted",
           "name misspelled",
           "name accepted",
           "replaced synonym",
           "replaced synonym",
           "replaced synonym",
           "replaced synonym",
           "+1 name, but 1 accepted",
           "not found",
           "check +1 name|replaced synonym",
           "replaced synonym", "replaced synonym",
           "replaced synonym", "replaced synonym", "name accepted",
           "name accepted", "name accepted", "name misspelled",
           "name accepted", "name misspelled",
           "not found", "not found", "not found",
           "not found", "replaced orth. variant",
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
             aut_list %in% "Sw.")] <- "Casearia decandra Jacq.|Casearia sylvestris Sw."
res2[which(sp.list %in% "Casearia serrulata" &
             aut_list %in% "J. Seber ex Griseb.")] <- "Casearia decandra Jacq.|Casearia sylvestris Sw."
res3 <- res2
res3[which(sp.list %in% "Casearia tropicana")] <- "Casearia tropicana L."

# Tests
test_that("prepSpecies works", {

  expect_error(prepSpecies(x = 1))
  expect_error(prepSpecies(data.frame(scientificName = character())))

  res <- prepSpecies(x = "Casearia sylvestris", use.authors = FALSE)
  expect_equal(res$scientificNameFull, "Casearia sylvestris Sw.")


  run_test <- prepSpecies(df, drop = "taxon.distribution")
  expect_equal(run_test$scientificNameFull, res1)
  expect_equal(run_test$match_type, match.type)
  expect_equal(run_test$tax.notes, notes)

  run_test <- prepSpecies(df[, 1, drop = FALSE], use.authors = FALSE)
  expect_equal(run_test$scientificNameFull, res2)

  run_test <- prepSpecies(df, use.authors = FALSE)
  expect_equal(run_test$scientificNameFull, res3)

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

  res <- prepSpecies(df)
  expect_equal(res$suggestedName, c(df$scientificName[1:2], "Rhynchospora"))
  expect_equal(res$taxon.rank, c("species","species","genus"))
  expect_equal(res$tax.notes, c("author misspelled","author misspelled",
                                "name accepted"))


})

# sp.list <- c("Abarema alexandri var. alexandri",
#              "Abies balsamea var. balsamea",
#              "Abelmoschus cucurbitaceus",
#              "Abarema barbouriana (Standl.) Barneby & J.W.Grimes var. barbouriana",
#              "Abarema barbouriana barbouriana var. barbouriana")
# aut_list <- c("(Urb.) Barneby & J.W. Grimes",
#               "(L.) Mill.",
#               "Walp.",
#               "",
#               "(Standl.) Barneby & J.W.Grimes")
# df <- data.frame(scientificName = sp.list,
#                  scientificNameAuthorship = aut_list)
#
# res1 <- c("exact_wout_author", "fuzzy_w_author", "bad_fuzzy_w_author",
#           "no_match", "no_match")
# res2 <- c("replaced synonym", "name accepted", rep("not found", 3))
# data("gbifNamesPlantae", package = "plantRdata")
#
# df1 <- fixSpecies(df)
# res1.1 <- c("exact_wout_author", "fuzzy_w_author", "bad_fuzzy_w_author",
#           "exact_wout_author", "exact_wout_author")
# res2.1 <- c("replaced synonym", "name accepted", "not found",
#             "replaced synonym", "replaced synonym")
#
# test_that("prepSpecies works with gibifNames", {
#
#   run_test <- prepSpecies(df, db = gbifNamesPlantae, drop = "")
#   expect_equal(run_test$match_type, res1)
#   expect_equal(run_test$tax.notes, res2)
#
#   run_test1 <- prepSpecies(df1, db = gbifNamesPlantae, drop = "",
#                            tax.names = c("scientificName.new",
#                                          "scientificNameAuthorship.new"),
#                            replace.names = TRUE)
#   expect_equal(run_test1$match_type, res1.1)
#   expect_equal(run_test1$tax.notes, res2.1)
#
# })
