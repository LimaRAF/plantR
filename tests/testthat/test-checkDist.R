sp.list = c(rep("Ocotea porosa", 8), rep("Araucaria angustifolia", 3),
            "Oreodaphne porosa", "Oreodaphne porosa", "Acalypha annobonae",
            "Justicia adhatoda", "Justicia adhatoda", "Abutilon rufinerve", "Mangifera indica",
            "Mangifera indica")

aut.list = c(rep("(Nees & Mart.) Barroso", 8),
             NA, NA, "(Bertol.) Kuntze", "Nees & Mart.", NA, "Pax & K.Hoffm.",
             "L.", "Mart. ex Nees", NA, "L.", "L.")

loc = c("brazil", "brazil_parana",
        "brazil_santa catarina_blumenau_parque são francisco",
        "paraguay", "japan_hokkaido", "paraguay_asuncion",
        "brazil_amazonas_manaus", NA, "brazil_espirito santo",
        "brazil_santa catarina_blumenau", "brazil_sao paulo",
        "chile_santiago", "brazil_parana_curitiba", "argentina_buenos aires",
        "india", "india", "brazil_parana", "brazil_santa catarina", "thailand")

df <- data.frame(scientificName = sp.list,
                 scientificNameAuthorship = aut.list,
                 loc.correct = loc)

my_source <- data.frame(tax.name = c("Ocotea porosa",
                                     "Araucaria angustifolia",
                                     "Oreodaphne porosa"),
                        tax.authorship = c("(Nees & Mart.) Barroso",
                                       "(Bertol.) Kuntze",
                                       "Nees & Mart."),
                        taxon.distribution = c("Brazil_Minas Gerais|Brazil|Brazil_Paraná",
                                               "Brazil_Santa Catarina_Blumenau|Paraguay_Asunción",
                                               NA))

# Objects with the expected resolution
res.bfo1 <- c("no_cannot_check",
              "ok_dist",
              "ok_dist",
              "no_cannot_check",
              "no_cannot_check",
              "no_cannot_check",
              "bad_dist",
              NA,
              "bad_dist",
              "ok_dist",
              "ok_dist",
              "no_cannot_check",
              "ok_dist",
              "no_cannot_check",
              "no_cannot_check",
              "no_cannot_check",
              "ok_dist",
              "introduced",
              "no_cannot_check")

res.bfo2 <- c("full_name_match",
              "full_name_match",
              "full_name_match",
              "full_name_match",
              "full_name_match",
              "full_name_match",
              "full_name_match",
              "full_name_match",
              "canonical_name_match",
              "canonical_name_match",
              "full_name_match",
              "full_name_match",
              "canonical_name_match",
              "no_match",
              "no_match",
              "no_match",
              "canonical_name_match",
              "full_name_match",
              "full_name_match")

res.wcvp1 <- c("bad_dist",
               "ok_dist",
               "ok_dist",
               "ok_dist",
               "bad_dist",
               "ok_dist",
               "bad_dist",
               NA,
               "ok_dist",
               "ok_dist",
               "ok_dist",
               "bad_dist",
               "ok_dist",
               "bad_dist",
               "ok_dist",
               "ok_dist",
               "ok_dist",
               "introduced",
               "ok_dist")

res.wcvp2 <- c("full_name_match",
               "full_name_match",
               "full_name_match",
               "full_name_match",
               "full_name_match",
               "full_name_match",
               "full_name_match",
               "full_name_match",
               "canonical_name_match",
               "canonical_name_match",
               "canonical_name_match",
               "full_name_match",
               "canonical_name_match",
               "full_name_match",
               "full_name_match",
               "full_name_match",
               "canonical_name_match",
               "full_name_match",
               "full_name_match")

res.user1 <- c("ok_dist_coarser",
               "ok_dist_finer",
               "ok_dist_coarser",
               "bad_dist",
               "bad_dist",
               "bad_dist",
               "ok_dist_coarser",
               NA,
               "ok_dist_coarser",
               "ok_dist_finer",
               "ok_dist_coarser",
               "no_cannot_check",
               "no_cannot_check",
               "no_cannot_check",
               "no_cannot_check",
               "no_cannot_check",
               "no_cannot_check",
               "no_cannot_check",
               "no_cannot_check")

res.user2 <- c(rep("full_name_match", 8),
               rep("canonical_name_match", 2),
               rep("full_name_match", 2),
               "canonical_name_match",
               "no_match",
               "no_match",
               "no_match",
               "no_match",
               "no_match",
               "no_match")


# Tests
test_that("checkDist works", {

  expect_error(checkDist("fbo"))
  expect_error(checkDist( data.frame(scientificName = character(),
                                     scientificNameAuthorship = character(),
                                     loc.correct = character())))
  expect_error(checkDist( data.frame(scientific.name = sp.list,
                                     scientificNameAuthorship = aut.list,
                                     loc.correct = loc)))
  expect_error(checkDist( data.frame(scientificName = sp.list,
                                     scientific.Name.Authorship = aut.list,
                                     loc.correct = loc)))
  expect_error(checkDist( data.frame(scientificName = sp.list,
                                     scientificNameAuthorship = aut.list,
                                     loc = loc)))

  expect_error(checkDist(df, source = ""))


  run_test <- checkDist(df)
  expect_length(run_test, length(df) + 2)
  expect_equal(setdiff(names(df), names(run_test)), character(0))
  expect_equal(run_test$dist.check, res.bfo1)
  expect_equal(run_test$dist.check.obs, res.bfo2)

  run_test <- checkDist(df, source = "wcvp")
  expect_length(run_test, length(df) + 2)
  expect_equal(setdiff(names(df), names(run_test)), character(0))
  expect_equal(run_test$dist.check, res.wcvp1)
  expect_equal(run_test$dist.check.obs, res.wcvp2)

  run_test <- checkDist(df, source = my_source)
  expect_length(run_test, length(df) + 2)
  expect_equal(setdiff(names(df), names(run_test)), character(0))
  expect_equal(run_test$dist.check, res.user1)
  expect_equal(run_test$dist.check.obs, res.user2)
})
