sp.list = c(rep("Ocotea porosa", 8), rep("Araucaria angustifolia", 3),
            "Oreodaphne porosa", "Oreodaphne porosa", "Acalypha annobonae")

aut.list = c(rep("(Nees & Mart.) Barroso", 8),
             NA, NA, "(Bertol.) Kuntze", "Nees & Mart.", NA, "Pax & K.Hoffm.")

loc = c("brazil", "brazil_parana",
        "brazil_santa catarina_blumenau_parque são francisco",
        "paraguay", "japan_hokkaido", "paraguay_asuncion",
        "brazil_amazonas_manaus", NA, "brazil_espirito santo",
        "brazil_santa catarina_blumenau", "brazil_sao paulo",
        "chile_santiago", "brazil_parana_curitiba", "argentina_buenos aires")

df <- data.frame(scientificName = sp.list,
                 scientificNameAuthorship = aut.list,
                 loc.correct = loc)

my_source <- data.frame(tax.name = c("Ocotea porosa", 
                                     "Araucaria angustifolia", 
                                     "Oreodaphne porosa"),
                        tax.author = c("(Nees & Mart.) Barroso", 
                                       "(Bertol.) Kuntze", 
                                       "Nees & Mart."),
                        taxon.distribution = c("Brazil_Minas Gerais|Brazil|Brazil_Paraná",
                                               "Brazil_Santa Catarina_Blumenau|Paraguay_Asunción",
                                               NA))

# Objects with the expected resolution
res.bfo1 <- c("no.cannot.check",
              "ok.dist",
              "ok.dist",
              "no.cannot.check",
              "no.cannot.check",
              "no.cannot.check",
              "invalid.dist",
              NA,
              "invalid.dist",
              "ok.dist",
              "ok.dist",
              "no.cannot.check",
              "ok.dist",
              "no.cannot.check")

res.bfo2 <- c("full.name.match",
              "full.name.match",
              "full.name.match",
              "full.name.match",
              "full.name.match",
              "full.name.match",
              "full.name.match",
              "full.name.match",
              "canonical.name.match",
              "canonical.name.match",
              "full.name.match",
              "full.name.match",
              "canonical.name.match",
              "no.match")

res.wcvp1 <- c("invalid.dist",
               "ok.dist",
               "ok.dist",
               "ok.dist",
               "invalid.dist",
               "ok.dist",
               "invalid.dist",
               NA,
               "ok.dist",
               "ok.dist",
               "ok.dist",
               "invalid.dist",
               "ok.dist",
               "invalid.dist")

res.wcvp2 <- c("full.name.match",
               "full.name.match",
               "full.name.match",
               "full.name.match",
               "full.name.match",
               "full.name.match",
               "full.name.match",
               "full.name.match",
               "canonical.name.match",
               "canonical.name.match",
               "canonical.name.match",
               "full.name.match",
               "canonical.name.match",
               "full.name.match")

res.user1 <- c("ok.dist.coarser",
               "ok.dist.finer",
               "ok.dist.coarser",
               "invalid.dist",
               "invalid.dist",
               "invalid.dist",
               "ok.dist.coarser",
               NA,
               "ok.dist.coarser",
               "ok.dist.finer",
               "ok.dist.coarser",
               "no.cannot.check",
               "no.cannot.check",
               "no.cannot.check")

res.user2 <- c(rep("full.name.match", 8),
               rep("canonical.name.match", 2),
               rep("full.name.match", 2),
               "canonical.name.match",
               "no.match")


# Tests
test_that("checkDist works", {
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
