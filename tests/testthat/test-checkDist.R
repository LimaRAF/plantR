sp.list = c(rep("Ocotea porosa", 8), rep("Araucaria angustifolia", 3),
            "Oreodaphne porosa", "Oreodaphne porosa")

aut.list = c(rep("(Nees & Mart.) Barroso", 8),
             NA, NA, "(Bertol.) Kuntze", "Nees & Mart.", NA)

loc = c("brazil", "brazil_parana",
        "brazil_santa catarina_blumenau_parque são francisco",
        "paraguay", "japan_hokkaido", "paraguay_asuncion",
        "brazil_amazonas_manaus", NA, "brazil_espirito santo",
        "brazil_santa catarina_blumenau", "brazil_sao paulo", 
        "chile_santiago", "brazil_parana_curitiba")
  
df <- data.frame(suggestedName = sp.list,
                 suggestedAuthorship = aut.list,
                 loc.correct = loc)

# Objects with the expected resolution
res.bfo1 <- c("no.cannot.check",
              "ok.dist",
              "ok.dist",
              "invalid.dist",
              "invalid.dist",
              "invalid.dist",
              "invalid.dist",
              NA,
              "invalid.dist",
              "ok.dist",
              "ok.dist",
              "invalid.dist",
              "ok.dist")

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
              "canonical.name.match")

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
               "ok.dist")

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
               "canonical.name.match")

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
  
    
    
})
