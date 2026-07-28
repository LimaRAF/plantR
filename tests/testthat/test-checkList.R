df <- data.frame(collectionCode = c("CRI","CRI","CRI","CRI","ESA"),
                  catalogNumber = c("3565","713","3073","15331","331"),
                  recordedBy = c("Rebelo, M.C.","Citadini-Zanette, V.",
                                 "Santos, R.","Zapelini, I.","Zapelini, I."),
                  recordNumber = c("s.n.","1063","11","s.n.","s.n."),
                  year = c("1994","1990","1994","2020","2021"),
                  family = c("Salicaceae","Salicaceae","Cannabaceae","Cannabaceae","Cannabaceae"),
                  scientificName = c("Casearia sylvestris","Casearia sylvestris",
                                     "Trema micranthum","Trema micranthum","Trema micranthum"),
                  scientificNameAuthorship = c("Sw.", "Sw.", "(L.) Blume", "(L.) Blume", NA),
                  country = c("brazil","brazil","brazil","brazil","brazil"),
                  stateProvince = c("santa catarina","santa catarina",
                                    "santa catarina","santa catarina", "santa catarina"),
                  municipality = c("jaguaruna","orleans","icara",NA, NA),
                  tax.check1 = c("high", "high", "high", "low", "low"),
                  geo.check1 = c("ok_county", "ok_county", "ok_county", "ok_state", "ok_state")
                   )

res1 <- c("BRAZIL, Santa Catarina, 2020, I. Zapelini s.n. (CRI 15331); BRAZIL, Santa Catarina: Içara, 1994, R. Santos 11 (CRI 3073)",
          "BRAZIL, Santa Catarina, 2021, I. Zapelini s.n. (ESA 331)",
          "BRAZIL, Santa Catarina: Jaguaruna, 1994, M.C. Rebelo s.n. (CRI 3565); BRAZIL, Santa Catarina: Orleans, 1990, V. Citadini-Zanette 1063 (CRI 713)")
res2 <- c("I. Zapelini s.n. (CRI 15331); R. Santos 11 (CRI 3073)",
          "I. Zapelini s.n. (ESA 331)",
         "M.C. Rebelo s.n. (CRI 3565); V. Citadini-Zanette 1063 (CRI 713)")


# Tests
test_that("checkList works", {
  expect_error(checkList(TRUE))
  expect_error(checkList(data.frame(character())))
  expect_error(checkList(data.frame(xxxx = c("Aa bb", "Bb cc"))))

  res <- checkList(df, rm.dup = FALSE, type = "selected")
  expect_equal(res$family, c("Cannabaceae", "Cannabaceae", "Salicaceae"))
  expect_equal(res$scientificName,
               c("Trema micranthum", "Trema micranthum", "Casearia sylvestris"))
  expect_equal(res$scientificNameAuthorship,
               c("(L.) Blume", NA, "Sw."))
  expect_equal(res$vouchers, res1)

  res <- checkList(df, rm.dup = FALSE, type = "short")
  expect_equal(res$vouchers, res2)
})
