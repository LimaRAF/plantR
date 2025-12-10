df <- data.frame(collectionCode = c("CRI","CRI","CRI","CRI"),
                  catalogNumber = c("3565","713","3073","15331"),
                  recordedBy = c("Rebelo, M.C.","Citadini-Zanette, V.",
                                 "Santos, R.","Zapelini, I."),
                  recordNumber = c("s.n.","1063","11","s.n."),
                  year = c("1994","1990","1994","2020"),
                  family = c("Salicaceae","Salicaceae","Cannabaceae","Cannabaceae"),
                  scientificName = c("Casearia sylvestris","Casearia sylvestris",
                                     "Trema micranthum","Trema micranthum"),
                  scientificNameAuthorship = c("Sw.", "Sw.", "(L.) Blume", "(L.) Blume"),
                  country = c("brazil","brazil","brazil","brazil"),
                  stateProvince = c("santa catarina","santa catarina",
                                    "santa catarina","santa catarina"),
                  municipality = c("jaguaruna","orleans","icara",NA),
                  tax.check1 = c("high", "high", "high", "low"),
                  geo.check1 = c("ok_county", "ok_county", "ok_county", "ok_state")
                   )


res1 <- c("BRAZIL, Santa Catarina, 2020, Zapelini, I., s.n. (CRI); BRAZIL, Santa Catarina: Içara, 1994, Santos, R., 11 (CRI)",
          "BRAZIL, Santa Catarina: Jaguaruna, 1994, Rebelo, M.C., s.n. (CRI); BRAZIL, Santa Catarina: Orleans, 1990, Citadini-Zanette, V., 1063 (CRI)")
res2 <- c("Santos, R., 11 (CRI, 3073); Zapelini, I., s.n. (CRI, 15331)",
         "Citadini-Zanette, V., 1063 (CRI, 713); Rebelo, M.C., s.n. (CRI, 3565)")


# Tests
test_that("checkList works", {
  expect_error(checkList(TRUE))
  expect_error(checkList(data.frame(character())))
  expect_error(checkList(data.frame(xxxx = c("Aa bb", "Bb cc"))))

  expect_equal(checkList(df, rm.dup = FALSE, type = "selected")$vouchers,
               res1)
  expect_equal(checkList(df, rm.dup = FALSE, type = "short")$vouchers,
               res2)
})
