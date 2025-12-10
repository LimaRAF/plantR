df <- data.frame(institutionCode = c("ASU", "UNEMAT", "MOBOT", "NYBG", NA,
                                     "Observation",
                                     NA, NA, "", NA, ""),
                 collectionCode = c("ASU-PLANTS", "NX-FANEROGAMAS", "MO", "NY", "MO",
                                    NA,
                                    "Xuxu", NA, NA, "", ""))
res <- c("ASU", "NX", "MO", "NY", "MO", "Observation", "Xuxu", NA, NA, NA, NA)
res1 <- c(rep(NA, 5), rep("code not found", 6))

test_that("getCode works", {
  expect_error(getCode())
  expect_error(getCode(data.frame()))
  expect_error(getCode(data.frame(institutionCode = NA)))
  expect_error(getCode(data.frame(collectionCode = NA)))

  df_code <- getCode(df)
  expect_equal(setdiff(names(df), names(df_code)), character(0))
  expect_equal(setdiff(names(df_code), names(df)), c("collectionCode.new", "collectionObs"))
  expect_equal(nrow(df_code), nrow(df))
  expect_equal(df_code$collectionCode.new, res)
  expect_equal(df_code$collectionObs, res1)

})

test_that("getCode parameter works", {
  df <- data.frame(institutionCodeA = c("ASU", "UNEMAT", "MOBOT", "NYBG"),
                   collectionCodeB = c("ASU-PLANTS", "NX-FANEROGAMAS", "MO", "NY"),
                   stringsAsFactors = FALSE)
  df_code <- getCode(df,
                     inst.code = "institutionCodeA",
                     col.code = "collectionCodeB",
                     drop = c("ordem.colecao","collectioncode.gbif",
                              "institutioncode.gbif",
                              "collection.string"))
  expect_equal(setdiff(names(df), names(df_code)), character(0))
  #check that all column names are created
  expect_true(all(setdiff(names(df_code), names(df)) %in%
                    c("collectionCode.new", "collectionObs", "organization")))
  expect_equal(nrow(df_code), nrow(df))
})
