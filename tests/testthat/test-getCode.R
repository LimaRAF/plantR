# df <- data.frame(institutionCode = c("ASU", "UNEMAT", "MOBOT", "NYBG"),
# collectionCode = c("ASU-PLANTS", "NX-FANEROGAMAS", "MO", "NY"))
df <- data.frame(institutionCode = c("ASU", "UNEMAT", "MOBOT", "NYBG", "AUA", "HAST", NA,
                                     "Observation",
                                     NA, NA, "", NA, ""),
                 collectionCode = c("ASU-PLANTS", "NX-FANEROGAMAS", "MO", "NY", "", "HAST-100012", "MO",
                                    NA,
                                    "Xuxu", NA, NA, "", ""))

res <- c("ASU", "NX", "MO", "NY", "AUA", "HAST","MO", "Observation", "Xuxu", NA, NA, NA, NA)
res1 <- c(rep(NA, 6), "code partially found", rep("code not found", 2), rep("cannot check", 4))

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

df <- data.frame(institutionCode = c(NA, NA, "",
                                    "HERB", "Botany", "NA", ## false NAs that are actually the code NA from United States National Arboretum
                                    "NY", "NY", "HERB", "CCM", "NO DISPONIBLE", "NA", NA, ""),
                 collectionCode = c("RB", "AA", "AA",
                                    "NA", "NA", "NA",  ## false NAs that are actually the code NA from United States National Arboretum
                                    NA, "", "-", "NO DISPONIBLE", "NO DISPONIBLE", "unknown", NA, ""))
res2 <- c(rep("code partially found", 5), rep(NA, 3),
          rep("code not found", 4), rep("cannot check", 2))

test_that("getCode parameter works", {
  df_code <- getCode(df)
  expect_equal(df_code$collectionObs, res2)
})
