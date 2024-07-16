


test_that("getTaxUnique works", {

  nomes <- c("Casearia sylvestris",
             "Casearia celtidifolia", "Casearia serrulata")
  orig.col <- "scientificName.new"
  df_input <- data.frame(scientificName.new = nomes)
  status.col = "taxon.status"
  type.match.col = "match_type"
  mult.match.col = "multiple_match"
  agg.cols = c("id", "family", "authorship", "taxon.rank", "name.status",
               "taxon.status", "accepted.name", "accepted.authorship",
               "accepted.taxon.rank", "accepted.name.status",
               status.col, type.match.col, mult.match.col)
  for (i in 1:length(agg.cols))
    df_input[[agg.cols[i]]] <- NA

  tmp.match.col <- "tmp.tax.name"
  df_input[[tmp.match.col]] <- cleanName(df_input[[orig.col]])

  df_ref <- bfoNames
  df_ref[[tmp.match.col]] <- cleanName(df_ref[["name"]])

  res_func <- getTaxUnique(df_input, df_ref,
                      match.col = tmp.match.col,
                      orig.col = orig.col,
                      status.col = status.col,
                      type.match.col = type.match.col,
                      mult.match.col = mult.match.col,
                      mult.matches = "all",
                      agg.cols = agg.cols)

  res0 <- c(NA, "Casearia ulmifolia", "Casearia decandra|Casearia sylvestris")
  res01 <- c("Sw.", "Kunth|Poepp. Eichler", "J. Seber ex Griseb.|Sw.")


  expect_length(res_func, 16)
  expect_equal(res_func[[orig.col]], nomes)
  expect_equal(res_func$accepted.name, res0)
  expect_equal(res_func$authorship, res01)


  res_func <- getTaxUnique(df_input, df_ref,
                           match.col = tmp.match.col,
                           orig.col = orig.col,
                           status.col = status.col,
                           type.match.col = type.match.col,
                           mult.match.col = mult.match.col,
                           mult.matches = "best",
                           agg.cols = agg.cols)

  res0 <- c(NA, "Casearia ulmifolia", "Casearia decandra")
  res01 <- c("Sw.", "Kunth", "J. Seber ex Griseb.")

  expect_equal(res_func$accepted.name, res0)
  expect_equal(res_func$authorship, res01)


})
