nomes <- c("Casearia sylvestris",
           "Casearia celtidifolia", "Casearia serrulata")
nomes1 <- c("Lobelia","Justicia","Mappia","Eupatorium",
           "Gerardia","Cunninghamia","Phyllostachys",
           "Struthanthus vulgaris")
orig.col <- "scientificName.new"
df_input <- data.frame(scientificName.new = nomes)
df_input1 <- data.frame(scientificName.new = nomes1)
status.col = "taxon.status"
type.match.col = "match_type"
mult.match.col = "multiple_match"
agg.cols = c("id", "family", "tax.authorship", "taxon.rank", "name.status",
             "taxon.status", "accepted.tax.name", "accepted.tax.authorship",
             "accepted.taxon.rank", "accepted.taxon.status",
             "accepted.name.status",
             status.col, type.match.col, mult.match.col)
for (i in 1:length(agg.cols)) {
  df_input[[agg.cols[i]]] <- NA
  df_input1[[agg.cols[i]]] <- NA
}

tmp.match.col <- "tmp.tax.name"
df_input[[tmp.match.col]] <- df_input[[orig.col]]
df_input1[[tmp.match.col]] <- df_input1[[orig.col]]

df_ref <- bfoNames
df_ref[[tmp.match.col]] <- df_ref[["tax.name"]]

test_that("getTaxUnique works", {

  expect_error(getTaxUnique())
  expect_error(getTaxUnique(data.frame(character())))
  expect_error(getTaxUnique(df_input))
  expect_error(getTaxUnique(df_input, data.frame(character())))
  expect_error(getTaxUnique(df_input, df_ref))

    res_func <- getTaxUnique(df_input, df_ref,
                      match.col = tmp.match.col,
                      orig.col = orig.col,
                      name.col = "tax.name",
                      status.col = status.col,
                      type.match.col = type.match.col,
                      mult.match.col = mult.match.col,
                      mult.matches = "all",
                      agg.cols = agg.cols)

  res0 <- c(NA, "Casearia ulmifolia", "Casearia decandra|Casearia sylvestris")
  res01 <- c("Sw.", "Kunth|Poepp. Eichler", "J. Seber ex Griseb.|Sw.")

  expect_length(res_func, 16)
  expect_equal(res_func[[orig.col]], nomes)
  expect_equal(res_func$accepted.tax.name, res0)
  expect_equal(res_func$tax.authorship, res01)


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

  expect_equal(res_func$accepted.tax.name, res0)
  expect_equal(res_func$tax.authorship, res01)


})
