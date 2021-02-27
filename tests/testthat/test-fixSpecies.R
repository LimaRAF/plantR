test_that("fixSpecies works", {
  df <- data.frame(scientificName = c("Lindsaea lancea",
                                      "Lindsaea lancea (L.) Bedd.",
                                      "Lindsaea lancea var. angulata",
                                      "Lindsaea Aff. lancea",
                                      "Lindsaea", "Lindsaea sp.", "Lindsaeaceae sp.",
                                      "Lindsaea aff. lancea (L.) Bedd."))

  df_clean <- fixSpecies(df)
  expect_length(df_clean, length(df) + 2)
  expect_equal(setdiff(names(df), names(df_clean)), character(0))
  expect_equal(setdiff(names(df_clean), names(df)),
               c("scientificName.new", "scientificNameStatus"))
})

test_that("fixSpecies parameters work", {
  df <- data.frame(scientificName = c("Lindsaea lancea var. angulata",
                                      "Lindsaea Aff. lancea",
                                      "Lindsaea aff. lancea (L.) Bedd."))

  df_clean <- fixSpecies(df, rm.rank = T)
  expect_length(df_clean, length(df) + 2)
  expect_equal(setdiff(names(df), names(df_clean)), character(0))
  expect_equal(setdiff(names(df_clean), names(df)),
               c("scientificName.new", "scientificNameStatus"))
  expect_equal(df_clean$scientificName.new[1], "Lindsaea lancea angulata")
  expect_equal(df_clean$scientificName.new[2], "Lindsaea lancea")
  expect_equal(df_clean$scientificName.new[3], "Lindsaea lancea (L.) Bedd.")
})
