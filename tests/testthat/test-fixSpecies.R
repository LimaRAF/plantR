test_that("fixSpecies works", {
  df <- data.frame(scientificName = c("Lindsaea lancea",
                                      "Lindsaea lancea (L.) Bedd.",
                                      "Lindsaea lancea var. angulata",
                                      "Lindsaea Aff. lancea",
                                      "Lindsaea",
                                      "Lindsaea sp.",
                                      "Lindsaeaceae sp.",
                                      "Lindsaea aff. lancea (L.) Bedd."),
                   scientificNameAuthorship = c("(L.) Bedd.", "", "",
                                                "(L.) Bedd.", "", "",
                                                "", ""))

  df_clean <- fixSpecies(df)
  expect_length(df_clean, length(df) + 3)
  expect_equal(setdiff(names(df), names(df_clean)), character(0))
  expect_equal(setdiff(names(df_clean), names(df)),
               c("scientificName.new", "scientificNameAuthorship.new",
                 "scientificNameStatus"))
})

test_that("fixSpecies parameters work", {
  df <- data.frame(scientificName = c("Lindsaea lancea var. angulata",
                                      "Lindsaea Aff. lancea",
                                      "Lindsaea aff. lancea (L.) Bedd."),
                   scientificNameAuthorship = c("", "(L.) Bedd.", ""))

  df_clean <- fixSpecies(df, rm.rank = TRUE)
  expect_length(df_clean, length(df) + 3)
  expect_equal(setdiff(names(df), names(df_clean)), character(0))
  expect_equal(setdiff(names(df_clean), names(df)),
               c("scientificName.new", "scientificNameAuthorship.new",
                 "scientificNameStatus"))
  expect_equal(df_clean$scientificName.new[1], "Lindsaea lancea angulata")
  expect_equal(df_clean$scientificName.new[2], "Lindsaea lancea")
  expect_equal(df_clean$scientificName.new[3], "Lindsaea lancea")

  expect_equal(df_clean$scientificNameAuthorship.new[1], NA_character_)
  expect_equal(df_clean$scientificNameAuthorship.new[2], "(L.) Bedd.")
  expect_equal(df_clean$scientificNameAuthorship.new[3], "(L.) Bedd.")

})
