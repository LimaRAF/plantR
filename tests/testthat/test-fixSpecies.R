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


test_that("test that fixSpecies works", {

  expect_error(fixSpecies(TRUE))
  expect_error(fixSpecies(data.frame(character())))
  expect_error(fixSpecies(data.frame(xuxu = c("Aa bb", "Bb cc"))))

  names <- c("Blechnumcf. spannagelii", "Blechnumcf.spannagelii", "Blechnum cf.spannagelii",
             "Blechnumaff. spannagelii", "Blechnumaff.spannagelii", "Blechnum aff.spannagelii")
  df <- data.frame(scientificName = names,
                   scientificNameAuthorship = rep("", length(names)))
  df_clean <- fixSpecies(df)

  expect_equal(df_clean$scientificName.new,
               rep(c("Blechnum spannagelii", "Blechnum spannagelii"), each = 3))

  names <- c("Blechnum sp.", "Blechnum spannageliivar. speciosa")
  df <- data.frame(scientificName = names,
                   scientificNameAuthorship = rep("", length(names)))
  df_clean <- fixSpecies(df, rm.rank = FALSE, rm.indet = TRUE)
  expect_equal(df_clean$scientificName.new,
               c("Blechnum", "Blechnum spannagelii var. speciosa"))

  df_clean <- fixSpecies(df, rm.rank = TRUE, rm.indet = TRUE)
  expect_equal(df_clean$scientificName.new,
               c("Blechnum", "Blechnum spannagelii speciosa"))

  df_clean <- fixSpecies(df, rm.rank = TRUE, rm.indet = FALSE)
  expect_equal(df_clean$scientificName.new,
               c("Blechnum sp.", "Blechnum spannagelii speciosa"))

})
