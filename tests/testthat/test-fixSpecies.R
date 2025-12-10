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

  expect_error(fixSpecies(x = 5))

  df_clean <- fixSpecies(df)
  expect_length(df_clean, length(df) + 3)
  expect_equal(setdiff(names(df), names(df_clean)), character(0))
  expect_equal(setdiff(names(df_clean), names(df)),
               c("scientificName.new", "scientificNameAuthorship.new",
                 "scientificNameStatus"))

  expect_equal(df_clean$scientificNameStatus,
               c("possibly_ok", "name_w_authors", "variety", "affinis",
                 "indet", "indet", "family_as_genus", "affinis|name_w_authors"))

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



  expect_error(fixSpecies(TRUE))
  expect_error(fixSpecies(data.frame(character())))
  expect_error(fixSpecies(data.frame(xxxx = c("Aa bb", "Bb cc"))))

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

  df <- data.frame(scientificName = c("Euthales fructipendula", "Euthales peruviana",
                                     "Rosales"),
                scientificNameAuthorship = c("(Ruiz & Pav.) Steud.", "F.Dietr.",
                                   "Bercht. & J.Presl"))
  df_clean <- fixSpecies(df, rm.indet = TRUE)
  expect_equal(df_clean$scientificName.new,
               df$scientificName)
  expect_equal(df_clean$scientificNameStatus,
               c("possibly_ok", "possibly_ok", "order_as_genus"))

  x <- c("Lindsaea Lancea", "Urbanodendron Mez", "Rosideae sp.")
  res <- fixSpecies(x)
  expect_equal(res$scientificName.new,
               c("Lindsaea lancea", "Urbanodendron", "Rosideae sp."))
  expect_equal(res$scientificNameStatus,
               c("name_w_wrong_case", "name_w_authors", "(sub)family_as_genus"))

  df <- data.frame(scientificName = c("Lindsaea lancea var. angulata",
                                      "Lindsaea lancea subsp. angulata",
                                      "Lindsaea lancea f. angulata"))
  res <- fixSpecies(df, rm.rank = TRUE)
  expect_equal(res$scientificName.new,
               rep("Lindsaea lancea angulata", 3))

  x <- c("Lafoensia pacari subsp. petiolata var. hemisphaerica f. latifolia Koehne",
         "Lafoensia pacari subsp. petiolata var. hemisphaerica Koehne",
         "Lafoensia pacari subsp. petiolata var. hemisphaerica",
         "Lafoensia pacari subsp. petiolata",
         "Lafoensia pacari",
         "Lafoensia",
         "Lafoensia Vand.")
  res1 <- c("Lafoensia pacari subsp. petiolata var. hemisphaerica f. latifolia",
            "Lafoensia pacari subsp. petiolata var. hemisphaerica",
            "Lafoensia pacari subsp. petiolata var. hemisphaerica",
            "Lafoensia pacari subsp. petiolata",
            "Lafoensia pacari",
            "Lafoensia sp.",
            "Lafoensia")
  res2 <- c("Lafoensia pacari petiolata hemisphaerica latifolia",
            "Lafoensia pacari petiolata hemisphaerica",
            "Lafoensia pacari petiolata hemisphaerica",
            "Lafoensia pacari petiolata",
            "Lafoensia pacari",
            "Lafoensia sp.",
            "Lafoensia")
  res3 <- c("subforma|name_w_authors",
            "subvariety|name_w_authors",
            "subvariety",
            "subspecies",
            "possibly_ok",
            "indet",
            "name_w_authors")

  df <- data.frame(scientificName = x)
  res <- fixSpecies(df, rm.rank = FALSE)
  expect_equal(res$scientificName.new,
               res1)
  expect_equal(res$scientificNameStatus,
               res3)

  res <- fixSpecies(df, rm.rank = TRUE)
  expect_equal(res$scientificName.new,
               res2)


  # x <- c("Lindsaea lancea var. angulata",
  #        "Lindsaea lancea subsp. angulata",
  #        "Lindsaea lancea f. angulata",
  #        "Lindsaea lancea")
  # expect_equal(fixSpecies(toupper(x))$scientificNameStatus,
  #              c("variety|name_w_wrong_case", "subspecies|name_w_wrong_case",
  #                "form|name_w_wrong_case", "name_w_wrong_case"))


})
