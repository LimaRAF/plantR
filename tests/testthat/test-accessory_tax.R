test_that("rmOpen works", {
  expect_equal(rmOpen("Lindsaea cf. lancea"), "Lindsaea lancea")
  expect_equal(rmOpen("Lindsaea cf lancea"), "Lindsaea lancea")
  expect_equal(rmOpen("Lindsaea aff. lancea"), "Lindsaea lancea")
  expect_equal(rmOpen("Lindsaea aff lancea"), "Lindsaea lancea")
  expect_equal(rmOpen("cf. Lindsaea lancea"), "Lindsaea lancea")
  expect_equal(rmOpen("aff. Lindsaea lancea"), "Lindsaea lancea")
})

test_that("rmInfra works", {
  expect_equal(rmInfra(NA), NA)
  expect_equal(rmInfra(""), "")
  expect_equal(rmInfra("Lindsaea"), "Lindsaea")
  expect_equal(rmInfra("Lindsaea"), "Lindsaea")
  expect_equal(rmInfra("Lindsaea lancea"), "Lindsaea lancea")
  expect_equal(rmInfra("Lindsaea lancea var. angulata"), "Lindsaea lancea angulata")
  expect_equal(rmInfra("Lindsaea lancea var angulata"), "Lindsaea lancea angulata")
  expect_equal(rmInfra("Lindsaea lancea subsp. angulata"), "Lindsaea lancea angulata")
  expect_equal(rmInfra("Lindsaea lancea subsp angulata"), "Lindsaea lancea angulata")
  expect_equal(rmInfra("Lindsaea lancea f. angulata"), "Lindsaea lancea angulata")
  expect_equal(rmInfra("Lindsaea lancea fo. angulata"), "Lindsaea lancea angulata")
  expect_equal(rmInfra("Lindsaea lancea form. angulata"), "Lindsaea lancea angulata")
  expect_equal(rmInfra("Lindsaea lancea forma angulata"), "Lindsaea lancea angulata")
  expect_equal(rmInfra("Lindsaea lancea f angulata"), "Lindsaea lancea angulata")
  expect_equal(rmInfra("Lindsaea lancea forma angulata"), "Lindsaea lancea angulata")
  expect_equal(rmInfra("Lindsaea lancea (L.) Bedd. var. angulata Rosenst."),
               "Lindsaea lancea angulata Rosenst.")
  expect_equal(rmInfra("Lindsaea schomburgkii f. coriifolia (Lindm.) K.U. Kramer"),
               "Lindsaea schomburgkii coriifolia (Lindm.) K.U. Kramer")
  expect_equal(rmInfra("Heracleum sphondylium subsp. sibiricum var. lecokii"),
               "Heracleum sphondylium sibiricum lecokii")
  expect_equal(rmInfra("Heracleum sphondylium subsp. sibiricum var. lecokii L."),
               "Heracleum sphondylium sibiricum lecokii L.")
  expect_equal(rmInfra(tolower("Heracleum sphondylium subsp. sibiricum")),
               tolower("Heracleum sphondylium sibiricum"))
  expect_equal(rmInfra(toupper("Heracleum sphondylium subsp. sibiricum")),
               toupper("Heracleum sphondylium sibiricum"))
  expect_equal(rmInfra(toupper("Lindsaea lancea Mez var. angulata Rosenst.")),
               toupper("Lindsaea lancea angulata Rosenst."))
})

test_that("rmHyb works", {
  expect_equal(rmHyb("× Blechnum antillanum"), "Blechnum antillanum")
  expect_equal(rmHyb("Blechnum ×antillanum"), "Blechnum antillanum")
  expect_equal(rmHyb("Blechnum × antillanum"), "Blechnum antillanum")
  expect_equal(rmHyb("Blechnum x antillanum"), "Blechnum antillanum")
  expect_equal(rmHyb("Blechnum X antillanum"), "Blechnum antillanum")
})

test_that("addRank works", {
  expect_error(addRank("Lindsaea lancea"))
  expect_equal(addRank("", rank = "cf."),
               "")
  expect_equal(addRank(NA, rank = "cf."),
               NA)
  expect_equal(addRank("Lindsaea", rank = "cf."),
               "Lindsaea")
  expect_equal(addRank("Lindsaea lancea", rank = "cf."),
               "Lindsaea cf. lancea")
  expect_equal(addRank("Lindsaea lancea angulata", rank = "var."),
               "Lindsaea lancea var. angulata")
  expect_equal(addRank("Lindsaea schomburgkii coriifolia", rank = "f."),
               "Lindsaea schomburgkii f. coriifolia")
  expect_equal(addRank("Blechnum antillanum", rank = "\u00d7"),
               "Blechnum × antillanum")
  expect_equal(addRank("Blechnum antillanum", rank = "\u00d7", TRUE),
               "Blechnum ×antillanum")
  expect_equal(addRank("Lindsaea lancea angulata minor", rank = "f."),
               "Lindsaea lancea angulata f. minor")
})

