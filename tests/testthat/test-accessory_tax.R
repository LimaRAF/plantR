test_that("rmOpen works", {
  expect_equal(rmOpen("Lindsaea cf. lancea"), "Lindsaea lancea")
  expect_equal(rmOpen("Lindsaea aff. lancea"), "Lindsaea lancea")
})

test_that("rmInfra works", {
  expect_equal(rmInfra("Lindsaea lancea var. angulata"), "Lindsaea lancea angulata")
  expect_equal(rmInfra("Lindsaea lancea (L.) Bedd. var. angulata Rosenst."),
               "Lindsaea lancea angulata Rosenst.")
  expect_equal(rmInfra("Lindsaea schomburgkii f. coriifolia (Lindm.) K.U. Kramer"),
               "Lindsaea schomburgkii coriifolia (Lindm.) K.U. Kramer")
})

test_that("rmHyb works", {
  expect_equal(rmHyb("Blechnum ×antillanum"), "Blechnum antillanum")
  expect_equal(rmHyb("Blechnum × antillanum"), "Blechnum antillanum")
  expect_equal(rmHyb("Blechnum x antillanum"), "Blechnum antillanum")
  expect_equal(rmHyb("Blechnum X antillanum"), "Blechnum antillanum")
})

test_that("addRank works", {
  expect_equal(addRank("Lindsaea lancea", rank = "cf."),
               "Lindsaea cf. lancea")
  expect_equal(addRank("Lindsaea lancea angulata", rank = "var."),
               "Lindsaea lancea var. angulata")
  expect_equal(addRank("Lindsaea schomburgkii coriifolia", rank = "f."),
               "Lindsaea schomburgkii f. coriifolia")
  expect_equal(addRank("Blechnum antillanum", rank = "\u00d7"),
               "Blechnum ×antillanum")
})
