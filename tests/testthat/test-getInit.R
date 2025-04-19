# Tests
test_that("getInit works", {
  # Full names
  expect_equal(getInit("Alwyn"), "A.")
  expect_equal(getInit("Alwyn Howard Gentry"), "A.H.G.")
  expect_equal(getInit("Alwyn H. Gentry"), "A.H.G.")
  expect_equal(getInit("A. Gentry"), "A.G.")
  expect_equal(getInit("A. H. Gentry"), "A.H.G.")
  expect_equal(getInit("A. H. Gentry", rm.spaces = FALSE), "A. H. G.")
  expect_equal(getInit("A.H.Gentry"), "A.H.G.")
  # Abbreviations
  expect_equal(getInit("A"), "A.")
  expect_equal(getInit("A H G"), "A.H.G.")
  expect_equal(getInit("A. H. G."), "A.H.G.")
  # Capitalized and lower-case names
  expect_equal(getInit("ALWYN HOWARD GENTRY"), "A.H.G.")
  expect_equal(getInit("AHG"), "A.H.G.")
  expect_equal(getInit("a.h. gentry"), "A.H.G.")
  expect_equal(getInit("alwyn"), "A.")
  expect_equal(getInit("alwyn", upper = FALSE), "a.")
  expect_equal(getInit("Alwyn", upper = FALSE), "A.")
  expect_equal(getInit("alwyn gentry", upper = FALSE), "a.g.")
  expect_equal(getInit("alwyn gentry", upper = FALSE, rm.spaces = FALSE), "a. g.")
  expect_equal(getInit("ALWYN"), "A.")
  # Other formats
  expect_equal(getInit("Auguste Saint-Hilaire"), "A.S.-H.")
  expect_equal(getInit("John MacDonald"), "J.MacD.")
  expect_equal(getInit("John McDonald"), "J.McD.")
  expect_equal(getInit("John O'Brien"), "J.O'B.")
})

# Some problematic (unresolved) examples
expect_equal(getInit("AG"), "A.G.")
expect_equal(getInit("AG", max.initials = 2), "A.")
expect_equal(getInit("Carl F. P. von Martius"), "C.F.P.V.M.")
expect_equal(getInit("AH gentry"), "A.G.")
