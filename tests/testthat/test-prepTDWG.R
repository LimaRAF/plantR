test_that("prepTDWG works", {
# Single names
  expect_equal(prepTDWG("gentry"), "Gentry")
  expect_equal(prepTDWG("GENTRY"), "Gentry")
  # Simple names
  expect_equal(prepTDWG("Alwyn Howard Gentry"), "Gentry, A.H.")
  expect_equal(prepTDWG("Alwyn H. Gentry"), "Gentry, A.H.")
  expect_equal(prepTDWG("A.H. Gentry"), "Gentry, A.H.")
  expect_equal(prepTDWG("A H Gentry"), "Gentry, A.H.")
  expect_equal(prepTDWG("Gentry, Alwyn Howard"), "Gentry, A.H.")
  expect_equal(prepTDWG("Gentry, AH"), "Gentry, A.H.")
  expect_equal(prepTDWG("Gentry AH"), "Gentry, A.H.")
  expect_equal(prepTDWG("GENTRY, A H"), "Gentry, A.H.")
  expect_equal(prepTDWG("GENTRY, A H", pretty = FALSE), "GENTRY, A. H.")
  expect_equal(prepTDWG("gentry, alwyn howard"), "Gentry, A.H.")
  expect_equal(prepTDWG("gentry, a.h."), "Gentry, A.H.")
  expect_equal(prepTDWG("gentry, a. h."), "Gentry, A.H.")
  # Name with prepositions
  expect_equal(prepTDWG("Carl F. P. von Martius"), "Martius, C.F.P.")
  expect_equal(prepTDWG("Carl F. P. von Martius", get.prep = TRUE), "Martius, C.F.P. von")
  expect_equal(prepTDWG("Carl F. P. von Martius", get.prep = TRUE, pretty = FALSE),
               "Martius, C. F. P. von")
  # Names with generational suffixes
  expect_equal(prepTDWG("Hermogenes de Freitas Leitao Filho"), "Leitao Filho, H.F.")
  expect_equal(prepTDWG("H.F. Leitao Filho"), "Leitao Filho, H.F.")
  expect_equal(prepTDWG("Leitao Filho, HF"), "Leitao Filho, H.F.")
  expect_equal(prepTDWG("Leitao filho, H. F."), "Leitao Filho, H.F.")
  # Compound last name
  expect_equal(prepTDWG("Augustin Saint-Hilaire"), "Saint-Hilaire, A.")
  expect_equal(prepTDWG("A. Saint-Hilaire"), "Saint-Hilaire, A.")
  expect_equal(prepTDWG("Saint-Hilaire, Augustin"), "Saint-Hilaire, A.")
  # Other formats
  expect_equal(prepTDWG("John MacDonald"), "MacDonald, J.")
  expect_equal(prepTDWG("John McDonald"), "McDonald, J.")
  expect_equal(prepTDWG("John O'Brien"), "O'Brien, J.")
  # Multiple names, different settings
  expect_equal(prepTDWG("Alwyn Howard Gentry", format = "init_last"), "A.H. Gentry")
  expect_equal(prepTDWG("Carl F. P. von Martius",
                        format = "init_last", get.prep = TRUE), "C.F.P. von Martius")
  expect_equal(prepTDWG("Carl F. P. von Martius",
                        get.prep = TRUE, format = "prep_last_init"), "von Martius, C.F.P.")
  expect_equal(prepTDWG("Carl F. P. von Martius",
                        get.prep = TRUE, format = "prep_last_init",
                        get.initials = FALSE), "von Martius, Carl F.P.")
  expect_equal(prepTDWG("Lima, R.A.F. (ESA)"), "Lima, R.A.F.")
  expect_equal(prepTDWG("R.A.F. Lima (ESA)"), "Lima, R.A.F.")
})






