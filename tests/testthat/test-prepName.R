test_that("prepName works", {
  expect_equal(prepName("Alwyn H. Gentry"), "Gentry, A.H.")
  expect_equal(prepName("Karl Emrich & Balduino Rambo", sep.out = "&"),
               "Emrich, K.&Rambo, B.")
  expect_equal(prepName("R. Reitz; R.M. Klein"), "Reitz, R.|Klein, R.M.")
  expect_equal(prepName("Reitz, Raulino et R.M. Klein"), "Reitz, R.|Klein, R.M.")
  expect_equal(
    prepName("Carl F. P. von Martius; Augustin Saint-hilaire"),
    "Martius, C.F.P.|Saint-Hilaire, A."
  )
  expect_equal(
    prepName("Carl F. P. von Martius; Auguste de Saint-Hilaire", get.prep = F),
    "Martius, C.F.P.|Saint-Hilaire, A."
  )
  expect_equal(prepName("A. ducke; dárdano de Andrade-Lima"),
               "Ducke, A.|Andrade-Lima, D.")
  # Names with generational suffixes
  expect_equal(
    prepName("HF Leitão Filho; GJ Shepherd", special.char = TRUE),
    "Leitão Filho, H.F.|Shepherd, G.J."
  )
  # Names with titles
  expect_equal(prepName("Pe. Raulino Reitz"), "Reitz, R.")
  expect_equal(prepName("Prof. Hermogenes de Freitas Leitao Filho"),
               "Leitao Filho, H.F.")
  expect_equal(prepName("Sir G.T. Prance"), "Prance, S.G.T.")
  expect_equal(prepName("Dra. Gloria Galeano"), "Galeano, G.")
  expect_equal(prepName("[D. Hugh-Jones]"), "[Hugh-Jones, D.]")
  expect_equal(prepName("L. McDade & J. O'Brien"), "McDade, L.|O'Brien, J.")
  })
