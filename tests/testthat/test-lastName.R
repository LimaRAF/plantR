test_that("lastName works", {
  expect_equal(lastName("Gert Hatschbach") , "Hatschbach")
  expect_equal(lastName("Hermogenes Leitão Filho") , "Leitão Filho")
  expect_equal(lastName("Augustin Saint-hilaire") , "Saint-Hilaire")
  expect_equal(lastName("Saint-Hilaire, A.") , "Saint-Hilaire")
  expect_equal(lastName("Maria Da Silva") , "Silva")
})

