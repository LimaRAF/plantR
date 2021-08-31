test_that("lastName works", {
  expect_equal(lastName("Gert Hatschbach") , "Hatschbach")
  expect_equal(lastName("Hermogenes Leitão Filho") , "Leitão Filho")
  expect_equal(lastName("Hermogenes Leitão Filho", invert = TRUE), "Hermogenes")
  expect_equal(lastName("Hermogenes Leitão Filho",
                        invert = TRUE, initials = TRUE), "H.")
  lastName("H Leitão Filho", invert = TRUE, initials = F)
  expect_equal(lastName("H Leitão Filho", invert = TRUE, initials = F), "H")
  expect_equal(lastName("H Leitão Filho", invert = TRUE, initials = TRUE), "H.")
  expect_equal(lastName("Augustin Saint-hilaire") , "Saint-Hilaire")
  expect_equal(lastName("Saint-Hilaire, A.") , "Saint-Hilaire")
  expect_equal(lastName("Maria Da Silva") , "Silva")
  expect_equal(lastName(c("José Santos", NA)) , c("Santos","s.n."))
})

