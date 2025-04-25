test_that("lastName works", {
  expect_equal(lastName("Gert Hatschbach") , "Hatschbach")
  expect_equal(lastName("Hermogenes Leitao Filho") , "Leitao Filho")
  expect_equal(lastName("Hermogenes Leitao Filho", invert = TRUE), "Hermogenes")
  expect_equal(lastName("Hermogenes Leitao Filho",
                        invert = TRUE, initials = TRUE), "H.")
  lastName("H Leitao Filho", invert = TRUE, initials = FALSE)
  expect_equal(lastName("H Leitao Filho", invert = TRUE, initials = F), "H")
  expect_equal(lastName("H Leitao Filho", invert = TRUE, initials = TRUE), "H.")
  expect_equal(lastName("hermogenes leitao filho", first.capital = FALSE), "leitao filho")
  expect_equal(lastName("Hermogenes Leitao Filho", first.capital = FALSE), "Leitao Filho")
  expect_equal(lastName("Augustin Saint-hilaire") , "Saint-Hilaire")
  expect_equal(lastName("Saint-Hilaire, A.") , "Saint-Hilaire")
  expect_equal(lastName("Maria Da Silva") , "Silva")
  #expect_equal(lastName("Maria Da Silva", invert = TRUE) , "Maria")
  expect_equal(lastName(c("José Santos", NA)) , c("Santos","s.n."))
})

test_that("lastName works again", {
  nomes <- c("AFA Lira", "AF Araujo Lira", "Araujo da Lira, Andre Ferreira")
  expected <- c(rep("Lira", 2), "Da Lira")
  expected1 <- c("AFA", "AF Araujo", "Andre Ferreira Araujo")

  expect_equal(lastName(nomes) , expected)
  expect_equal(lastName(nomes, invert = TRUE), expected1)
})
