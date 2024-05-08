test_that("fixAnnotation works", {

  taxa <- c("Aa aa var ab", "Aa Aff.ab", "Ba cf.bc", "Bbcf.bc",
            "Aa aa subsp ab", "Ba Cf.bc")
  res <- c("Aa aa var. ab", "Aa aff. ab", "Ba cf. bc", "Bbcf. bc",
           "Aa aa subsp. ab", "Ba cf. bc")
  expect_equal(fixAnnotation(taxa), res)
})
