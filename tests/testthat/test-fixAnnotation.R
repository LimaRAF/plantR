test_that("fixAnnotation works", {

  taxa <- c("Aa aa var ab", "Aa Aff.ab", "Ba cf.bc", "Bbcf.bc",
            "Aa aa subsp ab", "Ba Cf.bc", "Ba f.bc", "Aa f. bb (H. f.)")
  res <- c("Aa aa var. ab", "Aa aff. ab", "Ba cf. bc", "Bb cf. bc",
           "Aa aa subsp. ab", "Ba cf. bc", "Ba f. bc", "Aa f. bb (H. f.)")
  expect_equal(fixAnnotation(taxa), res)

  taxa <- c("Blechnumcf. spannagelii", "Blechnumcf.spannagelii", "Blechnum cf.spannagelii",
             "Blechnumaff. spannagelii", "Blechnumaff.spannagelii", "Blechnum aff.spannagelii")
  expect_equal(fixAnnotation(taxa),
               rep(c("Blechnum cf. spannagelii", "Blechnum aff. spannagelii"), each = 3))

})

