


test_that("getTaxBackbone works", {

  dbs <- getTaxBackbone("bfo")

  expect_equal(length(dbs[[1]]), length(plantR::bfoNames))
  expect_equal(names(dbs), "bfo")
  expect_equal(class(dbs), "list")

})
