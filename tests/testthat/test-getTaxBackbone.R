


test_that("getTaxBackbone works", {

  expect_error(getTaxBackbone())
  expect_error(getTaxBackbone(db = character()))
  expect_error(getTaxBackbone(db = "tpl"))
  expect_error(getTaxBackbone(db = "xuxu"))

  dbs <- getTaxBackbone("bfo")
  expect_equal(length(dbs[[1]]), length(plantR::bfoNames))
  expect_equal(names(dbs), "bfo")
  expect_equal(class(dbs), "list")

  db.fb0 <- head(plantR::bfoNames, 2)
  dbs1 <- getTaxBackbone(db.fb0)

  expect_equal(names(dbs1), "user-provided")
  expect_equal(class(dbs1), "list")

  db.fb0.list <- list(head(plantR::bfoNames, 2), tail(plantR::bfoNames, 2))
  dbs1.list <- getTaxBackbone(db.fb0.list)


})
