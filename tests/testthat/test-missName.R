test_that("missName works", {
  expect_equal(missName("", type = "collector"), "Anonymous")
  expect_equal(missName("s/col.", type = "collector"), "Anonymous")
  expect_equal(missName("s/det.", type = "collector"), "s/det.")
  expect_equal(missName("s/collecteur", type = "identificator"), "s/collecteur")
  expect_equal(missName("s/colector", type = "collector"), "Anonymous")
  expect_equal(missName("s/col", type = "colector"), "Anonymous")
  #expect_equal(missName("s/collecteur", type = "collector"), "Anonymous")
  expect_equal(missName("s/colector", type = "collector"), "Anonymous")
  expect_equal(missName("s/colector", type = "colector"), "Anonymous")
  #expect_equal(missName("sans collecteur",
  #                      type = "collector",
  #                      noName = "s./c."), "s./c.")
})
