# Objects with the input
genus <- c("Trema", "Turnera", "Viviania", "Dombeya",
          "Casearia", "Casearia", "Casearea", "Xuxu",
          "Indet.", "",  NA, "Guidonia", "Casiarae")

# Objects with the expected resolutions
res0 <- c("Cannabaceae", "Turneraceae", "Vivianiaceae",
          "Malvaceae", "Salicaceae", "Salicaceae",
          rep(NA, 5), "Salicaceae", NA)
res1 <- c("Cannabaceae", "Turneraceae", "Vivianiaceae",
          "Malvaceae", "Salicaceae", "Salicaceae", "Salicaceae",
          rep(NA, 4), "Salicaceae", "Salicaceae")
res2 <- c("Cannabaceae", "Turneraceae", "Vivianiaceae",
          "Malvaceae", "Salicaceae", "Salicaceae", "Salicaceae",
          rep(NA, 4), "Salicaceae", NA)

# Tests
test_that("getFamily works", {
  expect_error(getFamily())
  expect_equal(getFamily(""), NA)
  expect_equal(getFamily(genus), res0)
  expect_equal(getFamily(genus, fuzzy.match = TRUE), res1)
  expect_equal(getFamily(genus, fuzzy.match = TRUE, max.dist = 0.1), res2)
})
