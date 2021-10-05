mat <- matrix(c(0, 150.871574753673, 150.871574753673,0), ncol=2)
test_that("geoDist works", {
  expect_equal(geoDist(c(-47, -46), c(-23, -24)), mat)
})

lat <- c(-23.475389, -23.475389, -23.475390, -23.475389)
lon <- c(-47.123768, -47.123768, -47.123768, -47.123868)
res <- c(0.0000000000, 0.0000000000, 0.0001342588, 0.0101992504)
test_that("minDist works", {
  expect_equal(minDist(lon, lat, output = 'group'), c(1,1,1,4))
  expect_equal(minDist(lon, lat, output = 'dist'), res)
})

lon <- c(-42.2,-42.6,-45.3,-42.5,-42.3,-39.0,-12.2)
lat <- c(-44.6,-46.2,-45.4,-42.2,-43.7,-45.0,-8.0)
test_that("mahalanobisDist works", {
  expect_equal(mahalanobisDist(lon, lat, method = "classic") > 10,
                rep(FALSE, 7))
  expect_equal(mahalanobisDist(lon, lat, method = "robust") > 10,
                c(rep(FALSE, 6), rep(TRUE, 1)))
})

test_that("distOutlier works", {
  expect_equal(distOutlier(jitter(rep(lon, each=2)), rep(lat, each=2))[2],
               c("n.out" = 6))
})
