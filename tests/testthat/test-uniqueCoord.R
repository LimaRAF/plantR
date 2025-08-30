# Creating a data frame with coordinates information
coords <- data.frame(scientificName.new = rep("sp1", 9),
 lat = c(-23.475389, -23.475389, -23.475390, -23.485389,
         -23.575389, -23.575389, -23.575390, -23.485389, -23.485389),
 lon = c(-47.123768, -47.123768, -47.123768, -47.113768,
         -47.223768, -47.223768, -47.223768, -47.113768, NA))

# Expected result
res0 <- cbind.data.frame(coords,
                         exact.ID = c(2, 2, 3, 1, 4, 4, 5, 1, NA),
                         dist.ID = c(2, 2, 2, 1, 3, 3, 3, 1, NA))
res1 <- cbind.data.frame(coords,
                         dist.ID = c(1, 1, 1, 2, 3, 3, 3, 2, NA))
res2 <- cbind.data.frame(coords,
                         exact.ID = c(2, 2, 3, 1, 4, 4, 5, 1, NA),
                         dist.ID = c(2, 2, 3, 1, 4, 4, 5, 1, NA))
res3 <- cbind.data.frame(coords,
                         exact = c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, NA),
                         dists = c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, NA))

# Tests
test_that("uniqueCoord works", {

  expect_error(uniqueCoord(x = c(-25, -43)))
  expect_error(uniqueCoord(coords))
  suppressWarnings(
    expect_warning(uniqueCoord(coords[, 2:3], lon = "lon", lat = "lat")))

  expect_equal(suppressWarnings(uniqueCoord(coords, lon = "lon", lat = "lat")),
               res0)
  expect_equal(suppressWarnings(uniqueCoord(coords, lon = "lon", lat = "lat", type = "dist")),
               res1)
  expect_equal(suppressWarnings(uniqueCoord(coords, lon = "lon", lat = "lat", min.dist = 0.00001)),
               res2)
  expect_equal(suppressWarnings(uniqueCoord(coords, lon = "lon", lat = "lat", output = "flag")),
               res3)
})
