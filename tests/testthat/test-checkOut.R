# few data and close coordinates (no outliers)
lon <- c(-42.2,-42.3,-42.4,-42.3,-42.3)
lat <- c(-44.3,-44.2,-44.2,-42.2,-42.2)
df <- data.frame(lon = lon, lat = lat)
df$scientificName.new <- "sp 1"
df$geo.check <- TRUE
df$cult.check <- TRUE

#expected results
res0 <- df
res0$out.check <- c(rep(FALSE, 5))
res1 <- df
res1$out.check <- c(rep(FALSE, 3), rep(TRUE, 2))


# Tests
test_that("checkOut works", {

  expect_error(checkOut(TRUE))
  expect_error(checkOut(data.frame(character())))
  expect_error(checkOut(data.frame(xxxx = c("Aa bb", "Bb cc"))))

  expect_equal(suppressWarnings(checkOut(df, lon = "lon", lat = "lat", n.min = 4)),
               res0)
  expect_equal(suppressWarnings(checkOut(df, lon = "lon", lat = "lat", clas.cut = NULL, n.min = 4)),
               res1)
})


# # some data and one outlier
# lon <- c(runif(5, -45, -41), -12.2)
# lat <- c(runif(5, -45, -41), -18.2)
# df <- data.frame(lon = lon, lat = lat)
#
# checkOut(df, lon = "lon", lat = "lat")
# checkOut(df, lon = "lon", lat = "lat", clas.cut = NULL)
# checkOut(df, lon = "lon", lat = "lat", rob.cut = NULL)
#
# # more data and one outlier
# lon <- c(runif(9, -45, -41), -12.2)
# lat <- c(runif(9, -45, -41), -18.2)
# df <- data.frame(lon = lon, lat = lat)
#
# checkOut(df, lon = "lon", lat = "lat")
# checkOut(df, lon = "lon", lat = "lat", clas.cut = NULL)
# checkOut(df, lon = "lon", lat = "lat", rob.cut = NULL)
