## Creating some locality information
df <- data.frame(resol.orig = c("municipality","locality"),
                 loc.string = c("brazil_rio janeiro_parati","brazil_rio janeiro_paraty"),
                 loc.string1 = c(NA, "brazil_rio janeiro_paraty_paraty mirim"),
                 stringsAsFactors = FALSE)

# Objects with the expected results
df.out <- data.frame(loc = c("brazil_rio janeiro_parati","brazil_rio janeiro_paraty_paraty mirim"),
                     loc.correct = c("brazil_rio janeiro_paraty","brazil_rio janeiro_paraty_paraty mirim"),
                     latitude.gazetteer = c(-23.14821837, -23.245651),
                     longitude.gazetteer = c(-44.70690993, -44.639223),
                     resolution.gazetteer = c("county","locality"),
                     stringsAsFactors = FALSE)
res0 <- cbind.data.frame(df, df.out)

# Tests
test_that("getLoc works", {
  expect_equal(getLoc(df), res0)
  expect_equal(getLoc(df, orig.names = TRUE), res0)
})
