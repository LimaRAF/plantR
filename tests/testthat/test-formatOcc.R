
occs <- read.csv("example.csv")
occs <- formatDwc(splink_data = occs)
occs_format <- formatOcc(occs)

test_that("formatOcc works", {
  #setdiff(names(occs), names(occs_format))
  setdiff(names(occs_format), names(occs))
  # check that all columns are still there
  expect_equal(setdiff(names(occs), names(occs_format)), character(0))
  # check that the columns were created
  new_cols <- c("collectionCode.new",
                "collectionObs",
                "recordedBy.new",
                "recordNumber.new",
                "year.new",
                "identifiedBy.new",
                "yearIdentified.new",
                "recordedBy.aux",
                "identifiedBy.aux",
                "last.name")
  expect_equal(setdiff(names(occs_format), names(occs)), new_cols)
})

occs_loc <- formatLoc(occs_format)
test_that("formatLoc works", {
  expect_equal(setdiff(names(occs_format), names(occs_loc)), character(0))
  loc_cols <- c("country.new",
                "stateProvince.new",
                "municipality.new",
                "locality.new",
                "locality.scrap",
                "resol.orig",
                "loc",
                "loc.correct",
                "latitude.gazetteer",
                "longitude.gazetteer",
                "resolution.gazetteer")
  expect_equal(setdiff(names(occs_loc), names(occs_format)), loc_cols)
})

occs_fc <- formatCoord(occs_loc)
test_that("formatCoord works", {
  expect_equal(setdiff(names(occs_loc), names(occs_fc)), character(0))
  coord_cols <- c("coord.check",
                  "decimalLatitude.new",
                  "decimalLongitude.new",
                  "origin.coord",
                  "precision.coord")
  expect_equal(setdiff(names(occs_fc), names(occs_loc)), coord_cols)
})

occs_tax <- formatTax(occs_fc)

test_that("formatTax works", {
  expect_equal(setdiff(names(occs_fc), names(occs_tax)), character(0))
  tax_cols <- c("scientificName.new",
                  "scientificNameStatus",
                  "suggestedName",
                  "scientificNameFull",
                  "tax.notes",
                  "tax.source",
                  "family.new")

  expect_equal(setdiff(names(occs_tax), names(occs_fc)), tax_cols)
})

occs_val_loc <- validateLoc(occs_tax)

test_that("validateLoc works", {
  expect_equal(setdiff(names(occs_tax), names(occs_val_loc)), character(0))
  expect_equal(setdiff(names(occs_val_loc), names(occs_tax)), "loc.check")
})

occs_val_coord <- validateCoord(occs_val_loc)
###AQUI quedó la consola
test_that("validateCoord works", {
  expect_equal(setdiff(names(occs_val_loc), names(occs_val_coord)), character(0))
  expect_equal(setdiff(names(occs_val_coord), names(occs_val_loc)), "loc.check")
})


