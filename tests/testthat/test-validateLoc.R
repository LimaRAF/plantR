# Creating a data frame with locality information
df <- data.frame(resol.orig = rep(c("locality", "municipality", "stateProvince", "country"), 5),
                resolution.gazetteer = rep(c("no_info", "country", "state","county", "locality"), each = 4),
                stringsAsFactors = FALSE)

# Expected result
res <- c("check_local.2no.info", "check_municip.2no.info", "check_state2no.info",
         "check_country2no.info", "check_local.2country", "check_municip.2country",
        "check_state2country", "ok_same_resolution", "check_local.2state",
        "check_municip.2state", "ok_same_resolution", "ok_country2state",
        "check_local.2municip.", "ok_same_resolution", "ok_state2municip.",
        "ok_country2municip.", "ok_same_resolution", "ok_municip.2locality",
        "ok_state2locality", "ok_country2locality")

# Tests
test_that("validateLoc works", {
  expect_equal(validateLoc(df)$loc.check, res)
})
