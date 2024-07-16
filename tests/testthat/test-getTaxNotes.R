


test_that("getTaxNotes works", {

  nomes <- c("Casearia sylvestris", "Casearia sylvestris",
             "Casearia sylvestris", NA,
             "Casearia sylvestris var. angustifolia",
             "Casearia attenuata",
             "Casearia celtidifolia", "Casearia celtidifolia",
             "Casearia celtidifolia", "Casearia serrulata",
             "Casearia serrulata", "Casearia serrulata",
             "Xylosma ciliatifolium", NA)
  df_input <- data.frame(suggestedName = nomes)
  df_input$match_type <- c("exact_w_author", "exact_wout_author",
                           "fuzzy_w_author", "no_match", "exact_w_author",
                           "exact_w_author", "exact_wout_author",
                           "exact_w_author", "exact_w_author",
                           "exact_wout_author", "exact_w_author",
                           "exact_w_author", "exact_w_author", "no_match")
  df_input$multiple_match <- c(FALSE, FALSE, FALSE, NA, FALSE, FALSE,
                               TRUE, FALSE, FALSE, TRUE, FALSE, FALSE,
                               FALSE, NA)
  df_input$fuzzy_dist_name <- c(0, 0, 0.2174, NA, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, NA)
  df_input$fuzzy_dist_author <- c(0, NA, 0, NA, 0, 0, NA, 0, 0, NA,
                                0, 0, 0, NA)
  df_input$name.status <- c("valid", "valid", "valid", NA, "incorrect",
                            "incorrect", "incorrect", "incorrect",
                            "incorrect", "incorrect", "incorrect",
                            "incorrect", "orthographic variant", NA)
  df_input$taxon.status <- c("accepted", "accepted", "accepted", NA,
                             "synonym", "synonym", "synonym",
                             "synonym", "synonym", "synonym", "synonym",
                             "synonym", "unplaced", NA)

  res_func <- getTaxNotes(df_input)

  res0 <- c("name accepted", "name accepted", "name misspelled", "not found",
            "synonym", "synonym", "check +1 name|synonym", "synonym",
            "synonym", "check +1 name|synonym", "synonym", "synonym",
            "orthographic variant|check not resolved", "not found")


  expect_length(res_func$notes, dim(df_input)[1])
  expect_equal(res_func[["suggestedName"]], nomes)
  expect_equal(res_func$notes, res0)

})
