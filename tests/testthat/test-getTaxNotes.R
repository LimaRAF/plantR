# output when mult.matches = "all"
nomes <- c("Casearia sylvestris", "Casearia sylvestris",
           "Casearia sylvestris", NA,
           "Casearia sylvestris var. angustifolia",
           "Casearia attenuata",
           "Casearia celtidifolia", "Casearia celtidifolia",
           "Casearia celtidifolia", "Casearia serrulata",
           "Casearia serrulata", "Casearia serrulata",
           "Xylosma ciliatifolium", NA,
           "Ocotea sp.", "Chrysophyllum mexicanum",
           "Lobelia","Justicia","Gerardia")
df_input <- data.frame(suggestedName = nomes)
df_input$match_type <- c("exact_w_author", "exact_wout_author",
                         "fuzzy_w_author", "no_match", "exact_w_author",
                         "exact_w_author", "exact_wout_author",
                         "exact_w_author", "exact_w_author",
                         "exact_wout_author", "exact_w_author",
                         "exact_w_author", "exact_w_author", "no_match",
                         "exact_w_indet", "bad_fuzzy_wout_author",
                         rep("exact_wout_author", 3))
df_input$multiple_match <- c(FALSE, FALSE, FALSE, NA, FALSE, FALSE,
                             TRUE, FALSE, FALSE, TRUE, FALSE, FALSE,
                             FALSE, NA, FALSE, FALSE,
                             TRUE, TRUE, TRUE)
df_input$fuzzy_dist_name <- c(0, 0, 0.2174, NA, 0, 0, 0, 0, 0, 0,
                              0, 0, 0, NA, 0, 0.1739,
                              0, 0, 0)
df_input$fuzzy_dist_author <- c(0, NA, 0, NA, 0, 0, NA, 0, 0, NA,
                                0, 0, 0, NA, 0, NA,
                                NA, NA, NA)
df_input$name.status <- c("valid", "valid", "valid", NA, "incorrect",
                          "incorrect", "incorrect", "incorrect",
                          "incorrect", "incorrect", "incorrect",
                          "incorrect", "orthographic variant", NA,
                          "valid", NA,
                          "illegitimate|valid", "conserved|illegitimate", "rejected")
df_input$taxon.status <- c("accepted", "accepted", "accepted", NA,
                           "synonym", "synonym", "synonym",
                           "synonym", "synonym", "synonym", "synonym",
                           "synonym", "unplaced", NA, "accepted", NA,
                           "synonym|accepted", "accepted|synonym", "synonym")

res0 <- c("name accepted", "name accepted", "name misspelled", "not found",
          "synonym", "synonym", "check +1 name", "synonym",
          "synonym", "check +1 name", "synonym", "synonym",
          "orthographic variant", "not found",
          "name accepted", "bad match",
          rep("check +1 name", 3))

# output when mult.matches = "best"
df_input1 <- df_input
rep_these <- res0 %in% "check +1 name"
df_input1$name.status[rep_these] <- c("incorrect", "incorrect",
                                      "valid", "conserved", "")
df_input1$taxon.status[rep_these] <- c("synonym", "synonym",
                                       "accepted", "accepted", "synonym")
res1 <- c("name accepted", "name accepted", "name misspelled", "not found",
          "synonym", "synonym", "check +1 name", "synonym",
          "synonym", "check +1 name", "synonym", "synonym",
          "orthographic variant", "not found",
          "name accepted", "bad match",
          rep("check +1 name", 3))



test_that("getTaxNotes works", {

  expect_error(getTaxNotes())
  expect_error(getTaxNotes(data.frame(character())))
  expect_error(getTaxNotes(df_input[,-which(names(df_input) == "name.status")]))

  res_func <- getTaxNotes(df_input)

  expect_length(res_func$notes, dim(df_input)[1])
  expect_equal(res_func[["suggestedName"]], nomes)
  expect_equal(res_func$notes, res0)


  res_func1 <- getTaxNotes(df_input1)
  expect_equal(res_func1$notes, res1)


})
