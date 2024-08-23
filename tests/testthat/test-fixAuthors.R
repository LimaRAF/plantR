
test_that("fixAuthors works", {
  taxa <- c("Lindsaea", "Lindsaea sp.", "Lindsaea lancea",
            "Lindsaea lancea (L.) Bedd.", "Parablechnum C.Presl",
            "Blechnum spannagelii Rosenst.",
            "Blechnum austrobrasilianum de la Sota",
            "Blechnum occidentale leopoldense Dutra",
            "Casearia sylvestris var. angustifolia",
            "Casearia sylvestris var. angustifolia Uittien",
            "Casearia sylvestris Sw. var. sylvestris",
            "Casearia sylvestris var. sylvestris Sw.",
            "Casearia sylvestris var. sylvestris",
            "Philodendron sodiroi hort.",
            "Taraxacum illyricum Dahlst.apud van Soest",
            "Helichrysum tenuiculum sensu Hilliard non DC.",
            "Chrysanthemum lavandulifolium var. hortense Makino",
            "Hieracium delasoiei De la Soie",
            "Aberemoa guianensis aubl.", "Ablania aubl.", "Ablania Aubl.",
            "Ablania guianensis Aubl.", "Acacia macracantha var. glabrens eggers",
            "Dolichos stipulosus f. angustifoliolata (Baker f. & DC.) Baker",
            "Agarista coriifolia (Thunb.) Hook. f. ex Nied.",
            "Agarista coriifolia (Thunb.) Hook. f.",
            "Cirsium caput-medusae Schur ex Nyman",
            "Onoseris paniculata ex DC.",
            "× Argyrautia degeneri Sherff")

  res <- c("Lindsaea", "Lindsaea sp.", "Lindsaea lancea",
           "Lindsaea lancea", "Parablechnum",
           "Blechnum spannagelii",
           "Blechnum austrobrasilianum",
           "Blechnum occidentale leopoldense",
           "Casearia sylvestris var. angustifolia",
           "Casearia sylvestris var. angustifolia",
           "Casearia sylvestris var. sylvestris",
           "Casearia sylvestris var. sylvestris",
           "Casearia sylvestris var. sylvestris",
           "Philodendron sodiroi",
           "Taraxacum illyricum",
           "Helichrysum tenuiculum",
           "Chrysanthemum lavandulifolium var. hortense",
           "Hieracium delasoiei",
           "Aberemoa guianensis", "Ablania", "Ablania",
           "Ablania guianensis", "Acacia macracantha var. glabrens",
           "Dolichos stipulosus f. angustifoliolata",
           "Agarista coriifolia", "Agarista coriifolia",
           "Cirsium caput-medusae", "Onoseris paniculata",
           "× Argyrautia degeneri")
  res1 <- c(NA, NA, NA, "(L.) Bedd.", "C.Presl", "Rosenst.",
            "de la Sota", "Dutra", NA, "Uittien", "Sw.", "Sw.", NA,
            "hort.", "Dahlst.apud van Soest", "sensu Hilliard non DC.",
            "Makino", "De la Soie", "Aubl.", "Aubl.", "Aubl.",
            "Aubl.", "Eggers",  "(Baker f. & DC.) Baker",
            "(Thunb.) Hook. f. ex Nied.", "(Thunb.) Hook. f.",
            "Schur ex Nyman", "ex DC.", "Sherff")

  res_func <- fixAuthors(taxa)

  expect_length(res_func, 3)
  expect_equal(names(res_func), c("orig.name", "tax.name", "tax.author"))
  expect_equal(res_func$orig.name, taxa)
  expect_equal(res_func$tax.name, res)
  expect_equal(res_func$tax.author, res1)

})
