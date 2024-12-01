test_that("fixIndet works", {

  taxa <- c("Indeterminada1", "Indeterminada sp1", "undeterminedA",
            "Fabaceae1", "FabaceaeA", "indet", "Fabales2", "Andira sp. 2",
            "Andira sp 2", "Andira sp", "Andira spp",
            "Fabaceae", "Fabaceae sp", "Fabaceae sp.",
            "Fabaceae 1", "Fabales 1", "Fabaceae sp.1",
            "Andira", "Andira1", "Andira 1", "Andira sp.1",
            "Fabaceae A", "Fabaceae spA", "indet", "")
  res <- c("Indet. sp.1", "Indet. sp.1", "Indet. sp.A", "Fabaceae sp.1",
           "Fabaceae sp.A", "Indet. sp.", "Fabales sp.2", "Andira sp.2",
           "Andira sp.2", "Andira sp.", "Andira spp.",
           "Fabaceae", "Fabaceae sp.", "Fabaceae sp.",
           "Fabaceae sp.1", "Fabales sp.1", "Fabaceae sp.1",
           "Andira", "Andira sp.1", "Andira sp.1", "Andira sp.1",
           "Fabaceae sp.A", "Fabaceae sp.A", "Indet. sp.", "Indet. sp.")
  expect_equal(fixIndet(taxa), res)
})
