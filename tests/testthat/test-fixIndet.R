test_that("fixIndet works", {

  taxa <- c("Indeterminada1", "Indeterminada sp1", "undeterminedA",
            "Fabaceae1", "FabaceaeA", "indet", "Fabales2", "Andira sp. 2",
            "Andira sp 2", "Andira sp", "Tabebuia spp")
  res <- c("Indet. sp.1", "Indet. sp.1", "Indet. sp.A", "Fabaceae sp.1",
           "Fabaceae sp.A", "Indet. sp.", "Fabales sp.2", "Andira sp.2",
           "Andira sp.2", "Andira sp.", "Tabebuia spp.")
  expect_equal(fixIndet(taxa), res)
})
