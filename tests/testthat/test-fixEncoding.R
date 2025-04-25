# test_that("fixEncoding works", {
#   nomes <- c("JosÃ©", "GonÃ§alves", "MÃ¼ller", "LondoÃ±o")
#   res <- fixEncoding(nomes, encoding = "UTF-8")
#   exp <- c("José", "Gonçalves", "Müller", "Londoño")
#   # exp <- c("Jos<c3><a9>", "Gon<c3><a7>alves", "M<c3><bc>ller", "Londo<c3><b1>o")
#   expect_equal(res, exp)
# })


