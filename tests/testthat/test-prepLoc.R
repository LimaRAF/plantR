test_that("prepLoc works", {
  expect_equal(prepLoc("Brazil_Rio de Janeiro"), "brazil_rio janeiro")
  expect_equal(prepLoc("Brazil_Rio Grande do Sul_Coqueiros do Sul"),
               "brazil_rio grande sul_coqueiros sul")
  expect_equal(prepLoc("Brazil_Bahia_Arraial d'Ajuda"),
               "brazil_bahia_arraial ajuda")
  expect_equal(prepLoc("Brazil_São Paulo_Sete Barras_Parque Estadual de Carlos Botelho"),
               "brazil_sao paulo_sete barras_parque estadual carlos botelho")
  expect_equal(prepLoc("Cuba_Isla de la Juventud"), "cuba_isla juventud")
  expect_equal(prepLoc("Brazil_São Paulo_São José dos Campos"), "brazil_sao paulo_sao jose campos")
})

