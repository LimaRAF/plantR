test_that("fixName works overall", {
  expect_equal(fixName("J.E.Q. Faria Jr."), "J.E.Q. Faria Junior")
  expect_equal(fixName("Leitão F°, H.F."), "Leitao Filho, H.F.")
  expect_equal(fixName("Gert G. Hatschbach, et al."), "Gert G. Hatschbach")
  expect_equal(fixName("Karl Emrich & Balduino Rambo"), "Karl Emrich|Balduino Rambo")
  expect_equal(fixName("( Karl) Emrich ;(Balduino ) Rambo"), "(Karl) Emrich|(Balduino) Rambo")
  expect_equal(fixName("F.daS.N.Thomé"), "F. da S.N. Thome")
  expect_equal(fixName("F. da S.N. Thomé"), "F. da S.N. Thome")
  expect_equal(fixName("Pedro L.R.de Moraes (30/4/1998)"), "Pedro L.R. de Moraes")
})

test_that("fixName separator work", {
  expect_equal(fixName("Karl Emrich & Balduino Rambo",
                       sep.out = ", "),# for now!!! this is not a good option
               "Karl Emrich, Balduino Rambo")
  #expect_equal(fixName("Karl Emrich & Balduino Rambo",
  #                     sep.out = ","), "Karl Emrich, Balduino Rambo")
})

test_that("fixName special character work", {
  expect_equal(fixName("F. da S.N. Thomé", special.char = TRUE), "F. da S.N. Thomé")
})

input <- c(NA, "", " ", "Alvares , Roberto",
           "A.Silva", "AlexS.", "Alex,Silva",
           "M. A. Costa, J. Ribeiro & E. C. Pereira",
           "M. A. Costa, et al.",
           "Det. by Alex Silva")
res <- c(NA, NA, NA, "Alvares, Roberto",
         "A. Silva", "Alex S.", "Alex, Silva",
         "M. A. Costa|J. Ribeiro|E. C. Pereira",
         "M. A. Costa",
         "Alex Silva")

test_that("fixName works", {

  expect_error(fixName())
  expect_error(fixName(1))
  expect_warning(fixName("A.  F.", sep.out = c(";", "|")))

  expect_equal(fixName(input), res)
})
