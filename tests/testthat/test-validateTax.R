# Creating a data frame with taxonomic information
df <- data.frame(
  family.new = c("Bignoniaceae", "Bignoniaceae","Bignoniaceae",
                 "Bignoniaceae","Bignoniaceae","Bignoniaceae",
                 "Bignoniaceae","Bignoniaceae","Bignoniaceae"),
  identifiedBy.new = c("Gentry, A.H.", "Hatschbach, G.", NA, NA, NA,
                       "Hatschbach, G.", "Hatschbach, G.", "Hatschbach, G.",
                       "Hatschbach, G."),
  recordedBy.new = c(NA, NA, NA, "Gentry, A.H.", NA, NA, NA, NA, NA),
  typeStatus = c(NA, NA, "isotype", NA, NA, NA, NA, NA, NA),
  identifiedBy.aux = c(NA, NA, NA, NA, NA, NA, "Gentry, A.H.",
                       "Lima, R.A.F.; Gentry, A.H.", "Gentry, A.H. & Lima, R.A.F."),
  numTombo = c("a_1","b_3","c_7","d_5","e_3","f_4","h_8", "i_2", "j6"),
  basisOfRecord = c(rep("exsicata", 5),"obs", "exsicata", "exsicata", "exsicata"),
  stringsAsFactors = FALSE)

# Expected result
res0 <- c("high", "low", "high", "high", "unknown", "low", "high", "high", "high")
res1 <- c("high", "medium", "high", "high", "unknown", "medium", "high", "high", "high")
res2 <- c("high", "low", "high", "high", "unknown", "high", "high", "high", "high")
res3 <- c("high", "medium", "high", "high", "unknown", "high", "high", "high", "high")
res4 <- c("high", "high", "high", "high", "unknown", "high", "high", "high", "high")
res5 <- c("high", "high", "high", "high", "unknown", "medium", "high", "high", "high")
res6 <- c("high", "high", "high", "high", "unknown", "unknown", "high", "high", "high")
res7 <- c("high", "low", "high", "high", "unknown", "low", "low", "low", "low")
res8 <- c("high", "medium", "high", "high", "unknown", "medium", "medium", "medium", "medium")


# Tests
test_that("validateTax works", {
  expect_error(validateTax())
  expect_error(validateTax(data.frame()))
  expect_error(validateTax(df[,4]))
  expect_error(validateTax(df[,4, drop = FALSE]))
  expect_error(validateTax(df, generalist = TRUE, generalist.class = "toto"))
  expect_error(validateTax(df[,-2]))

  expect_equal(validateTax(df, print = FALSE)$tax.check, res0)
  expect_equal(validateTax(df, print = FALSE, generalist = TRUE)$tax.check, res1)
  expect_equal(validateTax(df, print = FALSE, voucher.list = "f_4")$tax.check, res2)
  expect_equal(validateTax(df, print = FALSE, generalist = TRUE, voucher.list = "f_4")$tax.check, res3)
  expect_equal(validateTax(df, miss.taxonomist = "Bignoniaceae_Hatschbach, G.")$tax.check, res4)
  expect_equal(validateTax(df, print = FALSE,
                           miss.taxonomist = "Bignoniaceae_Hatschbach, G.",
                           other.records = 1)$tax.check, res5)
  expect_equal(validateTax(df, print = FALSE,
                           miss.taxonomist = "Bignoniaceae_Hatschbach, G.",
                           other.records = "unknown")$tax.check, res6)
  expect_equal(validateTax(df, print = FALSE, aux.identifier = FALSE)$tax.check, res7)
  expect_equal(validateTax(df, print = FALSE, generalist = TRUE,
                           aux.identifier = FALSE)$tax.check, res8)

  #expect_equal(validateTax(df, print = TRUE)$tax.check, res0)
})

# For animals
sp_names=c("Actinopus anselmoi", "Murmidius drakei","Hydra iheringi",
           "Chrysops brevifascia","Forbesopsis sphingipennis",
           "Alphamenes campanulatus", "Aegla abrupta","Panthera onca", "Ateles marginatus")
class = c("Arachnida", "Insecta", "Hydrozoa", "Insecta", "Insecta", "Insecta", "Malacostraca", "Mammalia", "Mammalia")
order = c("Araneae", "Coleoptera", "Anthoathecata", "Diptera", "Lepidoptera", "Hymenoptera", "Decapode", "Carnivora", "Primates")
family = c("Actinopodidae", "Cerylonidae", "Hydridae", "Tabanidae", "Thyrididae", "Vespidae", "Aeglidae", "Felidae", "Atelidae")

iden_names = c("Santos, A. J.","Slipinski, A.","Cunha, A.","Henriques, A. L.","Santos, L. Q.","Hermes, M. G.","Santos, S.", "Pinto, C. E.", "Lima, R.")
df_tax <- data.frame(
  row_id = seq_along(sp_names),
  class = class,
  order = order,
  family.new = family,
  scientificName.new = sp_names,
  identifiedBy.new = plantR::prepName(iden_names),
  identifiedBy.aux = NA,
  recordedBy.new = "s.n.",
  typeStatus = NA,
  numTombo = NA,
  basisOfRecord = NA
)

res0 <- c("low", "high", "low", "high", "high", "high", "high", "low", "low")
res1 <- c("medium", "high", "medium", "high", "high", "high", "high", "low", "low")


test_that("validateTax works", {
  expect_equal(validateTax(as.data.frame(df_tax), kingdom = "animalia", print = FALSE)$tax.check,
               res0)
  expect_equal(validateTax(as.data.frame(df_tax), generalist = TRUE, kingdom = "animalia", print = FALSE)$tax.check,
               res1)
})



