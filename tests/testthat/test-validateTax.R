# Creating a data frame with taxonomic information
df <- data.frame(
  family.new = c("Bignoniaceae", "Bignoniaceae","Bignoniaceae",
                 "Bignoniaceae","Bignoniaceae","Bignoniaceae"),
  identifiedBy.new = c("Gentry, A.H.", "Hatschbach, G.", NA, NA, NA, "Hatschbach, G."),
  recordedBy.new = c(NA, NA, NA, "Gentry, A.H.", NA, NA),
  typeStatus = c(NA, NA, "isotype", NA, NA, NA),
  numTombo = c("a_1","b_3","c_7","d_5","e_3","f_4"),
  basisOfRecord = c(rep("exsicata", 5),"obs"),
  stringsAsFactors = FALSE)

# Expected result
res0 <- c("high", "low", "high", "high", "unknown", "low")
res1 <- c("high", "medium", "high", "high", "unknown", "medium")
res2 <- c("high", "low", "high", "high", "unknown", "high")
res3 <- c("high", "medium", "high", "high", "unknown", "high")
res4 <- c("high", "high", "high", "high", "unknown", "high")
res5 <- c("high", "high", "high", "high", "unknown", "medium")
res6 <- c("high", "high", "high", "high", "unknown", "unknown")

# Tests
test_that("validateTax works", {
  expect_error(validateTax())
  expect_error(validateTax(data.frame()))
  expect_error(validateTax(df[,4]))
  expect_error(validateTax(df[,4, drop = FALSE]))
  expect_error(validateTax(df, generalist = TRUE, generalist.class = "toto"))

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



