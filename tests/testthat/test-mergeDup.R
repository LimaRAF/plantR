## ADD TESTS FOR 'geo' and 'loc'

## Creating some taxonomic information for merging
df <- data.frame(
  ID = c("a7","b2","c4","d1","e9","f3","g2","h8","i6","j5"),
  dup.ID = c("a7|b2","a7|b2","c4|d1|e9","c4|d1|e9","c4|d1|e9",
             "f3|g2","f3|g2","h8|i6|j5","h8|i6|j5","h8|i6|j5"),
  fam = c("AA","AA","BB","BB","Bb","CC","DD","EE","Ee","ee"),
  sp = c("a a","a b","c c","c d","c d","e e","f f","h h","h h","h h"),
  det = c("spec","n_spec","spec1","spec2","n_spec1",
          "spec3","spec4","n_spec2","n_spec3","n_spec4"),
  yr = c("2010","2019","2019","2000","2020",NA,"1812","2020","2020","2020"),
  check = c("high","low","high","high","low","high","high","low","low","low"),
  stat = rep("possibly_ok", 10),
  dup.prop = 1)

# Objects with the expected resolutions
df.out <- df[,3:8]
names(df.out) <- paste0(names(df.out),1)
df.out[2, c(1:5)] <- df.out[1, c(1:5)]
df.out[4, c(1:5)] <- df.out[3, c(1:5)]
df.out[5, c(1:5)] <- df.out[3, c(1:5)]
df.out[6, c(1:5)] <- df.out[7, c(1:5)]
df.out$ref.spec.tax <- c("a7","a7","c4","c4","c4",
                         "g2","g2","h8","h8","h8")
res0 <- cbind.data.frame(df, df.out)

# Tests
test_that("mergeDup works", {

  expect_error(mergeDup(TRUE))
  expect_error(mergeDup(data.frame(character())))
  expect_error(mergeDup(data.frame(xxxx = c("Aa bb", "Bb cc"))))


  expect_equal(mergeDup(df, info2merge = "tax",
        rec.ID = "ID",
        tax.names = c(family = "fam",
                      species = "sp",
                      det.name = "det",
                      det.year = "yr",
                      tax.check = "check",
                      status = "stat")), res0)
})

